#!/usr/bin/env stack
{- stack runghc --verbosity info
   --package hledger-lib
   --package hledger
-}

import Control.Monad
import Data.List
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Map as M
import Hledger
import Hledger.Cli

cmdmode :: Mode RawOpts
cmdmode = (defCommandMode ["check"])
    { modeHelp = "check for a various inconsistency situations"
    , modeGroupFlags = Group
        { groupNamed =
            [ ("Input",inputflags)
            , ("Misc",helpflags)
            ]
        , groupUnnamed = []
        , groupHidden = []
        }
    }

showCommodity :: Commodity -> String
showCommodity c
        | Just s <- cformat c = showAmount $ sample { astyle = s }
        | otherwise = showAmount sample
    where
        sample = amount
            { acommodity = csymbol c
            , aquantity = 1000
            }

showAmountStyle :: CommoditySymbol -> AmountStyle -> String
showAmountStyle symb = showCommodity . Commodity symb . Just

checkCommodity :: (T.Text, AmountStyle, AmountStyle, Either String Posting) -> IO ()
checkCommodity (symb, decl, infer, ctx) = do
    when (not $ infer `isSubStyle` decl) $ do
        putStrLn $ "Commodity format mismatch for " ++ T.unpack symb
        case map (showAmountStyle symb) [decl, infer] of
            [expect, actual]
                | expect /= actual -> do
                    putStrLn $ "\tExpect: " ++ showAmountStyle symb decl
                    putStrLn $ "\tActual: " ++ showAmountStyle symb infer
                | otherwise -> do -- more complicated difference
                    putStrLn $ "\tExpect: " ++ show decl
                    putStrLn $ "\tActual: " ++ show infer
            _ -> undefined
        case ctx of
            Right p -> do
                let p0 = originalPosting p
                putStrLn "In context of"
                putStr $ show p0
                case ptransaction p0 of
                    Just txn -> do
                        putStrLn $ "Within transaction at " ++ show (tsourcepos txn)
                        print $ txn
                    Nothing -> putStrLn ""
            Left s -> putStrLn $ "(" ++ s ++ ")"
        putStrLn ""

isSubStyle :: AmountStyle -> AmountStyle -> Bool
isSubStyle a b = basicOk && digitsOk && decimalsOk
    where
        stripNumberStyle x
            = x { asprecision = 0
                , asdecimalpoint = Nothing
                , asdigitgroups = Nothing
                }
        basicOk = stripNumberStyle a == stripNumberStyle b
        digitsOk = case map asdigitgroups [a, b] of
                [Nothing, _] -> True
                [_, Nothing] -> False
                [Just (DigitGroups ac ag), Just (DigitGroups bc bg)]
                    -> ac == bc && ag `isSuffixOf` bg
                _ -> undefined
        decimalsOk
            | Nothing <- asdecimalpoint a = asprecision a == 0
            | otherwise = eqOn asdecimalpoint a b && eqOn asprecision a b

eqOn :: Eq b => (a -> b) -> a -> a -> Bool
eqOn f a b = f a == f b

declaredStyles :: Journal -> M.Map CommoditySymbol AmountStyle
declaredStyles j = M.mapMaybe cformat (jcommodities j) `M.union` M.fromList (maybeToList $ jparsedefaultcommodity j)

main :: IO ()
main = do
    opts <- getHledgerCliOpts cmdmode
    withJournalDo opts $ \_ j -> do
        let commodities = declaredStyles j

        mapM_ checkCommodity $
            M.elems $ M.intersectionWithKey (\c d s -> (c, d, s, Left "inferred from journal")) commodities $ jinferredcommodities j

        let checkCommodityPostings ps = mapM_ checkCommodity
                [(c, d, s, Right p)
                | p <- ps
                , let p0 = originalPosting p
                , a <- amounts $ pamount p0
                , let c = acommodity a
                , let s = astyle a
                , Just d <- [M.lookup c commodities]
                ]

        checkCommodityPostings [p | t <- jtxns j, p <- tpostings t]
        checkCommodityPostings [p | mt <- jmodifiertxns j, p <- mtpostings mt]
        checkCommodityPostings [p | pt <- jperiodictxns j, p <- ptpostings pt]

#!/usr/bin/env stack
{- stack runghc --verbosity info
   --package hledger-lib
   --package hledger
-}

import Control.Monad
import Data.List
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

checkCommodity :: (T.Text, AmountStyle, AmountStyle, Maybe Posting) -> IO ()
checkCommodity (symb, decl, infer, ctx) = do
    when (not $ infer `isSubStyle` decl) $ do
        putStrLn $ "Commodity format mismatch for " ++ T.unpack symb
        case map (showAmountStyle symb) [decl, infer] of
            [expect, act]
                | expect /= act -> do
                    putStrLn $ "\tExpect: " ++ showAmountStyle symb decl
                    putStrLn $ "\tActual: " ++ showAmountStyle symb infer
                | otherwise -> do -- more complicated difference
                    putStrLn $ "\tExpect: " ++ show decl
                    putStrLn $ "\tActual: " ++ show infer
            _ -> error "Impossible"
        case ctx of
            Just p -> do
                putStrLn "In context of"
                putStr $ show p
                case ptransaction p of
                    Just txn -> do
                        putStrLn $ "Within transaction at " ++ show (tsourcepos txn)
                        print $ originalTransaction txn
                        print $ txn
                    Nothing -> putStrLn ""
            Nothing -> putStrLn ""
    return ()

isSubStyle :: AmountStyle -> AmountStyle -> Bool
isSubStyle a b = basicOk && digitsOk && decimalsOk
    where
        stripSome x = x { asprecision = 0
                        , asdecimalpoint = Nothing
                        , asdigitgroups = Nothing
                        }
        basicOk = stripSome a == stripSome b
        digitsOk = case map asdigitgroups [a, b] of
                [Nothing, _] -> True
                [_, Nothing] -> False
                [Just (DigitGroups ac ag), Just (DigitGroups bc bg)]
                    -> ac == bc && ag `isSuffixOf` bg
                _ -> error "Impossible"
        decimalsOk
            | Nothing <- asdecimalpoint a = asprecision a == 0
            | otherwise = eqOn asdecimalpoint a b && eqOn asprecision a b

        -- reduceMap g f x y = g (f x) (f y)
        eqOn f x y = f x == f y

main :: IO ()
main = do
    opts <- getHledgerCliOpts cmdmode
    withJournalDo opts $ \_ j -> do
        let commodities = M.mapMaybe cformat $ jcommodities j
        mapM_ checkCommodity $
            M.elems $ M.intersectionWithKey (\c d s -> (c, d, s, Nothing)) commodities $ jinferredcommodities j
        let checkCommodityPostings ps = mapM_ checkCommodity
                [(c, d, s, Just p)
                | p <- ps
                , a <- amounts $ pamount p
                , let c = acommodity a
                , let s = astyle a
                , Just d <- [M.lookup c commodities]
                ]

        checkCommodityPostings [p | t <- jtxns j, p <- tpostings t]
        checkCommodityPostings [p | mt <- jmodifiertxns j, p <- mtpostings mt]
        checkCommodityPostings [p | pt <- jperiodictxns j, p <- ptpostings pt]
        return ()

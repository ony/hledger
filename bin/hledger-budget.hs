#!/usr/bin/env stack
{- stack runghc --verbosity info
  --package hledger-lib
  --package hledger
  --package cmdargs
  --package text
  --package optparse-applicative
-}
{-# LANGUAGE OverloadedStrings, LambdaCase, TupleSections #-}
{-

hledger-budget REPORT-COMMAND [--no-offset] [--no-buckets] [OPTIONS...]

Perform some subset of reports available in core hledger but process automated
and periodic transactions. Also simplify tree of accounts to ease view of
"budget buckets".

For people familiar with [`ledger`
budgeting](http://www.ledger-cli.org/3.0/doc/ledger3.html#Budgeting) may
consider this tool as an alias to `ledger --budget`.

With this tool you may either use so called periodic transactions that being
issued with each new period or use a family of approaches with automated
transactions. You may want to look at [budgeting section of
plaintextaccounting](http://plaintextaccounting.org/#budgeting).

Periodic transaction that being interpreted by this tool may look like:

```ledger
~ monthly from 2017/3
    income:salary  $-4,000.00
    expenses:taxes  $1,000
    expenses:housing:rent  $1,200
    expenses:grocery  $400
    expenses:leisure  $200
    expenses:health  $200
    expenses  $100
    assets:savings
```

Header of such entries starts with `'~'` (tilde symbol) following by an
interval with an effect period when transactions should be injected.

Effect of declaring such periodic transaction is:

- Transactions will be injected at the beginning of each period. I.e. for
  monthly it will always refer to 1st day of month.
- Injected transaction will have inverted amounts to offset existing associated
  expenses. I.e. for this example negative balance indicates how much you have
  within your budget and positive amounts indicates how far you off from your
  budget.
- Set of accounts across of all periodic transactions will form kinda buckets
  where rest of the accounts will be sorted into. Each account not mentioned in
  any of periodic transaction will be dropped without changing of balance for
  parent account. I.e. for this example postings for `expenses:leisure:movie`
  will contribute to the  balance of `expenses:leisure` only in reports.

Note that beside a periodic transaction all automated transactions will be
handled in a similar way how they are handled in `rewrite` command.

#### Bucketing
It is very common to have more expense accounts than budget
"envelopes"/"buckets". For this reason all periodic transactions are treated as
a source of information about your budget "buckets".

I.e. example from previous section will build a sub-tree of accounts that look like

```
assets:savings
expenses
  taxes
  housing:rent
  grocery
  leisure
  health
income:salary
```

All accounts used in your transactions journal files will be classified
according to that tree to contribute to an appropriate bucket of budget.

Everything else will be collected under virtual account `<unbucketed>` to give
you an idea of what parts of your accounts tree is not budgeted. For example
`liabilities` will contributed to that entry.

#### Reports
You can use `budget` command to produce next reports:

- `balance` - the most important one to track how you follow your budget. If
  you use month-based budgeting you may want to use `--monthly` and
  `--row-total` option to see how you are doing through the months. You also
  may find it useful to add `--tree` option to see aggregated totals per
  intermediate node of accounts tree.
- `register` - might be useful if you want to see long history (ex. `--weekly`)
  that is too wide to fit into your terminal.
- `print` - this is mostly to check what actually happens. But you may use it
  if you prefer to generate budget transactions and store it in a separate
  journal for some less popular budgeting scheme.

#### Extra options for reports
You may tweak behavior of this command with additional options `--no-offset` and `--no-bucketing`.

- Don't use these options if your budgeting schema includes both periodic
  transactions, and "bucketing". Unless you want to figure out how your
  budgeting might look like. You may find helpful values of average column from
  report

```shell
$ hledger budget -- bal --period 'monthly to last month' --no-offset --average
```

- Use `--no-offset` and `--no-bucketing` if your schema fully relies on
  automated transactions and hand-crafted budgeting transactions. In this mode
  only automated transactions will be processed. I.e. when you journal looks
  something like

```ledger
= ^expenses:food
  budget:gifts  *-1
  assets:budget  *1

2017/1/1 Budget for Jan
  assets:bank  $-1000
  budget:gifts  $200
  budget:misc
```

- Use `--no-bucketing` only if you want to produce a valid journal. For example
  when you want to pass it as an input for other `hledger` command. Most people
  will find this useless.

#### Recommendations
- Automated transaction should follow same rules that usual transactions follow
  (i.e. keep balance for real and balanced virtual postings).
- Don't change the balance of real asset and liability accounts for which you
  usually put assertions. Keep in mind that `hledger` do not apply modification
  transactions.
- In periodic transactions to offset your budget use either top-level account
  like `Assets` or introduce a "virtual" one like `Assets:Bank:Budget` that
  will be a child to the one you want to offset.

-}
import Data.Maybe
import Data.List
import Data.Monoid ((<>))
import System.Console.CmdArgs hiding (help)
import Hledger.Cli
import Options.Applicative hiding (action)
import Options.Applicative.Builder.Internal (HasName)
import Hledger.Data.AutoTransaction

-- | Build a 'ParserInfo' based on cmdargs mode
--
-- Note that sub modes are not handled
modeAsParser :: Mode a -> (Parser (Either String a), InfoMod (Either String a))
modeAsParser mode0 | not . null . fromGroup $ modeGroupModes mode0 = error "No support for sub modes"
modeAsParser mode0 = (parser, infoMod) where
    seedValue = modeValue mode0
    infoMod = progDesc (modeHelp mode0)
    parser = foldr (=<<) (Right seedValue) . concat <$> sequenceA (flagParsers ++ argParsers ++ [tailArgParser])
    flagParsers = map ((maybeToList <$>) . optional . flagAsParser) . fromGroup $ modeGroupFlags mode0
    argParsers = map (fmap (:[]) . argAsParser) . fst $ modeArgs mode0
    tailArgParser = case snd $ modeArgs mode0 of
        Nothing -> pure []
        Just arg0
            | argRequire arg0 -> some (argAsParser arg0)
            | otherwise -> many (argAsParser $ arg0 { argRequire = True })

-- | Represent flag from cmdargs package in a form of optparse-applicative
flagAsParser :: Flag a -> Parser (a -> Either String a)
flagAsParser flag0 = updParser where
    upd = flagValue flag0
    updParser = case flagInfo flag0 of
        FlagNone -> parserNone (error "Shouldn't reference argument")
        FlagReq -> parserReq
        FlagOpt arg -> parserNone arg
        FlagOptRare arg -> parserNone arg
    parserNone arg = flag Right (upd arg) (helpInfo <> nameInfo)
    parserReq = upd <$> strOption (helpInfo <> nameInfo <> typeInfo)
    helpInfo = flagHelpAsInfoMod $ flagHelp flag0
    flagHelpAsInfoMod = \case
        "" -> idm
        x -> help x
    nameInfo :: HasName f => Mod f a
    nameInfo = mconcat . map flagNameAsInfoMod $ flagNames flag0
    flagNameAsInfoMod :: HasName f => String -> Mod f a
    flagNameAsInfoMod = \case
        [c] -> short c
        cs -> long cs
    typeInfo = flagTypeAsInfoMod $ flagType flag0
    flagTypeAsInfoMod = \case
        "" -> idm
        x -> metavar x

-- | Represent positional argument from cmdargs package in a form of optparse-applicative
argAsParser :: Arg a -> Parser (a -> Either String a)
argAsParser arg0 = updParser' where
    updParser = upd <$> strArgument modType
    updParser'
        | argRequire arg0 = updParser
        | otherwise = fromMaybe Right <$> optional updParser
    upd = argValue arg0
    modType = case argType arg0 of
        [] -> idm
        n -> metavar n

actions :: [(Mode RawOpts, (BudgetOpts, CliOpts) -> IO ())]
actions =
    [ (manmode, man . snd)
    , (infomode, info' . snd)
    , (balancemode, flip withJournalDo' balance)
    , (balancesheetmode, flip withJournalDo' balancesheet)
    , (cashflowmode, flip withJournalDo' cashflow)
    , (incomestatementmode, flip withJournalDo' incomestatement)
    , (registermode, flip withJournalDo' register)
    , (printmode, flip withJournalDo' print')
    ]

commands :: Parser (Either String RawOpts, (BudgetOpts, CliOpts) -> IO ())
commands = subparser . mconcat . concat $ map f actions where
    f (mode0, io) = cmd cmdName : map alias cmdAliases where
        (parser, infoMod) = modeAsParser mode0
        mainInfo = info (parser <**> helper) infoMod
        -- https://github.com/pcapriotti/optparse-applicative/issues/113#issuecomment-69594575
        aliasInfo = info (parser <**> helper) (progDesc $ "alias for " ++ cmdName)
        (cmdName:cmdAliases) = modeNames mode0
        cmd n = command n $ (,io) <$> mainInfo
        alias n = command n $ (,io) <$> aliasInfo


data BudgetOpts
        = BudgetOpts
          { budgetOptBuckets :: Bool
          , budgetOptOffset :: Bool
          }
    deriving (Show)

budgetOpts :: Parser BudgetOpts
budgetOpts = BudgetOpts
    <$> nswitch (long "no-buckets"
                <> help "show all accounts besides mentioned in periodic transactions")
    <*> nswitch (long "no-offset"
                <> help "do not add up periodic transactions")

nswitch :: Mod FlagFields Bool -> Parser Bool
nswitch = flag True False

journalBalanceTransactions' :: CliOpts -> Journal -> IO Journal
journalBalanceTransactions' opts j = do
    let assrt = not $ ignore_assertions_ opts
    either error' return $ journalBalanceTransactions assrt j

withJournalDo' :: (BudgetOpts, CliOpts) -> (CliOpts -> Journal -> IO ()) -> IO ()
withJournalDo' (bopts, opts) = withJournalDo opts . wrapper where
    wrapper f opts' j = do
        -- use original transactions as input for journalBalanceTransactions to re-infer balances/prices
        let modifier = originalTransaction . foldr (flip (.) . runModifierTransaction') id mtxns
            runModifierTransaction' = fmap txnTieKnot . runModifierTransaction Any
            mtxns = jmodifiertxns j
            dates = jdatespan j
            ts' = map modifier $ jtxns j
            ts'' | not $ budgetOptOffset bopts = ts'
                 | otherwise= [makeBudget t | pt <- jperiodictxns j, t <- runPeriodicTransaction pt dates] ++ ts'
            makeBudget t = txnTieKnot $ t
                { tdescription = "Budget transaction"
                , tpostings = map makeBudgetPosting $ tpostings t
                }
            makeBudgetPosting p = p { pamount = negate $ pamount p }
        j' <- journalBalanceTransactions' opts' j{ jtxns = ts'' }

        -- re-map account names into buckets from periodic transaction
        let buckets = budgetBuckets j
            remapAccount "" = "<unbucketed>"
            remapAccount an
                | an `elem` buckets = an
                | otherwise = remapAccount (parentAccountName an)
            remapPosting p = p { paccount = remapAccount $ paccount p, porigin = Just . fromMaybe p $ porigin p }
            remapTxn = mapPostings (map remapPosting)
        let j'' | not $ budgetOptBuckets bopts = j'
                | null buckets = j'
                | otherwise = j' { jtxns = remapTxn <$> jtxns j' }

        -- finally feed to real command
        f opts' j''

budgetBuckets :: Journal -> [AccountName]
budgetBuckets = nub . map paccount . concatMap ptpostings . jperiodictxns

mapPostings :: ([Posting] -> [Posting]) -> (Transaction -> Transaction)
mapPostings f t = txnTieKnot $ t { tpostings = f $ tpostings t }

main :: IO ()
main = do
    let mainParseInfo = info ((,) <$> budgetOpts <*> commands <**> helper) infoMod
        infoMod = progDesc "run some basic reports in budget mode"
        p = prefs disambiguate
    (bopts, erawopts) <- customExecParser p mainParseInfo
    case erawopts of
        (Left msg, _) -> putStrLn $ "Error: " ++ msg
        (Right rawopts, io) -> do
            let rawopts' = decodeRawOpts rawopts
            opts <- rawOptsToCliOpts rawopts'
            io (bopts, opts)

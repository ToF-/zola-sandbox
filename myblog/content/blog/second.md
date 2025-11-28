+++
title = "My second post"
date = 2025-11-28
+++

Haskell makes it possible to write statically typed, purely functional programs. This gives us two interesting conveniences. The first one is that any incoherence in the type of our expressions and sequences can be spotted at compile time. The second one is that we can rule out the direct use of partial functions by wrapping their results in new data types and compose expressions with values of these types.

<!--more-->

<img src="/images/design.png" alt="combo" style="height:256px">

However, when a program has to process IO (as any useful program will), static typing and purity might seem to get in the way for a beginner. We use the `IO` Monad to get `IO` values, and the `Either` Monad to deal with failures, but combining the two makes our programs cumbersome.

Monad Transformers can help us write simpler programs, by hiding the boilerplate code that this combination requires.

In this blog post I present how to chain monadic actions and controls with the `ExceptT` Monad Transformer in Haskell through a small example, in 4 steps:
* writing a naïve implementation, which halts on IO exception
* improving its robustness with conditionals and pattern matching
* combining `IO` and `Either` using `ExceptT`
* refactoring the program's chaining of actions

## Program #1: A naïve solution

Let's say we want to write a program that reads a CSV file containing transactions, which are composed of a category and an amount, and prints the total spent for each category.

For instance, given a file `transactions.csv` containing this data:
```
Groceries, 100.00
Savings, 500.00
Equipment, 32.00
Groceries, 42.00
Insurance, 38.17
Groceries, 30.00
Equipment, 179.00
```
the command `summary transactions.csv` will output this:
```
Equipment, 211.0
Groceries, 172.0
Insurance, 38.17
Savings, 500.0
```

Our program will

* obtain the name of a file from the command line,
* read this file, splitting each line into *category* and *amount*, so as to create *transactions*,
* sort and group these *transactions* by *category*,
* sum these groups into *summary lines*,
* and finally print these lines.

After importing some standard functions, we define adequate data types for our program, starting with `Category`.
```haskell
import System.Environment ( getArgs )
import Data.List          ( groupBy
                          , sortBy  )
import Data.Function      ( on )

data Category = Category { categoryLabel :: String }
    deriving (Eq, Show)
```
### Reading Categories
We need a way to `read` a `Category` from a `String`, so let's make this type an instance of the `Read` class. Parsing a category label amounts to reading alphanumeric chars, possibly some spaces, and rejecting everything else. For instance `"Credit Cards Payments"` and `"Credit 1"` can be used as a category label, while `"Savings & Investing"` cannot.

`readsPrec` is the function that we need to implement. It has the signature
```haskell
Int -> String -> [(a,String)]
```
where the first argument is the precedence level (which we don't need to specify for our simple program), the second argument is the `String` to be parsed, and the result is a list of possible results. Returning an empty list means that the input string could not be parsed to a value of type `a`.

```haskell
instance Read Category where
    readsPrec _ s = if not (null label)
                       then return (Category label, rest)
                       else []
        where
        label = takeWhile (isLegal) s
        rest  = drop (length label) s
        isLegal c = isAlphaNum c || c == ' '
```
The function takes all the legal characters in the input string `s`, and returns a `Category` value, coupled with the part of the input that remains to be parsed. Or it returns an empty list if no legal character was found at the beginning of the input string.

Let's try to `read` a `Category` using *ghci*:
```haskell
$ ghci
> import Program1.hs
> read "Foo" :: Category
Category "Foo"

> read "*$!" :: Category
*** Exception: Prelude.read: no parse

(reads :: ReadS Category) "Bar, 42"
[(Category "Bar",",42")]
```
### Reading Transactions
A `Transaction` is composed with a `Category` and a `Double`. Since we want to `read` transactions, we need to implement `readsPrec` for this type as well.
```haskell
data Transaction = Transaction { transactionCategory :: Category
                               , transactionAmount   :: Double }
    deriving (Eq,Ord,Show)

instance Read Transaction where
    readsPrec _ line = do
        (categ,  rest1) <- reads line
        (_,      rest2) <- readComma rest1
        (number, rest3) <- reads rest2
        return $ (Transaction categ number, rest3)
            where
            readComma :: ReadS String
            readComma s = case lex s of
                            ((",",r):_) -> return (",",r)
                            _           -> []
```

This `readsPrec` is a bit more complicated than the first one: it is chaining computations on the list monad, reading first a `Category`, then a comma (and discarding it), then a `Double` value. Chaining these three parsers ensures that the evaluation will result in an empty list as soon as one of them returns an empty list.


☞ *To illustrate the effect of failure in a chain of list actions try this expression in ghci:*
```haskell
[1,2,3] >>= \n -> [n,n*10,n*100] >>= \m -> [m*m,m*m*m]
```
*then try it again, replacing any of the three lists by the empty list.*

Trying our parser on *ghci*:
```haskell
$ ghci
> read "Foo, 42" :: Transaction
Transaction {transactionCategory = Category "Foo", transactionAmount = 42.0}

> read ", 42" :: Transaction
*** Exception: Prelude.read: no parse

> read "Bar, i42" :: Transaction
*** Exception: Prelude.read: no parse
>
```
### Computing Summary Lines
Now for the summary: since a *summary line* has the exact same structure as a *transaction*, we choose to define it as a type synonym. Also we should be able to `display` summary lines.

```haskell
type SummaryLine = Transaction

display :: SummaryLine -> String
display t = categoryLabel (transactionCategory t)
           ++ ", " ++ show (transactionAmount t)
```

To *summarize* the transactions is to sort and group them by category, then for each group, create a `SummaryLine` with the category and total amount of the group:
```haskell
type SummaryLine = Transaction

summarize :: [Transaction] -> [SummaryLine]
summarize = map summary
          . groupBy ( (==)    `on` transactionCategory )
          . sortBy  ( compare `on` transactionCategory )
    where
    summary :: [Transaction] -> SummaryLine
    summary txs = Transaction (category txs) (total txs)
        where
        category = transactionCategory . head
        total    = sum . map transactionAmount
```
Note how `on` helps expressing the logic by composing the functions that are required by `sortBy` and `groupBy`.
Since
```haskell
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
```
then
```haskell
on compare :: (Ord b) => (a -> b) -> a -> a -> Ordering
```
and thus
```haskell
on compare transactionCategory :: Transaction -> Transaction -> Ordering
```
which is conform to the type of function required by `sortBy`. Similarly, `on` composed with `(==)` will create a function of the type required by `groupBy`.

Reporting summary lines is done by mapping our `display` function for each line, and then merging this list of `String`s into one single `String`:
```haskell
report :: [SummaryLine] -> String
report = unlines . map display
```
### The main program
We can now write our main function, which will get a file name on the command line, read that file, convert its content into a list of `Transaction`s, and then compute and print the summary.
```haskell
program1 :: IO ()
program1 = do
    args    <- getArgs
    content <- readFile (head args)
    let transactions = map read $ lines content
    putStrLn $ report $ summarize transactions
```
And voilà, we have our program:
```
$ ghc --make program1.hs
$ program1 transactions.csv
Equipment, 211.0
Groceries, 172.0
Insurance, 38.17
Savings, 500.0
```

It is, indeed, a very naïve program. Let's see what could go wrong:

* we could forget to specify a file name when lauching the program from the command line
* we could specify a file name that doesn't correspond to an existing file
* the file could contain data that can't be read as comma separated transaction values
* the file could be empty, in which case nothing would be output

```
$ program1
program1: Prelude.head: empty list

$ program1 foo
program1: foo: openFile: does not exist (No such file or directory)

$ program1 wrong.csv
program1: Prelude.read: no parse

$ program1 empty.csv

```
None of these conditions is adequately managed. This means that given certain inputs some of the functions will not return a value and the program will halt. Let's change this.

## Program #2: responding to failure conditions
### Partial and total functions

We want to deal with failure conditions in a graceful way. Our program should not stop abruptly with a strange message like *"empty list"* or *"no parse"*. Instead it should print a clear diagnostic and possibly propose a way for the user to remedy the problem.

What parts of the program should change? Well, every part where the program calls a function that is not *total*. A function is said to be total if it returns a value for each possible value of its argument.

The function `head`, used in the expression `content <- readFile (head args)` is not total and could halt the program with an *"empty list"* message.
The function `read` is also partial: it will halt the program if the string it is supposed to convert into a value is not correct.

On the other hand, the function:
```haskell
lookup :: Eq a => a -> [(a, b)] -> Maybe b
```
*is* an example of a total function. It will not interrupt the program, for all possible values of type `a` and `b`.

☞ *`head` is used inside the function `summary` in the expression `transactionCategory . head`. Does it constitute a risk of halting the program in case we apply it on an empty list? Why?*
### A data type to represent failure
If a function is not total, one safe way to use it is to combine it with a data type that can represent failure. The `Either` type constructor is designed just for such representations, and we will use it. To make things a bit clearer, let's first define a type synonym for the `String` used as messages.
```haskell
type Message = String
```
Our most frequent concern will be about the CSV file data format, so let's create a reader function that will manage faulty data in a graceful way:
```haskell
readTransaction :: String -> Either Message Transaction
readTransaction s =
    case reads s of
      []        -> Left $ "Error: incorrect CSV format : " ++ s
      ((t,_):_) -> Right t
```
This reader calls the `reads` function (which in turn calls the `readSprec` that we defined earlier) and wraps the result into an `Either` context.

Parsing *several* transactions from a `String` is a matter of applying `readTransaction` to each line of the argument. This is done with `mapM`:
```haskell
mapM :: (Traversable t, Monad m) => (a -> m b) -> t a -> m (t b)
```
The function is used to chain a monadic action to the elements of a structure and return a single monadic value. Here, it transposes a `[Either Message Transaction]` into an `Either Message [Transaction]`.
```haskell
readTransactions :: String -> Either Message [Transaction]
readTransactions = mapM readTransaction . lines
```

Another failure possibility resides in getting the first argument on the command line: what if there is none? Let's wrap `getArgs :: IO [String]` into a `IO Either Message FilePath` to be on the safe side:
```haskell
getFileNameArg :: IO (Either Message FilePath)
getFileNameArg = do
    args <- getArgs
    return $ if null args
                    then Left "Error: no file name given"
                    else Right (args !! 0)
```
### Dealing with IO Exceptions
What if the CSV file can't be open? Using `Control.Exception` will help us dealing with such a situation:
```haskell
getFileContent :: FilePath -> IO (Either Message String)
getFileContent fp = (fmap Right $ readFile fp) `catch` handle
    where
    handle :: IOException -> IO (Either Message String)
    handle = return . Left . ("Error: " ++) . show
```
The function:
```haskell
catch :: IOException e => IO a -> (e -> IO a) -> IO a
```
is our "graceful exit" instrument here: given an IO action and a handler function, it will catch any IO exception and apply the handler to it, which will yield a legit `IO a` value.

In our case, what we want to do with the exception is:
* `show` it into a `String` starting with `"Error: "`
* make this message a `Left` value
* `return` this left value, making it an `IO (Either Message String)`

In the case when everything is fine with the file and no exception is triggered, `readFile` will give us an `IO String` value. We make that value an `Either Message String` by mapping the `Right` constructor to it.

Note that our function returns an `IO (Either Message String)` value. Why not simply return a `Either Message String` instead? Because there is no safe way to convert an IO value into a non-IO value. Any function dealing with IO is partial, not total, because IO actions are always prone to some failure condition. If we could compile a function with signature `IO a -> a` then Haskell's type checker would be much less useful to detect problems in our constructions when dealing IOs, and we would be hiding to ourselves some crucial concern with our program reliability.

`IO` is an inescapable context. However, the fact that `getFileContent` returns an `IO` value should not be a problem because it will be used within the context of an IO action and nowhere else.

☞ *There is actually a function with type `IO a -> a`. It's called `unsafePerformIO`. Use it at your own risk*

Here's the version 2 of the program. It will examine the values returned by `get`... functions and branch accordingly instead of halting:

```haskell
program2 :: IO ()
program2 = do
    fileName <- getFileNameArg
    case fileName of
        Left msg -> putStrLn msg
        Right fp -> do
            content <- getFileContent fp
            case (content >>= readTransactions) of
                Left msg -> putStrLn msg
                Right []  -> putStrLn $ "error: no transactions"
                Right txs -> putStrLn $ report $ summarize txs

main :: IO ()
main = program2
```
This program is dealing with failures in a better way:
```
$ ghc --make program2.hs

$ program2 transactions.csv
Equipment, 179.0
Groceries, 172.0
Insurance, 38.17
Investment, 6007.0
Savings, 500.0

$ program2
Error: no file name given

$ program2 foo
Error: foo: openFile: does not exist (No such file or directory)

$ echo "foo,bar" >wrong.csv
$ program2 wrong.csv
Error: incorrect CSV format : Foo, bar

$ touch empty.csv
$ program2 empty.csv
Error: no transactions

```
## 3. Monadic actions as isolated contexts
The bind operator (`>>=`) used in the `case .. of` instruction:
```haskell
            ...
            content <- getfilecontent fp
            case (content >>= readtransactions) of
                left msg -> putstrln msg
                right []  -> putstrln $ "error: no transactions"
                right txs -> putstrln $ unlines $ map show $ summarize txs
```
can be chained with as many functions of the type `a -> Either Message b` as we want over the initial value of `content`. For instance we could add new controls to detect an empty transaction list or to check that no transaction in the list has amount of zero.
```haskell

checkNotEmpty :: [Transaction] -> Either Message [Transaction]
checkNotEmpty []  = Left "Error: no transactions"
checkNotEmpty txs = Right txs

checkNonZero :: Transaction -> Either Message Transaction
checkNonZero (Transaction _ 0)
    = Left "Error: amount equal to zero"

program2 :: IO ()
program2 = do
    fileName <- getFileNameArg
    case fileName of
        Left msg -> putStrLn msg
        Right fp -> do
            content <- getFileContent fp
            case (content >>= readTransactions
                          >>= checkNotEmpty
                          >>= mapM checkNonZero) of
                Left msg -> putStrLn msg
                Right txs -> putStrLn $ unlines $ map show $ summarize txs
```
This chaining of controls could give the impression that we've missed an opportunity to simplify the code of the whole function, which we could have done by equally chaining the values obtained by `getFileNameArg` and `getFileContent`. Instead we used explicit `case ... of` branching.

Question: Could it be possible to chain *all* our `Either Message a` functions like this ?

```haskell
wrong_program2 :: IO () -- won't compile
wrong_program2 = do
    case (getFileNameArg >>= getFileContent
                         >>= readTransactions
                         >>= checkNotEmpty
                         >>= mapM checkNonZero) of
               Left msg -> putStrLn msg
               Right txs -> putStrLn $ unlines $ map show $ summarize txs
```
Answer: No. The compiler has no less than 6 complaints about this change to the function. Here's the first one:

```
    • Couldn't match type ‘Either Message String’ with ‘[Char]’
      Expected type: Either Message String -> IO (Either Message String)
        Actual type: FilePath -> IO (Either Message String)
        case (getFileNameArg >>= getFileContent
```
In essence: we cannot chain monadic actions from the `IO` monad to the `Either` monad, and vice versa. Since the `case ... of` is examining a value of type `Either Message [Transaction]`, the expected type for actions leading to that value is `a -> Either Message b`. But we are trying to somehow get to that value through actions of type `a -> IO b`. That can't work.

We can always bind monadic actions to distinct types through the **same monad**, as these examples with `Maybe` and `IO` show:
```haskell
ghci
> notNull l = if null l then Nothing else Just l
> notLong l = if length l > 10 then Nothing else Just l

> Just "foo" >>= notNull >>= notLong
Just "foo"

> Just "" >>= notNull >>= notLong
Nothing

> Just "this is too long" >>= notNull >>= notLong
Nothing

> getLine >>= putStrLn
foo
foo
```
but we can never bind monadic actions through different monadic types:
```
> getLine >>= notNull

<interactive>:16:13: error:
    • Couldn't match type ‘Maybe’ with ‘IO’
      Expected type: String -> IO [Char]
        Actual type: [Char] -> Maybe [Char]
    • In the second argument of ‘(>>=)’, namely ‘notNull’
      In the expression: getLine >>= notNull
      In an equation for ‘it’: it = getLine >>= notNull

> Just "foo" >>= putStrLn

<interactive>:17:16: error:
    • Couldn't match type ‘IO’ with ‘Maybe’
      Expected type: [Char] -> Maybe ()
        Actual type: String -> IO ()
    • In the second argument of ‘(>>=)’, namely ‘putStrLn’
      In the expression: Just "foo" >>= putStrLn
      In an equation for ‘it’: it = Just "foo" >>= putStrLn
>
```
We can always, as in `program2` chain `Either` actions *inside* expressions that are themselves produced inside `IO`. But we cannot produce one single, simplified chaining as in *please, chain all these actions and controls over my input data and signal any failure*.

Where do we go from here?

## 4. Combining Monads with Monad Transformers

What we need in order to simplify the code that does all the controls and actions is the ability to chain, approximately speaking, `Either` actions *inside* the `IO` monad.

This is what the [`Control.Monad.Trans.Except`](https://hackage.haskell.org/package/transformers-0.5.6.2/docs/Control-Monad-Trans-Except.html) library offers:

> __Control.Monad.Trans.Except__
>
> This monad transformer extends a monad with the ability to throw exceptions.
>
> A sequence of actions terminates normally, producing a value, only if none of the actions in the sequence throws an exception. If one throws an exception, the rest of the sequence is skipped and the composite action exits with that exception.
>
> ```haskell
> newtype ExceptT e m a
> ```
> A monad transformer that adds exceptions to other monads.
>
> `ExceptT` constructs a monad parameterized over two things:
>
> - *e* - The exception type.
> - *m* - The inner monad.
>
> The `return` function yields a computation that produces the given value, while `>>=` sequences two subcomputations, exiting on the first exception.
>
> ```haskell
> throwE :: Monad m => e -> ExceptT e m a
> ```
>
> Signal an exception value *e*.
>
> ```haskell
> runExceptT (throwE e) = return (Left e)
> throwE e >>= m = throwE e
> ```

### Pure values and input values
Let's experiment on *ghci*. We start with importing our program, and the module.

```haskell
> import Program2.hs
> import Control.Monad.Trans.Except
```

Let's try to create a pure `ExecptT Message IO [Transaction]` value using the `ExceptT` constructor:
```haskell
> value = ExceptT $ return $ Right $ [Transaction (Category "Groceries") 42.0]

> :type value
value :: Monad m => ExceptT e m [Transaction]
```

Now let's create a function to get a list of transactions from a file. That amounts to:
* reading the file and putting its content in an `IO String`
* applying `readTransactions` to this value, which gets us a `IO (Either Message String)`
* wrapping this into an `ExceptT` value

```haskell
> fromFile = ExceptT . fmap readTransactions . readFile

> :type fromFile
fromFile :: FilePath -> ExceptT Message IO [Transaction]
```
This seems promising: we get the same result type whether our value comes from a constant or from reading a file!

Extracting the value from an `ExceptT` context is done via `runExceptT`:
```haskell
> runExceptT value
Right [Transaction {transactionCategory = Category {categoryLabel = "Groceries"}, transactionAmount = 42.0}]

> runExceptT $ fromFile "transactions.csv"
Right [Transaction {transactionCategory = Category {categoryLabel = "Groceries"}, transactionAmount = 100.0}
. . .
,Transaction {transactionCategory = Category {categoryLabel = "Equipment"}, transactionAmount = 179.0}]
```
### Exceptions and interactions
Naturally our function doesn't handle exceptions yet:
```haskell
> runExceptT $ fromFile "foo"
*** Exception: foo: openFile: does not exist (No such file or directory)
```
If we want it to return a `Left` value, we have to provide a handler:
```haskell
> handler = return . Left . show :: (IOException -> IO (Either Message [Transaction]))
> fromFile fp = ExceptT $ fmap readTransactions (readFile fp) `catch` handler

> runExceptT $ fromFile "foo"
Left "foo: openFile: does not exist (No such file or directory)"
```
And now our function handles exceptions correctly.

Another case where we want to have `IO` and `Either` working together seamlessly is about extracting the CSV file name from the arguments provided on the command line, returning a `Left` if no argument was given. Could we instead prompt the user for a file name?

Let's write a prompt function:
```haskell
> promptForFileName = putStrLn "please enter a file name:" >> getLine
```
when the empty list pattern is met, we prompt for a file name, in other cases, we extract the first argument from the list
```haskell
> :{
| getFileName :: [String] -> ExceptT Message IO FilePath
| getFileName []      = ExceptT $ fmap Right $ prompt
| getFileName (arg:_) = ExceptT $ return $ Right arg
| :}

> runExceptT $ getFileName ["transactions.csv"]
Right "transactions.csv"

> runExceptT $ getFileName []
please enter a file name:
transactions.csv
Right "transactions.csv"
```
It works! In one case we convert an `IO String` into an `IO (Either Message String)` and then nest that value into an `ExceptT`. In the other case we nest a `Right` value into `IO` (getting also an `IO (Either Message String)`) and also nest that value into an `ExceptT`.

But all this converting is tedious. First, since `ExceptT` is a monad, it offers a `return` function. Let' use it.

```haskell
> :{
| getFileName :: [String] -> ExceptT Message IO FilePath
| getFileName []      = ExceptT $ fmap Right $ prompt
| getFileName (arg:_) = return arg
| :}
```
Secondly, the combination `ExceptT . fmap Right` can be done using a general function found in `Control.Monad.Trans.Class`:

> ```haskell
> lift :: Monad m => m a -> t m a
> ```
>
> Lift a computation from the argument monad to the constructed monad.
>

```haskell
import Control.Monad.Trans.Class
> :{
| getFileName :: [String] -> ExceptT Message IO FilePath
| getFileName []      = lift prompt
| getFileName (arg:_) = return arg
| :}
```

What we have seen so far:
* we can compose together the `Either` monad with the `IO` monad, using the `ExceptT` monad transformer
* we hold values in `ExceptT` and extract them when needed with `runExceptT`, yielding a `Right` or `Left` value
* we chain monadic functions on these values with the bind (`>>=`) operation just like we would with `Either` values
* when a function leads to failure the chaining is cut short and we get an exception value: extracting it will yield a `Left`
* we also obtain values from IO operations `lift`ing these operations into the `ExceptT` monad
* thanks to exception `catch`ing when a failure occurs on the IO operation we also get an exception value

## Program #3: A chain of actions that can fail gracefully

Let's integrate what we learned into our program.
```haskell
import Control.Monad.Trans.Except ( ExceptT (..)
                                  , runExceptT
                                  , throwE
                                  )

import Control.Monad.Trans.Class     ( lift )
```
`Domain` is the type of values possibly acquired from IO operations, and that can indicate failure:
```haskell
type Domain = ExceptT Message IO
```
We can rewrite our conversion and control functions, using `throwE` instead of `Left`:
```haskell
readTransaction :: String -> Domain Transaction
readTransaction s =
    case reads s of
      []        -> throwE ("incorrect CSV format : " ++ s)
      ((t,_):_) -> return t

readTransactions :: String -> Domain [Transaction]
readTransactions = mapM readTransaction . lines

checkNotEmpty :: [Transaction] -> Domain [Transaction]
checkNotEmpty []  = throwE "no transactions"
checkNotEmpty txs = return txs

checkNonZero :: Transaction -> Domain Transaction
checkNonZero (Transaction _ 0) = throwE "amount equal to zero"
checkNonZero tx                 = return tx
```
Acquiring the CSV file name will follow the logic we experimented on *ghci*:

```haskell
getFileNameArg :: Domain FilePath
getFileNameArg = do
    args <- lift getArgs
    if null args then lift promptForFileName
                 else return (args !! 0)
                     where
    promptForFileName :: IO String
    promptForFileName = putStrLn "please enter a file name:" >> getLine
```
Dealing with IO exceptions implies using and wrapping the `catch` expression into the `ExceptT` monad:
```haskell
getFileContent :: FilePath -> Domain String
getFileContent fp = ExceptT $ (readFileE fp) `catch` handleE
    where
    readFileE :: FilePath -> IO (Either Message String)
    readFileE filePath =  Right <$> readFile filePath

    handleE :: IOException -> IO (Either Message String)
    handleE = return . Left . show
```
Note that we use the `<$>` (an infix shortcut for `fmap`) since we need to apply the `Right` function into the IO value that `readFile` acquired.

### Chaining all of this together
Now we can chain all these acquiring and controlling functions into a single one:
```haskell
getTransactions :: Domain [Transaction]
getTransactions = do
    filePath     <- getFileNameArg
    content      <- getFileContent filePath
    unchecked    <- readTransactions content
    notEmpty     <- checkNotEmpty unchecked
    transactions <- mapM checkNonZero notEmpty
    return $ transactions
```
Of course, using variables and left arrows is one way to make the chaining of action explicit. Another way is to use the bind operator:
```haskell
getTransactions :: Domain [Transaction]
getTransactions  = getFileNameArg
               >>= getFileContent
               >>= readTransactions
               >>= checkNotEmpty 
               >>= mapM checkNonZero
```
Finally, we need a way to output the result of our program, be it a failure or a valid list of summary lines:

```haskell
report :: Either Message [SummaryLine] -> String
report (Left msg)   = "Error: " ++ msg
report (Right sums) = unlines $ map showSummaryLine sums
```
As usual the main program will get the transactions, summarize them, and print the report:
```haskell
program3 :: IO ()
program3 = do
    transactions <- runExceptT getTransactions
    putStrLn $ report $ summarize <$> transactions

main :: IO ()
main = program3
```
The operator `<$>` is used instead of `$` in  `summarize <$> transactions` since this variable is bound to an `Either Message [Transaction]` value. We have to map `summarize` instead of just applying it.

```
$ ghc --make program3.hs
$ program3
please enter a file name:
transactions.csv
Equipment, 211.0
Groceries, 172.0
Insurance, 38.17
Savings, 500.0
```

## Conclusion

In this blog post, we went from a naïve haskell program doing IOs, to a less naïve implementation that deals with exceptions and failures, while trying to keep the program flow simple and the amount of boiler plate to a minimum. I hope you enjoyed it and learned from it. I would greatly appreciate feedback! You can email me at cthibauttof@gmail.com or contact me on Twitter: @ToF_.

The program can be found on [github](https://github.com/ToF-/Domain)

Enjoy!

*THANK YOU Andrea Chiou for helping me to improve my writing. THANK YOU Arnaud Bailly for making Haskell easier to learn for me.*

module Test.Tasty.Megaparsec (
    shouldSucceedFor,
    shouldSucceedOn,
    shouldFailFor,
    shouldFailOn,
    parseSatisfies,
    parseSatisfies',
    shouldParse,
    shouldParse',
    shouldParseLeaving,
    shouldParseLeaving',
    parseSatisfiesLeaving,
    parseSatisfiesLeaving',
    succeedsLeaving,
    succeedsLeaving',
    failsLeaving,
    failsLeaving',
    shouldFailWith,
    shouldFailWithM,
    shouldFailWith',
    shouldFailWithM',
    expectSuccess,
    expectSuccess_,
    expectFailure,
    initialState,
    initialPosState,

    -- * Re-exports
    module Text.Megaparsec.Error.Builder,
)
where

import Control.Monad (unless, void)

import qualified Data.List.NonEmpty as NE
import Test.Tasty.HUnit
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Error.Builder

{- | Create Assertion to check that the parser parses the given input.

Usage:

> (char 'c') `shouldSucceedFor` "c"

This is a convenience function wrapping 'shouldSucceedOn'.
-}
shouldSucceedFor ::
    (VisualStream s, TraversableStream s, ShowErrorComponent e, HasCallStack) =>
    -- | The 'Parsec' parser to run
    Parsec e s a ->
    -- | The input to run the parser on
    s ->
    Assertion
p `shouldSucceedFor` i = parse p "" `shouldSucceedOn` i

{- | Create Assertion to check that a parser parses the given input.

This is a more general version of 'shouldSucceedFor' that expects a parser
from a stream to either an error or parsing result (instead of a 'Parsec') as its first input.

Usage:

> parse (char 'c') "" `shouldSucceedOn` "c"
-}
shouldSucceedOn ::
    (VisualStream s, TraversableStream s, ShowErrorComponent e, HasCallStack) =>
    -- | The parser to run
    (s -> Either (ParseErrorBundle s e) b) ->
    -- | The input to run the parser on
    s ->
    Assertion
p `shouldSucceedOn` i = expectSuccess_ $ p i

{- | Create Assertion to check that a parser produces the specified result.

Usage:

> shouldParse' (char 'c') "c" 'c'

It is a convenience function wrapping 'shouldParse'.
-}
shouldParse' ::
    ( Show a
    , VisualStream s
    , TraversableStream s
    , ShowErrorComponent e
    , Eq a
    , HasCallStack
    ) =>
    -- | The 'Parsec' parser to run
    Parsec e s a ->
    -- | The input to run the parser on
    s ->
    -- | The expected parsing result
    a ->
    Assertion
shouldParse' p i o = parse p "" i `shouldParse` o

{- | Create Assertion to check that a parser produced the specified result.


This is a more general version of 'shouldParse'' that expects either an error or
a parsing result (instead of a 'Parsec') as its first input.

Usage:

> parse (char 'c') "" "c" `shouldParse'` 'c'
-}
shouldParse ::
    (Show a, VisualStream s, TraversableStream s, ShowErrorComponent e, Eq a, HasCallStack) =>
    -- | The result parsing, e.g. via 'parse'
    Either (ParseErrorBundle s e) a ->
    -- | The expected parsing result
    a ->
    Assertion
r `shouldParse` v = case r of
    Left e ->
        assertFailure $
            "expected: "
                ++ show v
                ++ "\nbut parsing failed with error:\n"
                ++ showBundle e
    Right x -> v @=? x

{- | Create an Assertion by saying that the parser should successfully
parse a value and that the value should satisfy some predicate.

Usage:

> parse (many alphaNumChar) "" "abc" `parseSatisfies` ((== 3) . length)
-}
parseSatisfies ::
    ( VisualStream s
    , TraversableStream s
    , ShowErrorComponent e
    , Show t
    ) =>
    -- | Result of parsing as returned by function like 'parse'
    Either (ParseErrorBundle s e) t ->
    -- | Predicate
    (t -> Bool) ->
    IO ()
r `parseSatisfies` p = case r of
    Left e ->
        assertFailure $
            "expected a parsed value to check against the predicate"
                ++ "\nbut parsing failed with error:\n"
                ++ showBundle e
    Right x ->
        unless (p x) . assertFailure $
            "the value did not satisfy the predicate: " ++ show x

{- | Create an Assertion by saying that the parser should successfully
parse a value, that the value should satisfy some predicate and that a
specified rest of the input was not consumed.

Use it with functions like 'runParser'' and 'runParserT''
that support incremental parsing.

Usage:

> parseSatisfiesLeaving (runParser' (many alphaNumChar) (initialState "abc!nothere!")) ((== 3) . length) "!nothere!"
-}
parseSatisfiesLeaving ::
    ( VisualStream s
    , TraversableStream s
    , ShowErrorComponent e
    , Show a
    , Show s
    , Eq s
    , HasCallStack
    ) =>
    (State s e, Either (ParseErrorBundle s e) a) ->
    (a -> Bool) ->
    s ->
    Assertion
parseSatisfiesLeaving (st, r) p s = do
    res <- expectSuccess r
    unless (p res) . assertFailure $
        "the value did not satisfy the predicate: " ++ show res
    checkUnconsumed s (stateInput st)

{- | Create an Assertion by saying that the parser should successfully
parse a value, that the value should satisfy some predicate and that a
specified rest of the input was not consumed.

This is a convenience function wrapping `parseSatisfiesLeaving`.

Usage:

> parseSatisfiesLeaving' (many alphaNumChar) "abc!not here!" ((== 3) . length) "!not here!"
-}
parseSatisfiesLeaving' ::
    ( VisualStream s
    , TraversableStream s
    , ShowErrorComponent e
    , Show a
    , Show s
    , Eq s
    , HasCallStack
    ) =>
    Parsec e s a ->
    s ->
    (a -> Bool) ->
    s ->
    Assertion
parseSatisfiesLeaving' p i = parseSatisfiesLeaving (runParser' p (initialState i))

{- | Create an Assertion by saying that the parser should successfully
parse a value and that the value should satisfy some predicate.

This function is a convenience function wrapping 'parseSatisfies'.

Usage:

> parseSatisfies' (many alphaNumChar) "abc!nothere!" ((== 3) . length)
-}
parseSatisfies' ::
    ( VisualStream s
    , TraversableStream s
    , ShowErrorComponent e
    , Show a
    , HasCallStack
    ) =>
    Parsec e s a ->
    s ->
    (a -> Bool) ->
    IO ()
parseSatisfies' p i o = parse p "" i `parseSatisfies` o

{- | Render a parse error bundle in a way that is suitable for inserting it
in a test suite report.
-}
showBundle ::
    ( VisualStream s
    , TraversableStream s
    , ShowErrorComponent e
    , HasCallStack
    ) =>
    ParseErrorBundle s e ->
    String
showBundle = unlines . fmap indent . lines . errorBundlePretty
  where
    indent x =
        if null x
            then x
            else "  " ++ x

{- | Create Assertion to check that a parser fails to parse the given input.

Usage:

> char 'c' `shouldFail` "d"

This is a convenience function wrapping 'shouldFailG'.
-}
shouldFailFor ::
    (Show a, HasCallStack) =>
    -- | Parser to run
    Parsec e p a ->
    -- | The input to run the parser on
    p ->
    Assertion
p `shouldFailFor` i = parse p "" `shouldFailOn` i

{- | Create Assertion to check that a parser fails to parse the given input.

This is a more general version of 'shouldFailFor' that expects a parser
from a stream to either an error or parsing result (instead of a 'Parsec') as its first input.

Usage:

> parse (char 'c') "" `shouldFailOn` "d"
-}
shouldFailOn ::
    (Show a, HasCallStack) =>
    -- | The parser to run
    (t -> Either (ParseErrorBundle s e) a) ->
    -- | The input to run the parser on
    t ->
    Assertion
p `shouldFailOn` i = expectFailure $ p i

{- | Check that a parser succeeds and leaves certain part of input
unconsumed. Use it with functions like 'runParser'' and 'runParserT''
that support incremental parsing.

Usage:

> runParser' (many (char 'x')) (initialState "xxa")
>   `succeedsLeaving` "a"

See also: 'initialState'.
-}
succeedsLeaving ::
    ( HasCallStack
    , Eq s
    , ShowErrorComponent e
    , VisualStream s
    , TraversableStream s
    , Show s
    ) =>
    -- | Parser that takes stream and produces result along with actual
    -- state information
    (State s e, Either (ParseErrorBundle s e) a) ->
    -- | Part of input that should be left unconsumed
    s ->
    Assertion
(st, r) `succeedsLeaving` s = do
    expectSuccess_ r
    checkUnconsumed s (stateInput st)

{- | Check that a parser succeeds and leaves certain part of input
unconsumed.

This is a convenience function wrapping `succeedsLeaving'.

Usage:

> succeedsLeaving' (many (char 'x')) "xxa" "a"

See also: 'initialState'.
-}
succeedsLeaving' ::
    ( ShowErrorComponent e
    , VisualStream s
    , TraversableStream s
    , Eq s
    , Show s
    ) =>
    -- | Parser to run
    Parsec e s a ->
    -- | Input to the parser
    s ->
    -- | Part of the unput that should be left unconsumed
    s ->
    Assertion
succeedsLeaving' p i r = runParser' p (initialState i) `succeedsLeaving` r

{- | A combination of `shouldParse'` and `succeedsLeaving'`.

Usage:

> shouldParseLeaving (runParser' (some (char 'x'))
>       (initialState "xkxa")) "xx" "b"
-}
shouldParseLeaving ::
    ( VisualStream s
    , TraversableStream s
    , ShowErrorComponent e
    , Show a
    , Show s
    , Eq a
    , Eq s
    ) =>
    -- | Parser that takes stream and produces result along with actual
    -- state information
    (State s e, Either (ParseErrorBundle s e) a) ->
    -- | The expected parsing result
    a ->
    -- | Part of input that should be left unconsumed
    s ->
    Assertion
shouldParseLeaving (st, res) v r = do
    resVal <- expectSuccess res
    v @=? resVal
    checkUnconsumed r (stateInput st)

{- | A combination of `shouldParse'` and `succeedsLeaving'`.

This is a convenience function wrapping `shouldParseLeaving`.

Usage:

> shouldParseLeaving' (some (char 'x')) "xkxa" "xx" "a"
-}
shouldParseLeaving' ::
    ( VisualStream s
    , TraversableStream s
    , ShowErrorComponent e
    , Show a
    , Show s
    , Eq a
    , Eq s
    ) =>
    -- | Parser to run
    Parsec e s a ->
    -- Input to run the parser on
    s ->
    -- | The expected parsing result
    a ->
    -- | Part of the unput that should be left unconsumed
    s ->
    Assertion
shouldParseLeaving' p i = shouldParseLeaving (runParser' p (initialState i))

{- | Check that a parser fails and leaves a certain part of input
unconsumed. Use it with functions like 'runParser'' and 'runParserT''
that support incremental parsing.

> runParser' (many (char 'x') <* eof) (initialState "xxa")
>   `failsLeaving` "a"

See also: 'initialState'.
-}
failsLeaving ::
    ( HasCallStack
    , Show a
    , Eq s
    , Show s
    ) =>
    -- | Parser that takes stream and produces result along with actual
    -- state information
    (State s e, Either (ParseErrorBundle s e) a) ->
    -- | Part of input that should be left unconsumed
    s ->
    Assertion
(st, r) `failsLeaving` s = do
    expectFailure r
    checkUnconsumed s (stateInput st)

{- | Check that a parser fails and leaves a certain part of input
unconsumed.

This is a convenience function wrapping `succeedsLeaving'.

Usage:

> failsLeaving' (many (char 'x') <* eof) "xxa" "a"

See also: 'initialState'.
-}
failsLeaving' ::
    (Eq s, Show a, Show s) =>
    -- | Parser to run
    Parsec e s a ->
    -- | The input to run the parser on
    s ->
    -- | Part of input that should be left unconsumed
    s ->
    Assertion
failsLeaving' p i r = runParser' p (initialState i) `failsLeaving` r

----------------------------------------------------------------------------
-- Helpers

-- | Given input for parsing, construct initial state for parser.
initialState :: s -> State s e
initialState s =
    State
        { stateInput = s
        , stateOffset = 0
        , statePosState = initialPosState s
        , stateParseErrors = []
        }

-- | Given input for parsing, construct initial positional state.
initialPosState :: s -> PosState s
initialPosState s =
    PosState
        { pstateInput = s
        , pstateOffset = 0
        , pstateSourcePos = initialPos ""
        , pstateTabWidth = defaultTabWidth
        , pstateLinePrefix = ""
        }

{- | Checks a parsing result for success.

If the parsing was not unexpectedly unsuccessfull, the error produced by Megaparsec is printed.
-}
expectSuccess ::
    (VisualStream s, TraversableStream s, ShowErrorComponent e, HasCallStack) =>
    -- | The result of a run parser
    Either (ParseErrorBundle s e) a ->
    IO a
expectSuccess r = case r of
    Left e ->
        assertFailure $
            "the parser is expected to succeed, but it failed with:\n"
                ++ showBundle e
    Right v -> return v

expectSuccess_ ::
    (VisualStream s, TraversableStream s, ShowErrorComponent e, HasCallStack) =>
    -- | The result of a run parser
    Either (ParseErrorBundle s e) a ->
    Assertion
expectSuccess_ r = void $ expectSuccess r

{- | Checks a parsing result for failure.

If the parsing was unexpectedly successfull, the parsed result is printed.
-}
expectFailure ::
    (Show a, HasCallStack) =>
    -- The result of a run parser
    Either (ParseErrorBundle s e) a ->
    Assertion
expectFailure r = case r of
    Left _ -> return ()
    Right v ->
        assertFailure $
            "the parser is expected to fail, but it parsed: " ++ show v

-- | Compare two streams for equality and in the case of mismatch report it.
checkUnconsumed ::
    (Eq p, Show p, HasCallStack) =>
    -- | Expected unconsumed output
    p ->
    -- | Actual unconsumed output
    p ->
    IO ()
checkUnconsumed e a =
    unless (e == a) . assertFailure $
        "the parser is expected to leave unconsumed input: "
            ++ show e
            ++ "\nbut it left this: "
            ++ show a

{- | Create an expectation that parser should fail producing certain
'ParseError'. Use the 'err' function re-exported from this module to construct a
'ParseError' to compare with.

Usage:

> parse (char 'x') "" "b" `shouldFailWith` err posI (utok 'b' <> etok 'x')
-}
shouldFailWith ::
    ( HasCallStack
    , ShowErrorComponent e
    , Stream s
    , VisualStream s
    , TraversableStream s
    , Show a
    , Eq e
    ) =>
    -- | The result of parsing
    Either (ParseErrorBundle s e) a ->
    -- | Expected parse error
    ParseError s e ->
    Assertion
r `shouldFailWith` perr1 = r `shouldFailWithM` [perr1]


{- | Create an expectation that parser should fail producing certain
'ParseError'. Use the 'err' function re-exported from this module to construct a
'ParseError' to compare with.

This is a convenience function wrapping `shouldFailWith`.

Usage:

> shouldFailWith'  (char 'x') "b" $ err 0 (utok 'b' <> etok 'x')
-}
shouldFailWith' ::
    ( ShowErrorComponent e
    , VisualStream s
    , TraversableStream s
    , Show a
    ) =>
    -- | The parser
    Parsec e s a ->
    -- | The input to run the parser on
    s ->
    -- | Expected parse error
    ParseError s e ->
    Assertion
shouldFailWith' p i e = shouldFailWithM' p i [e]

{- | Similar to 'shouldFailWith'', but allows us to check parsers that can
report more than one parse error at a time.
-}
shouldFailWithM' ::
    ( ShowErrorComponent e
    , VisualStream s
    , TraversableStream s
    , Show a
    ) =>
    -- | The parser to run
    Parsec e s a ->
    -- | The input to run the parser on
    s ->
    -- | Expected parse errors
    [ParseError s e] ->
    Assertion
shouldFailWithM' p i e = parse p "" i `shouldFailWithM` e

{- | Similar to 'shouldFailWith', but allows us to check parsers that can
report more than one parse error at a time.
-}
shouldFailWithM ::
    ( HasCallStack
    , ShowErrorComponent e
    , Stream s
    , VisualStream s
    , TraversableStream s
    , Show a
    , Eq e
    ) =>
    -- | The result of parsing
    Either (ParseErrorBundle s e) a ->
    -- | Expected parse errors, the argument is a normal linked list (as
    -- opposed to the more correct 'NonEmpty' list) as a syntactical
    -- convenience for the user, passing empty list here will result in an
    -- error
    [ParseError s e] ->
    Assertion
r `shouldFailWithM` perrs1' = case r of
    Left e0 ->
        let e1 = e0{bundleErrors = perrs1}
            perrs0 = bundleErrors e0
            perrs1 = NE.fromList perrs1'
         in unless (perrs0 == perrs1) . assertFailure $
                "the parser is expected to fail with:\n"
                    ++ showBundle e1
                    ++ "but it failed with:\n"
                    ++ showBundle e0
    Right v ->
        assertFailure $
            "the parser is expected to fail, but it parsed: " ++ show v
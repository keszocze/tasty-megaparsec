import Control.Monad.Combinators (many, some)
import Data.Void (Void)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.ExpectedFailure (expectFail)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.Megaparsec (
    err,
    etok,
    failsLeaving,
    failsLeaving',
    initialState,
    parseSatisfies,
    parseSatisfies',
    parseSatisfiesLeaving,
    parseSatisfiesLeaving',
    shouldFailFor,
    shouldFailWith,
    shouldFailWith',
    shouldFailOn,
    shouldParse,
    shouldParse',
    shouldParseLeaving,
    shouldParseLeaving',
    shouldSucceedFor,
    shouldSucceedOn,
    succeedsLeaving,
    succeedsLeaving',
    utok
 )
import Text.Megaparsec (MonadParsec (eof), Parsec, parse, runParser')
import Text.Megaparsec.Char (alphaNumChar, char, letterChar)

main :: IO ()
main = do
    defaultMain $
        testGroup
            "Megaparsec Tasty Wrapper Tests"
            [ shouldSucceedForTests
            , failsLeavingTests
            , succeedsLeavingTests
            , shouldFailTests
            , shouldParseTests
            , parseSatisfiesTests
            , shouldParseLeavingTests
            , parseSatisfiesLeavingTests
            , shouldFailWithTests
            ]

{- | Convenience type for the parser used for testing

Currently, we only test streams of type 'String'
-}
type Parser = Parsec Void String

lC, aC :: Parser Char

{- | Alias for the 'letterChar' parser

This definition simply ensures that Haskell uses the correct type
-}
lC = letterChar

{- | Alias for the 'alphaNumChar' parser

This definition simply ensures that Haskell uses the correct type
-}
aC = alphaNumChar

succeedsLeavingTests :: TestTree
succeedsLeavingTests = testCase "Tests for `succeedsLeaving`" $ do
    runParser' (many (char 'x') :: Parser String) (initialState "xxa") `succeedsLeaving` "a"
    succeedsLeaving' (many (char 'x') :: Parser String) "xxa" "a"

failsLeavingTests :: TestTree
failsLeavingTests = testCase "Tests for `succeedsLeaving`" $ do
    runParser' (many (char 'x' :: Parser Char) <* eof) (initialState "xxa") `failsLeaving` "a"
    failsLeaving' (many (char 'x' :: Parser Char) <* eof) "xxa" "a"

shouldParseLeavingTests :: TestTree
shouldParseLeavingTests = testGroup "Tests for shouldParseLeaving" [succeeding, failing]
  where
    succeeding = testCase "succeeding tests" $ do
        shouldParseLeaving' p "xxa" "xx" "a"
        shouldParseLeaving (runParser' p (initialState "xxa")) "xx" "a"
    failing =
        testGroup
            "expectedly failing tests"
            [ expectFail $ testCase "fails on the comparison" $ do
                shouldParseLeaving' (some (char 'x' :: Parser Char)) "xkxa" "xx" "a"
                shouldParseLeaving (runParser' p (initialState "xkxa")) "xx" "a"
            , expectFail $ testCase "fails on the rest" $ do
                shouldParseLeaving' (some (char 'x' :: Parser Char)) "xxa" "xx" "b"
                shouldParseLeaving (runParser' p (initialState "xkxa")) "xx" "b"
            ]
    p = some (char 'x' :: Parser Char)

shouldSucceedForTests :: TestTree
shouldSucceedForTests =
    testGroup
        "Tests for `shouldSucceedFor`"
        [ testCase "letterChar" $ do
            lC `shouldSucceedFor` "x"
            parse lC "" `shouldSucceedOn` "x"
            lC `shouldSucceedFor` "xdg"
        , testCase "some alphaNumChars" $ do
            some aC `shouldSucceedFor` "xk43g"
            some aC `shouldSucceedFor` "x"
            parse (some aC) "" `shouldSucceedOn` "xk43g"
        , testCase "many alphaNumChars" $ do
            many aC `shouldSucceedFor` "xk43g"
            many aC `shouldSucceedFor` "x"
            many aC `shouldSucceedFor` ""
            parse (many aC) "" `shouldSucceedOn` "x"
        ]

shouldFailWithTests :: TestTree
shouldFailWithTests =
    testCase "Tests for shouldFailWith" $ do
        parse (char 'x' :: Parser Char) "" "b"
            `shouldFailWith` err 0 (utok 'b' <> etok 'x')
        shouldFailWith' (char 'x' :: Parser Char) "b" $ err 0 (utok 'b' <> etok 'x')

parseSatisfiesTests :: TestTree
parseSatisfiesTests =
    testGroup
        "Tests for `parseSatisfies"
        [ testCase "length of parsed alphaNumChars" $ parse (many aC) "" "abc!not here!" `parseSatisfies` ((== 3) . length)
        , testCase "empty parse" $ parse (many aC) "" "!not even here!abc!not here!" `parseSatisfies` null
        , testCase "length of parsed alphaNumChars" $ parseSatisfies' (many aC) "abc!not here!" ((== 3) . length)
        , testCase "empty parse" $ parseSatisfies' (many aC) "!not even here!abc!not here!" null
        ]

parseSatisfiesLeavingTests :: TestTree
parseSatisfiesLeavingTests =
    testGroup
        "Tests for `parseSatisfiesLeaving"
        [ testCase "length of parsed alphaNumChars and rest" $ parseSatisfiesLeaving (runParser' (many aC) (initialState "abc!not here!")) ((== 3) . length) "!not here!"
        , testCase "empty parse and rest" $ parseSatisfiesLeaving (runParser' (many aC) (initialState "!not even here!abc!not here!")) null "!not even here!abc!not here!"
        , testCase "length of parsed alphaNumChars and rest" $ parseSatisfiesLeaving' (many aC) "abc!not here!" ((== 3) . length) "!not here!"
        , testCase "empty parse and rest" $ parseSatisfiesLeaving' (many aC) "!not even here!abc!not here!" null "!not even here!abc!not here!"
        ]

-- shouldParseLeaving (runParser' p (initialState "xkxa")) "xx" "a"

shouldFailTests :: TestTree
shouldFailTests =
    testGroup
        "Tests for `shouldFail`"
        [ testCase "letterChar" $ do
            lC `shouldFailFor` ""
            lC `shouldFailFor` "3"
            parse lC "" `shouldFailOn` ""
            parse lC "" `shouldFailOn` "3"
        , testCase "some letterChars" $ do
            some lC `shouldFailFor` "43gdfhdf"
            some lC `shouldFailFor` ""
            parse (some lC) "" `shouldFailOn` "43gdfhdf"
            parse (some lC) "" `shouldFailOn` ""
        ]

shouldParseTests :: TestTree
shouldParseTests = testGroup "Tests for shouldParse" [original, convenient]
  where
    convenient =
        testGroup
            "Tests for convenient wrapper function"
            [ testCase "letterChar" $ do
                shouldParse' lC "x" 'x'
                shouldParse' lC "xdg" 'x'
            , testCase "some alphaNumChars" $ do
                shouldParse' (some aC) "xk43g" "xk43g"
                shouldParse' (some aC) "x" "x"
            , testCase "many alphaNumChars" $ do
                shouldParse' (many aC) "xk43g" "xk43g"
                shouldParse' (many aC) "x" "x"
                shouldParse' (many aC) "" ""
            , testCase "some letterChars" $ do
                shouldParse' (some lC) "xk43g" "xk"
                shouldParse' (some lC) "x" "x"
            , testCase "many letterChars" $ do
                shouldParse' (many lC) "xk43g" "xk"
                shouldParse' (many lC) "x" "x"
                shouldParse' (many lC) "" ""
            ]
    original =
        testGroup
            "Tests for convenient wrapper function"
            [ testCase "letterChar" $ do
                parse lC "" "x" `shouldParse` 'x'
                parse lC "" "xdg" `shouldParse` 'x'
            , testCase "some alphaNumChars" $ do
                parse (some aC) "" "xk43g" `shouldParse` "xk43g"
                parse (some aC) "" "x" `shouldParse` "x"
            , testCase "many alphaNumChars" $ do
                parse (many aC) "" "xk43g" `shouldParse` "xk43g"
                parse (many aC) "" "x" `shouldParse` "x"
                parse (many aC) "" "" `shouldParse` ""
            , testCase "some letterChars" $ do
                parse (some lC) "" "xk43g" `shouldParse` "xk"
                parse (some lC) "" "x" `shouldParse` "x"
            , testCase "many letterChars" $ do
                parse (many lC) "" "xk43g" `shouldParse` "xk"
                parse (many lC) "" "x" `shouldParse` "x"
                parse (many lC) "" "" `shouldParse` ""
            ]

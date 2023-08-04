# Tasty Megaparsec

[![Haskell CI](https://github.com/keszocze/tasty-megaparsec/actions/workflows/haskell.yml/badge.svg)](https://github.com/keszocze/tasty-megaparsec/actions/workflows/haskell.yml)

This package is intended for testing [`Megaparsec`](https://hackage.haskell.org/package/megaparsec) parsers with
with [`Tasty`](https://hackage.haskell.org/package/tasty) via [`Tasty.HUnit`](https://hackage.haskell.org/package/tasty-hunit).

It is a partial port of [`hspec-megaparsec`](https://hackage.haskell.org/package/hspec-megaparsec) but extents it by adding more convenience features for directly testing `Parser a` type parsers (i.e. less manual calls to `parse` are necessary) as well as know functionality:

- `shouldParseLeaving`  Checks for the provided parse result and provided rest of the input
- `shouldSatisfyLeaving` Checkst that the parse result satisfies a given predicate and that the specified rest of the input is left over



## Usage

The library should, hopefully, be easy to use. The naming scheme of [hspec-megaparsec](https://hackage.haskell.org/package/hspec-megaparsec) is used throughout this library. When applicable, versions directly operating on `Parser a` type parsers are provided. These versions are indicated by a tick (e.g., `shouldParse` and `shouldParse'`) and are not to be meant to be uesd infix. There is an exception for `shouldSucceedOn`. The `Parser a` version is called `shouldSuccedFor`.

Examples:

```Haskell
main = do defaultMain $ testGroup
        "Tasty Megaparsec examples"
        [ testCase "check if it parses ('classical' version)" $ parse (some alphaNumChar) "" `shouldSucceedOn` "xk43g"
        , testCase "check if it parses ('Parser version')" $ some alphaNumChar `shouldSucceedFor` "xk43g"
        , testCase "shouldParse example ('classical' version)" $ parse (some alphaNumChar) "" "xk43g" `shouldParse` "xk43g"
        , testCase "shouldParse example ('Parser version')" $ shouldParse' (some alphaNumChar) "xk43g" "xk43g"
        ]
```

For more examples, see also the [provided tests](https://github.com/keszocze/tasty-megaparsec#readme#test/Spec).

## Contribution

Issues, bugs, and questions may be reported in [the GitHub issue tracker for
this project](https://github.com/keszocze/tasty-megaparsec/blob/master/test/Spec.hs).

If you know how to find bounds for the library versions, e.g. `base >= 4.7 && < 5` please let
me know as my current workflow is to use let stack and stackage do the magic.

## Known issues

Haddock fails with:
```
tasty-expected-failure> haddock: internal error: <redacted>/9.2.8/doc/unbounded-delays-0.1.1.1/doc-index.json: openBinaryFile: does not exist (No such file or directory)
Progress 1/2

Error: [S-7282]
       Stack failed to execute the build plan.
       
       While executing the build plan, Stack encountered the following errors:
       
       [S-7011]
       While building package tasty-expected-failure-0.12.3 (scroll up to its section to see the error) using:
       /home/keszocze/.stack/setup-exe-cache/x86_64-linux-tinfo6/Cabal-simple_SvXsv1f__3.6.3.0_ghc-9.2.8 --verbose=1 --builddir=.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.6.3.0 haddock --html --hoogle --html-location=../$pkg-$version/ --haddock-option=--hyperlinked-source --haddock-option=--odir=./docs --haddock-option=--quickjump
```
If you know how to fix this, please let me know.

## License

Licencing issues are still pending. At some point in the future, this will be filled with actual info. 
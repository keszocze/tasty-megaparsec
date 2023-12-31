# Tasty Megaparsec

[![License](https://img.shields.io/badge/License-BDS3-brightgreen)](https://github.com/keszocze/tasty-megaparsec/blob/master/LICENSE)
[![Haskell CI](https://github.com/keszocze/tasty-megaparsec/actions/workflows/haskell.yml/badge.svg)](https://github.com/keszocze/tasty-megaparsec/actions/workflows/haskell.yml)
[![Haskell CI (nightly)](https://github.com/keszocze/tasty-megaparsec/actions/workflows/haskell_nightly.yml/badge.svg)](https://github.com/keszocze/tasty-megaparsec/actions/workflows/haskell_nightly.yml)


This package is intended for testing [`Megaparsec`](https://hackage.haskell.org/package/megaparsec) parsers with
with [`Tasty`](https://hackage.haskell.org/package/tasty) via [`Tasty.HUnit`](https://hackage.haskell.org/package/tasty-hunit).

It is a partial port of [`hspec-megaparsec`](https://hackage.haskell.org/package/hspec-megaparsec) but extents it by adding more convenience features for directly testing `Parser a` type parsers (i.e. less manual calls to `parse` are necessary) as well as new functionality such as

- `shouldParseLeaving`  Checks for the provided parse result and provided rest of the input
- `shouldSatisfyLeaving` Checks that the parse result satisfies a given predicate and that the specified rest of the input is left over



## Usage

The library should, hopefully, be easy to use. The naming scheme of [hspec-megaparsec](https://hackage.haskell.org/package/hspec-megaparsec) is used throughout this library. When applicable, versions directly operating on `Parser a` type parsers are provided. These versions are indicated by a tick (e.g., `shouldParse` and `shouldParse'`) and are not to be meant to be used infix. There are exceptions for `shouldSucceedOn` and `shouldFailOn`. The `Parser a` versions are `shouldSuccedFor` and'`shouldFailFor`, respectively.

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

For more examples, see also the [provided tests](https://github.com/keszocze/tasty-megaparsec/blob/master/test/Spec.hs).

## Contribution

Issues, bugs, and questions may be reported in [the GitHub issue tracker for
this project](https://github.com/keszocze/tasty-megaparsec/blob/master/test/Spec.hs).



## License
Copyright © 2023–present Oliver Keszöcze

Copyright © 2016–present Mark Karpov

Distributed under BSD 3 clause license.

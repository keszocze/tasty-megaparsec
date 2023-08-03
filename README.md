# Tasty Megaparsec

[![Haskell CI](https://github.com/keszocze/tasty-megaparsec/actions/workflows/haskell.yml/badge.svg)](https://github.com/keszocze/tasty-megaparsec/actions/workflows/haskell.yml)

This package is intended for testing [`Megaparsec`](https://hackage.haskell.org/package/megaparsec) parsers with
with [`Tasty`](https://hackage.haskell.org/package/tasty) via [`Tasty.HUnit`](https://hackage.haskell.org/package/tasty-hunit).

It is a partial port of [`hspec-megaparsec`](https://hackage.haskell.org/package/hspec-megaparsec) but extents it by adding more convenience features for directly testing `Parser a` type parsers, i.e. less manual calls to `parse` are necessary.

## Contribution

Issues, bugs, and questions may be reported in [the GitHub issue tracker for
this project](https://github.com/keszocze/tasty-megaparsec/issues).

## License

Licencing issues are still pending. At some point in the future, this will be filled with actual info.
# Changelog for `holeyexp`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased
## [0.3.0.0] - 2026-09-02
### Changed
    - Now builds with GHC 9.8.4. This required the removal of the 
      TypeApplications extension in the Test.QuickCheck.HExp. In order to 
      continue building we had to add new constraints to the genHExpNat, 
      genHExp, and the Arbitrary instance for HExp.

## [0.2.0.0] - 2026-08-28
### Added
    - New documentation and complete Haddock docs on all definitions.
    - New cabal package.
### Changed
    - Better naming of combinators. 
           - Data.HoleyExp.HExp.empty is now emptyExp
           - Data.HoleyExp.HExp.hole is now empty           
           - Data.HoleyExp.HExp.plugHole is now plug
           - Data.HoleyExp.HExp.fillHole is now update
           - Data.HoleyExp.HExp.placeInHole is now place
    - Moved template haskell and JSON out of this package and into their own packages.

## [0.1.0.0] - 2026-08-14
    - First complete implementation.
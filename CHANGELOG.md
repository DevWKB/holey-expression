# Changelog for `holey-expression`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

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

## [0.1.0.0] - 2026-08-14
    - First complete implementation.
# Project DazzleBox
DazzleBox is a Pac Man clone written in Haskell as a student project.

#### Dependencies
* Gloss 1.10

#### Usage
If you do not already have ``Gloss 1.10`` installed, turn the root directory of the project into a sandbox and from there install gloss.
```bash
$ cabal sandbox init
$ cabal install gloss==1.10.*
```

Run the program using:
```bash
$ cabal run
```

##### Testing
Testing requires the packages ``hspec >= 2.4.1`` and ``QuickCheck >= 2.9.2``. Install these before running any tests.

```bash
$ cabal test
```

### Authors
DazzleBox is created by Love Nordling (@lovenordling), Ulf Sigvardsson (@ulfsigvardsson) and Ardalan Samimi (@pkrll).

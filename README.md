# DazzleBox
DazzleBox is a Pac-Man clone written in Haskell as a student project for Program Design and Data Structures at Uppsala University.

#### Dependencies
* Gloss 1.10
* Gloss-Juicy
* HSpec (for testing)

##### Installing Dependencies
If you do not already have the packages installed, turn the root directory of the project into a sandbox and from there install gloss.
```bash
$ cabal sandbox init
$ cabal install gloss==1.10.*
$ cabal install gloss-juicy
$ cabal install hspec
$ cabal install hspec-contrib
```

#### Usage
Start the game by typing:
```bash
$ cabal run
```

##### Testing
Testing requires the packages ``hspec >= 2.4.1`` and ``hspec-contrib``. Install these before running any tests.

```bash
$ cabal test --show-detail=always
```

### Authors
DazzleBox is created by Love Nordling ([@lovenording](https://github.com/lovenordling)),
 Ulf Sigvardsson ([@ulfsigvardsson](https://github.com/ulfsigvardsson)), and Ardalan Samimi ([@pkrll](https://github.com/pkrll)).

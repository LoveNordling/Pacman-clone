## Unit Testing with <s>Cable</s> Cabal
We will use __Cabal__ and ``HSpec`` together with ``HUnit`` to unit test DazzleBox.

All tests should be written as ``HUnit`` test suites. These will later be converted automatically by ``HSpec``. Don't worry about it.

Below is a guide on how to design the tests.

#### Writing tests
All modules should include a test suite, consisting of a number of tests. These should be at the bottom of the file, and marked out:
```haskell
... some awesome Haskell code needed to be tested ...

-------------------------------------------
-- TEST CASES
-------------------------------------------
test1, test2, test3, test4 :: Test
testSuite = TestList [ test1, test2, test3, test4 ]
```

The tests them selves must have a label associated with them. This is done by using ``TestLabel "Some description"...`` followed by the test:

```haskell
test1 = TestLabel "Some test" $ TestCase $ assertEqual "" (value) (expression)
```

Don't forget to export the test suite (and not the actual tests themselves.)

The tests are later run from ``Test/main.hs``, where the ``HUnit`` tests are converted to ``HSpec``.

Adding tests here is easy. Just use the ``describe`` function which takes as its first argument the description and then put in a do statement where you import the test and convert it using ``fromHUnitTest``:

```haskell
main :: IO ()
main = hspec $ do
    describe "Graphics Engine Test Suite" $ do
        fromHUnitTest GraphicsEngine.testSuite
    describe "AI Test Suite" $ do
        fromHUnitTest AI.testSuite
```

And that's it. Though it is better one person does this last part, so we don't overwrite each other.

#### Running tests
You run tests using cabal. Type in the Terminal, from within the __root__ folder of the project:

```bash
$ cabal test
```

module Skeletest.Internal.SnapshotSpec (spec) where

import Data.Aeson qualified as Aeson
import Data.String (fromString)
import Skeletest
import Skeletest.Predicate qualified as P

import Skeletest.TestUtils.Integration

spec :: Spec
spec = do
  integration . it "detects corrupted snapshot files" $ do
    runner <- getFixture
    addTestFile runner "ExampleSpec.hs" $
      [ "module ExampleSpec (spec) where"
      , ""
      , "import Skeletest"
      , "import qualified Skeletest.Predicate as P"
      , ""
      , "spec = it \"should error\" $ do"
      , "  \"\" `shouldSatisfy` P.matchesSnapshot"
      ]
    addTestFile runner "__snapshots__/ExampleSpec.snap.md" ["asdf"]

    (code, stdout, stderr) <- runTests runner []
    code `shouldBe` ExitFailure 1
    stderr `shouldBe` ""
    stdout `shouldSatisfy` P.matchesSnapshot

  integration . it "uses registered snapshot renderers" $ do
    runner <- getFixture
    setMainFile runner $
      [ "import Skeletest.Main"
      , "import Lib.User"
      , "snapshotRenderers ="
      , "  [ renderWithShow @User"
      , "  ]"
      ]
    addTestFile runner "Lib/User.hs" $
      [ "module Lib.User (User (..)) where"
      , "data User = User {name :: String, age :: Int} deriving (Show)"
      ]
    addTestFile runner "ExampleSpec.hs" $
      [ "module ExampleSpec (spec) where"
      , ""
      , "import Lib.User"
      , "import Skeletest"
      , "import qualified Skeletest.Predicate as P"
      , ""
      , "spec = it \"test user\" $ do"
      , "  let testUser = User {name = \"Alice\", age = 30}"
      , "  testUser `shouldSatisfy` P.matchesSnapshot"
      ]

    _ <- expectSuccess $ runTests runner ["-u"]
    snapshot <- readTestFile runner "__snapshots__/ExampleSpec.snap.md"
    snapshot `shouldSatisfy` P.hasInfix "User {name = \"Alice\", age = 30}"

  it "renders JSON values" $ do
    let result = Aeson.decode $ fromString "{\"hello\": [\"world\", 1]}"
    (result :: Maybe Aeson.Value) `shouldSatisfy` P.just P.matchesSnapshot

  integration . it "shows helpful failure messages" $ do
    runner <- getFixture
    addTestFile runner "ExampleSpec.hs" $
      [ "module ExampleSpec (spec) where"
      , ""
      , "import Skeletest"
      , "import qualified Skeletest.Predicate as P"
      , ""
      , "spec = it \"fails\" $ do"
      , "  unlines [\"new1\", \"same1\", \"same2\", \"new2\"] `shouldSatisfy` P.matchesSnapshot"
      ]
    addTestFile runner "__snapshots__/ExampleSpec.snap.md" $
      [ "# Example"
      , ""
      , "## fails"
      , ""
      , "```"
      , "same1"
      , "old1"
      , "same2"
      , "old2"
      , "```"
      ]

    (code, stdout, stderr) <- runTests runner []
    code `shouldBe` ExitFailure 1
    stderr `shouldBe` ""
    stdout `shouldSatisfy` P.matchesSnapshot

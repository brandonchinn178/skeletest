{-# LANGUAGE RecordWildCards #-}

module Skeletest.Internal.SnapshotSpec (spec) where

import Data.Aeson qualified as Aeson
import Data.String (fromString)
import Skeletest
import Skeletest.Predicate qualified as P
import Skeletest.Prop.Gen qualified as Gen
import Skeletest.Prop.Range qualified as Range

import Skeletest.Internal.Snapshot (
  SnapshotFile (..),
  SnapshotValue (..),
  decodeSnapshotFile,
  encodeSnapshotFile,
  normalizeSnapshotFile,
 )
import Skeletest.TestUtils.Integration

spec :: Spec
spec = do
  prop "decodeSnapshotFile . encodeSnapshotFile === pure" $ do
    (decodeSnapshotFile . encodeSnapshotFile) P.=== pure `shouldSatisfy` P.isoWith genSnapshotFile

  prop "normalizeSnapshotFile is idempotent" $ do
    file <- forAll genSnapshotFileRaw
    n <- forAll $ Gen.int (Range.linear 1 10)
    let normalizeSnapshotFile' = foldr (.) id $ replicate n normalizeSnapshotFile
    normalizeSnapshotFile' file `shouldBe` normalizeSnapshotFile file

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

genSnapshotFileRaw :: Gen SnapshotFile
genSnapshotFileRaw = do
  moduleName <- Gen.text (Range.linear 0 100) genHsModuleChar
  snapshots <- Gen.map rangeNumTests genSnapshot
  pure SnapshotFile{..}
  where
    rangeNumTests = Range.linear 0 10
    rangeSnapshotsPerTest = Range.linear 0 5
    rangeSnapshotSize = Range.linear 0 1000

    genHsModuleChar = Gen.choice [Gen.alphaNum, pure '\'']

    genSnapshot = do
      ident <- Gen.list (Range.linear 1 10) (Gen.text (Range.linear 1 100) Gen.unicode)
      vals <- Gen.list rangeSnapshotsPerTest genSnapshotVal
      pure (ident, vals)

    genSnapshotVal = do
      snapshotContent <- Gen.text rangeSnapshotSize Gen.unicode
      snapshotLang <- Gen.maybe $ Gen.text (Range.linear 1 5) Gen.unicode
      pure SnapshotValue{..}

genSnapshotFile :: Gen SnapshotFile
genSnapshotFile = normalizeSnapshotFile <$> genSnapshotFileRaw

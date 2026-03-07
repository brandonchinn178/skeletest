{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Skeletest.Internal.SnapshotSpec (spec) where

import Data.Aeson qualified as Aeson
import Data.String (fromString)
import Data.Text qualified as Text
import Skeletest
import Skeletest.Internal.Snapshot (
  SnapshotFile (..),
  SnapshotValue (..),
  decodeSnapshotFile,
  encodeSnapshotFile,
  normalizeSnapshotFile,
 )
import Skeletest.Predicate qualified as P
import Skeletest.Prop.Gen qualified as Gen
import Skeletest.Prop.Range qualified as Range
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

  integration . it "creates a new snapshot" $ do
    runner <- getFixture @TestRunner
    runner.addTestFile "ExampleSpec.hs" $
      [ "module ExampleSpec (spec) where"
      , ""
      , "import Skeletest"
      , "import qualified Skeletest.Predicate as P"
      , ""
      , "spec = it \"test\" $ do"
      , "  \"example result\" `shouldSatisfy` P.matchesSnapshot"
      ]

    (stdout, stderr) <- expectFailure $ runner.runTestsWith def{cliArgs = []}
    stderr `shouldBe` ""
    stdout `shouldSatisfy` P.matchesSnapshot

    _ <- expectSuccess $ runner.runTestsWith def{cliArgs = ["-u"]}
    snapshot <- runner.readTestFile "__snapshots__/ExampleSpec.snap.md"
    snapshot `shouldSatisfy` P.hasInfix "example result"

    _ <- expectSuccess $ runner.runTestsWith def{cliArgs = []}
    pure ()

  integration . it "updates an existing snapshot" $ do
    runner <- getFixture @TestRunner
    runner.addTestFile "ExampleSpec.hs" $
      [ "module ExampleSpec (spec) where"
      , ""
      , "import Skeletest"
      , "import qualified Skeletest.Predicate as P"
      , ""
      , "spec = it \"fails\" $ do"
      , "  unlines [\"new1\", \"same1\", \"same2\", \"new2\"] `shouldSatisfy` P.matchesSnapshot"
      ]
    runner.addTestFile "__snapshots__/ExampleSpec.snap.md" $
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

    (stdout, stderr) <- expectFailure $ runner.runTestsWith def{cliArgs = []}
    stderr `shouldBe` ""
    stdout `shouldSatisfy` P.matchesSnapshot

    _ <- expectSuccess $ runner.runTestsWith def{cliArgs = ["-u"]}
    snapshot <- runner.readTestFile "__snapshots__/ExampleSpec.snap.md"
    snapshot `shouldSatisfy` P.hasInfix "new1\nsame1\nsame2\nnew2"

    _ <- expectSuccess $ runner.runTestsWith def{cliArgs = []}
    pure ()

  integration . it "detects corrupted snapshot files" $ do
    runner <- getFixture @TestRunner
    runner.addTestFile "ExampleSpec.hs" $
      [ "module ExampleSpec (spec) where"
      , ""
      , "import Skeletest"
      , "import qualified Skeletest.Predicate as P"
      , ""
      , "spec = it \"should error\" $ do"
      , "  \"\" `shouldSatisfy` P.matchesSnapshot"
      ]
    runner.addTestFile "__snapshots__/ExampleSpec.snap.md" ["asdf"]

    (stdout, stderr) <- expectFailure runner.runTests
    stderr `shouldBe` ""
    stdout `shouldSatisfy` P.matchesSnapshot

  integration . it "uses registered snapshot renderers" $ do
    runner <- getFixture @TestRunner
    runner.setMainFile
      [ "import Skeletest.Main"
      , "import Lib.User"
      , "snapshotRenderers ="
      , "  [ renderWithShow @User"
      , "  ]"
      ]
    runner.addTestFile "Lib/User.hs" $
      [ "module Lib.User (User (..)) where"
      , "data User = User {name :: String, age :: Int} deriving (Show)"
      ]
    runner.addTestFile "ExampleSpec.hs" $
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

    _ <- expectSuccess $ runner.runTestsWith def{cliArgs = ["-u"]}
    snapshot <- runner.readTestFile "__snapshots__/ExampleSpec.snap.md"
    snapshot `shouldSatisfy` P.hasInfix "User {name = \"Alice\", age = 30}"

  it "renders JSON values" $ do
    let result = Aeson.decode $ fromString "{\"hello\": [\"world\", 1]}"
    (result :: Maybe Aeson.Value) `shouldSatisfy` P.just P.matchesSnapshot

  integration . it "works when test changes directories" $ do
    runner <- getFixture @TestRunner
    runner.addTestFile "ExampleSpec.hs" $
      [ "module ExampleSpec (spec) where"
      , ""
      , "import Skeletest"
      , "import qualified Skeletest.Predicate as P"
      , "import System.Directory (withCurrentDirectory)"
      , ""
      , "spec = it \"tests snapshot\" $ do"
      , "  withCurrentDirectory \"/\" $ do"
      , "    (123 :: Int) `shouldSatisfy` P.matchesSnapshot"
      ]

    let args = def{ghcArgs = ["-package", "directory"]}
    _ <- expectSuccess $ runner.runTestsWith args{cliArgs = ["-u"]}
    _ <- expectSuccess $ runner.runTestsWith args
    pure ()

genSnapshotFileRaw :: Gen SnapshotFile
genSnapshotFileRaw = do
  testFile <- genHsModule
  snapshots <- Gen.map rangeNumTests genSnapshot
  pure SnapshotFile{..}
 where
  rangeNumTests = Range.linear 0 10
  rangeSnapshotsPerTest = Range.linear 0 5
  rangeSnapshotSize = Range.linear 0 1000

  genHsModule = do
    dirs <- Gen.list (Range.linear 0 10) genHsModuleName
    file <- genHsModuleName
    pure $ Text.intercalate "/" dirs <> file <> ".hs"
  genHsModuleName = Gen.text (Range.linear 0 50) $ Gen.choice [Gen.alphaNum, pure '\'']

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

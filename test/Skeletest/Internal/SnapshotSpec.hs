{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Skeletest.Internal.SnapshotSpec (spec) where

import Data.Aeson qualified as Aeson
import Data.Map qualified as Map
import Data.String (fromString)
import Data.Text (Text)
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
  let noopRank = const 0

  prop "decodeSnapshotFile . encodeSnapshotFile === pure" $ do
    (decodeSnapshotFile . encodeSnapshotFile noopRank) P.=== pure `shouldSatisfy` P.isoWith genSnapshotFile

  prop "normalizeSnapshotFile is idempotent" $ do
    file <- forAll genSnapshotFileRaw
    n <- forAll $ Gen.int (Range.linear 1 10)
    let normalizeSnapshotFile' = foldr (.) id $ replicate n normalizeSnapshotFile
    normalizeSnapshotFile' file `shouldBe` normalizeSnapshotFile file

  it "sanitizes literal ``` lines" $ do
    let roundtrip x = (decodeSnapshotFile . encodeSnapshotFile noopRank) x `shouldBe` Just x
    roundtrip $ mkSnapshot "```"
    roundtrip $ mkSnapshot "    ```"
    roundtrip $ mkSnapshot "a```b"

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

  integration . it "only updates if test passes" $ do
    runner <- getFixture @TestRunner
    runner.addTestFile "ExampleSpec.hs" $
      [ "module ExampleSpec (spec) where"
      , ""
      , "import Skeletest"
      , "import qualified Skeletest.Predicate as P"
      , ""
      , "spec = it \"fails\" $ do"
      , "  \"content\" `shouldSatisfy` P.matchesSnapshot"
      , "  failTest \"failure\""
      ]

    _ <- expectFailure $ runner.runTestsWith def{cliArgs = ["-u"]}
    runner.lookupTestFile "__snapshots__/ExampleSpec.snap.md"
      `shouldReturn` Nothing

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

    (stdout1, stderr1) <- expectCode 5 $ runner.runTestsWith def{cliArgs = []}
    stderr1 `shouldBe` ""
    stdout1 `shouldSatisfy` P.matchesSnapshot

    (stdout2, stderr2) <- expectSuccess $ runner.runTestsWith def{cliArgs = ["-u"]}
    stderr2 `shouldBe` ""
    stdout2 `shouldSatisfy` P.matchesSnapshot

    runner.readTestFile "__snapshots__/ExampleSpec.snap.md"
      `shouldReturn` "# ./ExampleSpec.hs\n\n## should error\n\n```\n\n```\n"

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

  integration . it "cleans up outdated snapshots" $ do
    runner <- getFixture @TestRunner
    -- snapshot test was deleted, file has no other snapshot tests
    runner.addTestFile "Foo/Test1Spec.hs" $
      [ "module Foo.Test1Spec (spec) where"
      , "import Skeletest"
      , "import qualified Skeletest.Predicate as P"
      , "spec = do"
      , "  it \"test other\" $ do"
      , "    pure ()"
      ]
    runner.addTestFile "Foo/__snapshots__/Test1Spec.snap.md" $
      [ "# Foo/Test1Spec.hs"
      , ""
      , "## test"
      , ""
      , "```"
      , "old snapshot"
      , "```"
      ]
    let expected1 = Nothing
    -- snapshot test was deleted, file still has other snapshots
    runner.addTestFile "Foo/Test2Spec.hs" $
      [ "module Foo.Test2Spec (spec) where"
      , "import Skeletest"
      , "import qualified Skeletest.Predicate as P"
      , "spec = do"
      , "  it \"test other\" $ do"
      , "    \"result\" `shouldSatisfy` P.matchesSnapshot"
      ]
    runner.addTestFile "Foo/__snapshots__/Test2Spec.snap.md" $
      [ "# Foo/Test2Spec.hs"
      , ""
      , "## test"
      , ""
      , "```"
      , "old snapshot"
      , "```"
      , ""
      , "## test other"
      , ""
      , "```"
      , "result"
      , "```"
      ]
    let expected2 =
          Just . Text.unlines $
            [ "# Foo/Test2Spec.hs"
            , ""
            , "## test other"
            , ""
            , "```"
            , "result"
            , "```"
            ]
    -- test still exists without snapshots
    runner.addTestFile "Foo/Test3Spec.hs" $
      [ "module Foo.Test3Spec (spec) where"
      , "import Skeletest"
      , "import qualified Skeletest.Predicate as P"
      , "spec = do"
      , "  it \"test\" $ do"
      , "    pure ()"
      , "  it \"test other\" $ do"
      , "    \"result\" `shouldSatisfy` P.matchesSnapshot"
      ]
    runner.addTestFile "Foo/__snapshots__/Test3Spec.snap.md" $
      [ "# Foo/Test3Spec.hs"
      , ""
      , "## test"
      , ""
      , "```"
      , "old snapshot"
      , "```"
      , ""
      , "## test other"
      , ""
      , "```"
      , "result"
      , "```"
      ]
    let expected3 =
          Just . Text.unlines $
            [ "# Foo/Test3Spec.hs"
            , ""
            , "## test other"
            , ""
            , "```"
            , "result"
            , "```"
            ]
    -- test still exists with fewer snapshots
    runner.addTestFile "Foo/Test4Spec.hs" $
      [ "module Foo.Test4Spec (spec) where"
      , "import Skeletest"
      , "import qualified Skeletest.Predicate as P"
      , "spec = it \"test\" $ do"
      , "  \"result\" `shouldSatisfy` P.matchesSnapshot"
      ]
    runner.addTestFile "Foo/__snapshots__/Test4Spec.snap.md" $
      [ "# Foo/Test4Spec.hs"
      , ""
      , "## test"
      , ""
      , "```"
      , "result"
      , "```"
      , ""
      , "```"
      , "old snapshot"
      , "```"
      ]
    let expected4 =
          Just . Text.unlines $
            [ "# Foo/Test4Spec.hs"
            , ""
            , "## test"
            , ""
            , "```"
            , "result"
            , "```"
            ]
    -- test file still exists, no tests with snapshots
    runner.addTestFile "Foo/Test5Spec.hs" $
      [ "module Foo.Test5Spec (spec) where"
      , "import Skeletest"
      , "import qualified Skeletest.Predicate as P"
      , "spec = do"
      , "  it \"test\" $ do"
      , "    pure ()"
      ]
    runner.addTestFile "Foo/__snapshots__/Test5Spec.snap.md" $
      [ "# Foo/Test5Spec.hs"
      , ""
      , "## test"
      , ""
      , "```"
      , "old snapshot"
      , "```"
      ]
    let expected5 = Nothing
    -- test file no longer exists
    runner.addTestFile "Foo/__snapshots__/Test6Spec.snap.md" $
      [ "# Foo/Test6Spec.hs"
      , ""
      , "## test"
      , ""
      , "```"
      , "old snapshot"
      , "```"
      ]
    let expected6 = Nothing

    (stdout1, stderr1) <- expectCode 5 $ runner.runTestsWith def
    stderr1 `shouldBe` ""
    stdout1 `shouldSatisfy` P.matchesSnapshot

    (stdout2, stderr2) <- expectSuccess $ runner.runTestsWith def{cliArgs = ["-u"]}
    stderr2 `shouldBe` ""
    stdout2 `shouldSatisfy` P.matchesSnapshot

    runner.lookupTestFile "Foo/__snapshots__/Test1Spec.snap.md"
      `shouldReturn` expected1
    runner.lookupTestFile "Foo/__snapshots__/Test2Spec.snap.md"
      `shouldReturn` expected2
    runner.lookupTestFile "Foo/__snapshots__/Test3Spec.snap.md"
      `shouldReturn` expected3
    runner.lookupTestFile "Foo/__snapshots__/Test4Spec.snap.md"
      `shouldReturn` expected4
    runner.lookupTestFile "Foo/__snapshots__/Test5Spec.snap.md"
      `shouldReturn` expected5
    runner.lookupTestFile "Foo/__snapshots__/Test6Spec.snap.md"
      `shouldReturn` expected6

  integration . it "does not clean up deselected tests" $ do
    runner <- getFixture @TestRunner
    runner.addTestFile "Test1Spec.hs" $
      [ "module Test1Spec (spec) where"
      , "import Skeletest"
      , "import qualified Skeletest.Predicate as P"
      , "spec = do"
      , "  it \"focused test\" $ do"
      , "    \"result\" `shouldSatisfy` P.matchesSnapshot"
      , "  it \"other test\" $ do"
      , "    \"result\" `shouldSatisfy` P.matchesSnapshot"
      ]
    runner.addTestFile "Test2Spec.hs" $
      [ "module Test2Spec (spec) where"
      , "import Skeletest"
      , "import qualified Skeletest.Predicate as P"
      , "spec = do"
      , "  it \"other test\" $ do"
      , "    \"result\" `shouldSatisfy` P.matchesSnapshot"
      ]
    let expected1 =
          Text.unlines
            [ "# ./Test1Spec.hs"
            , ""
            , "## focused test"
            , ""
            , "```"
            , "result"
            , "```"
            , ""
            , "## other test"
            , ""
            , "```"
            , "result"
            , "```"
            ]
    let expected2 =
          Text.unlines
            [ "# ./Test2Spec.hs"
            , ""
            , "## other test"
            , ""
            , "```"
            , "result"
            , "```"
            ]

    _ <- expectSuccess $ runner.runTestsWith def{cliArgs = ["-u"]}
    runner.readTestFile "__snapshots__/Test1Spec.snap.md"
      `shouldReturn` expected1
    runner.readTestFile "__snapshots__/Test2Spec.snap.md"
      `shouldReturn` expected2

    _ <- expectSuccess $ runner.runTestsWith def{cliArgs = ["[focused test]", "-u"]}
    runner.readTestFile "__snapshots__/Test1Spec.snap.md"
      `shouldReturn` expected1
    runner.readTestFile "__snapshots__/Test2Spec.snap.md"
      `shouldReturn` expected2

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
    content <- Gen.text rangeSnapshotSize Gen.unicode
    lang <- Gen.maybe $ Gen.text (Range.linear 1 5) Gen.unicode
    pure SnapshotValue{..}

genSnapshotFile :: Gen SnapshotFile
genSnapshotFile = normalizeSnapshotFile <$> genSnapshotFileRaw

mkSnapshot :: Text -> SnapshotFile
mkSnapshot content =
  normalizeSnapshotFile
    SnapshotFile
      { testFile = "FooSpec.hs"
      , snapshots =
          Map.singleton ["test"] . (: []) $
            SnapshotValue
              { content
              , lang = Nothing
              }
      }

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE NoFieldSelectors #-}

module Skeletest.Internal.CLI (
  Flag (..),
  flag,
  IsFlag (..),
  FlagSpec (..),
  FlagType (..),
  getFlag,
  loadCliArgs,

  -- * General flags
  ANSIFlag (..),

  -- * Internal
  parseCliArgsWith,
  FlagInfos,
  SomeFlagSpec (..),
  CLIParseResult (..),
) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bifunctor (first)
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.Foldable (foldlM)
import Data.Foldable qualified as Seq (toList)
import Data.Foldable1 qualified as Foldable1
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Typeable (TypeRep, Typeable, typeOf, typeRep)
import Skeletest.Internal.Error (invariantViolation, skeletestError)
import Skeletest.Internal.Exit (TestExitCode (..), exitWith)
import Skeletest.Internal.TestTargets (TestTargets, parseTestTargets)
import Skeletest.Internal.Utils.Color qualified as Color
import Skeletest.Internal.Utils.Term qualified as Term
import System.Environment (getArgs)
import System.IO.Unsafe (unsafePerformIO)

#if !MIN_VERSION_base(4, 20, 0)
import Data.Foldable (foldl')
#endif

-- | Register a CLI flag.
--
-- Usage:
--
-- @
-- {- MyFixture.hs -}
-- import Skeletest
--
-- newtype MyFlag = MyFlag String
-- instance IsFlag MyFlag where
--   flagName = "my-flag"
--   flagHelp = "The value for MyFixture"
--   flagSpec =
--     OptionalFlag
--       { flagDefault = "foo"
--       , flagParse = \case
--           "illegal" -> Left "invalid flag value"
--           s -> Right (MyFlag s)
--       }
--
-- instance Fixture MyFixture where
--   fixtureAction = do
--     MyFlag val <- getFlag
--     ...
--
-- {- Main.hs -}
-- import MyFixture
--
-- cliFlags =
--   [ flag @MyFlag
--   ]
-- @
data Flag = forall a. (IsFlag a) => Flag (Proxy a)

flag :: forall a. (IsFlag a) => Flag
flag = Flag (Proxy @a)

class (Typeable a) => IsFlag a where
  flagName :: String

  flagShort :: Maybe Char
  flagShort = Nothing

  -- | The placeholder for the flag to show in the help text, if
  -- the flag takes an argument.
  flagMetaVar :: String
  flagMetaVar = "VAR"

  flagHelp :: String

  flagSpec :: FlagSpec a

-- TODO: Remove 'flag' prefix from these fields
-- https://github.com/brandonchinn178/skeletest/issues/89
data FlagSpec a
  = SwitchFlag
      { flagFromBool :: Bool -> a
      }
  | RequiredFlag
      { flagParse :: String -> Either String a
      }
  | OptionalFlag
      { flagDefault :: a
      , flagParse :: String -> Either String a
      }
  | forall x.
    MultiFlag
      { type_ :: FlagType x
      , parseMulti :: [x] -> Either String a
      }

data FlagType x where
  FlagType_Switch :: FlagType Bool
  FlagType_Arg :: FlagType String

getFlag :: forall a m. (MonadIO m, IsFlag a) => m a
getFlag =
  liftIO $
    lookupCliFlag rep >>= \case
      Just dyn ->
        case fromDynamic dyn of
          Just a -> pure a
          Nothing ->
            invariantViolation . unwords $
              [ "CLI flag store contained incorrect types."
              , "Expected: " <> show rep <> "."
              , "Got: " <> show dyn
              ]
      Nothing ->
        skeletestError . Text.unwords $
          [ "CLI flag '" <> Text.pack (flagName @a) <> "' was not registered."
          , "Did you add it to cliFlags in Main.hs?"
          ]
 where
  rep = typeRep (Proxy @a)

{----- General flags -----}

newtype ANSIFlag = ANSIFlag (Maybe Bool)
instance IsFlag ANSIFlag where
  flagName = "ansi"
  flagHelp = "Whether to enable ANSI output: auto (default), always, never"
  flagSpec =
    OptionalFlag
      { flagDefault = ANSIFlag Nothing
      , flagParse = \case
          "auto" -> Right . ANSIFlag $ Nothing
          "always" -> Right . ANSIFlag $ Just True
          "never" -> Right . ANSIFlag $ Just False
          s -> Left $ "invalid value: " <> s
      }

{----- Load CLI arguments -----}

-- | Parse the CLI arguments using the given user-defined flags, then
-- stores the flags in the global state and returns the positional
-- arguments.
loadCliArgs :: [Flag] -> [Flag] -> IO TestTargets
loadCliArgs builtinFlags flags = do
  args0 <- getArgs
  case parseCliArgs (builtinFlags <> flags) args0 of
    CLISetupFailure msg -> do
      Term.outputErr $ Color.red $ "ERROR: " <> msg
      exitWith ExitCLIFailure
    CLIHelpRequested -> do
      Term.output helpText
      exitWith ExitSuccess
    CLIParseFailure msg -> do
      Term.outputErr $ msg <> "\n\n" <> helpText
      exitWith ExitCLIFailure
    CLIParseSuccess{testTargets, flagStore} -> do
      setCliFlagStore flagStore
      pure testTargets
 where
  helpText = getHelpText builtinFlags flags

getHelpText :: [Flag] -> [Flag] -> Text
getHelpText builtinFlags customFlags =
  Text.intercalate "\n\n" $
    "Usage: skeletest [OPTIONS] [--] [TARGETS]" : map (uncurry renderSection) helpSections
 where
  helpSections =
    filter (not . Text.null . snd) $
      [ ("TEST SELECTION", testSelectionDocs)
      , ("BUILTIN OPTIONS", renderFlagList builtinFlagDocs)
      , ("CUSTOM OPTIONS", renderFlagList customFlagDocs)
      ]

  testSelectionDocs =
    Text.intercalate "\n" $
      [ "Test targets may be specified as plain positional arguments, with the following syntax:"
      , "    * Tests including substring:      '[myFooFunc]'"
      , "    * Tests tagged with marker:       '@fast'"
      , "    * Tests in file, relative to CWD: 'test/MyLib/FooSpec.hs'"
      , "    * Tests matching pattern in file: 'test/MyLib/FooSpec.hs[myFooFunc]'"
      , "        * Syntax sugar for '(test/MyLib/FooSpec.hs and [myFooFunc])'"
      , "    * Tests matching both targets:    '[func1] and [func2]'"
      , "    * Tests matching either target:   '[func1] or [func2]'"
      , "    * Tests not matching target:      'not [func1]'"
      , ""
      , "More examples:"
      , "    * 'test/MySpec.hs and ([myFooFunc] or [myBarFunc]) and @fast'"
      , "    * '[myFooFunc] or test/MySpec.hs[myBarFunc]'"
      , ""
      , "When multiple targets are specified, they are joined with 'or'."
      ]

  builtinFlagDocs = ("help", Just 'h', Nothing, "Display this help text") : fromFlags builtinFlags
  customFlagDocs = fromFlags customFlags
  fromFlags flags =
    [ (Text.pack (flagName @a), flagShort @a, mMetaVar, Text.pack (flagHelp @a))
    | Flag (Proxy :: Proxy a) <- flags
    , let mMetaVar =
            case flagSpec @a of
              SwitchFlag{} -> Nothing
              RequiredFlag{} -> Just $ Text.pack (flagMetaVar @a)
              OptionalFlag{} -> Just $ Text.pack (flagMetaVar @a)
              MultiFlag{} -> Just $ Text.pack (flagMetaVar @a)
    ]

  renderSection title body =
    Text.intercalate "\n" $
      [ "===== " <> title
      , ""
      , body
      ]

  renderFlagList flagList =
    Text.intercalate "\n" . mkTabular $
      [ (shortName <> renderLongFlag longName <> metaVar, help)
      | (longName, mShortName, mMetaVar, help) <- flagList
      , let
          shortName =
            case mShortName of
              Just short -> renderShortFlag short <> ", "
              Nothing -> ""
          metaVar =
            case mMetaVar of
              Just meta -> " <" <> meta <> ">"
              Nothing -> ""
      ]

  mkTabular rows0 =
    case NonEmpty.nonEmpty rows0 of
      Nothing -> []
      Just rows ->
        let fstColWidth = Foldable1.maximum $ NonEmpty.map (Text.length . fst) rows
            margin = 2 -- space between columns
         in [ a <> Text.replicate (fstColWidth - Text.length a + margin) " " <> b
            | (a, b) <- NonEmpty.toList rows
            ]

{----- Parse args -----}

data CLIParseResult
  = CLISetupFailure Text
  | CLIHelpRequested
  | CLIParseFailure Text
  | CLIParseSuccess
      { testTargets :: TestTargets
      , flagStore :: CLIFlagStore
      }

type FlagInfos = [(Text, Maybe Char, SomeFlagSpec)]
data SomeFlagSpec = forall a. (Typeable a) => SomeFlagSpec (FlagSpec a)

parseCliArgs :: [Flag] -> [String] -> CLIParseResult
parseCliArgs flags args = either id id $ do
  flagInfos <- getFlagInfos flags
  pure $ parseCliArgsWith flagInfos args

getFlagInfos :: [Flag] -> Either CLIParseResult FlagInfos
getFlagInfos flags = do
  let infos = map fromFlag flags
  checkDups [renderLongFlag name | (name, _, _) <- infos]
  checkDups [renderShortFlag c | (_, Just c, _) <- infos]
  pure infos
 where
  fromFlag (Flag (Proxy @a)) =
    let name = Text.pack $ flagName @a
        spec = SomeFlagSpec (flagSpec @a)
     in (name, flagShort @a, spec)

  checkDups vals =
    case Map.keys . Map.filter (> 1) . Map.fromListWith (+) . map (,1 :: Int) $ vals of
      [] -> pure ()
      dup : _ -> Left . CLISetupFailure $ "Flag registered multiple times: " <> dup

parseCliArgsWith :: FlagInfos -> [String] -> CLIParseResult
parseCliArgsWith flagInfos args = either id id $ do
  -- quick sweep for --help/-h; skip parsing flags if so
  when (any (`elem` ["--help", "-h"]) args) $ Left CLIHelpRequested

  (flagVals, args') <- collectCLIArgs flagInfos $ map Text.pack args
  testTargets <- first CLIParseFailure $ parseTestTargets args'
  flagStore <- parseCLIFlags flagInfos flagVals
  pure CLIParseSuccess{testTargets, flagStore}

collectCLIArgs ::
  FlagInfos ->
  [Text] ->
  Either CLIParseResult (Map Text [Text], [Text])
collectCLIArgs flagInfos args0 = first CLIParseFailure $ do
  (flagVals, posArgs) <- go Map.empty Seq.empty args0
  pure (Map.map Seq.toList flagVals, Seq.toList posArgs)
 where
  go :: Map Text (Seq Text) -> Seq Text -> [Text] -> Either Text (Map Text (Seq Text), Seq Text)
  go flagVals posArgs = \case
    [] -> Right (flagVals, posArgs)
    "--" : rest -> Right (flagVals, posArgs <> Seq.fromList rest)
    curr : rest
      | Just rawFlag <- Text.stripPrefix "--" curr -> do
          (name, arg, rest') <- parseLongFlag rawFlag rest
          go (addFlag flagVals (name, arg)) posArgs rest'
      | Just rawFlag <- Text.stripPrefix "-" curr -> do
          (args, rest') <- parseShortFlags rawFlag rest
          let flagVals' = foldl' addFlag flagVals args
          go flagVals' posArgs rest'
      | otherwise -> do
          go flagVals (posArgs Seq.|> curr) rest

  longFlags = Map.fromList [(name, spec) | (name, _, spec) <- flagInfos]
  parseLongFlag rawFlag rest = do
    let (name, mArg) =
          case Text.breakOn "=" rawFlag of
            (_, "") -> (rawFlag, Nothing)
            (n, post) -> (n, Just $ Text.drop 1 post)
        flagDisp = renderLongFlag name
    SomeFlagSpec spec <-
      maybe (Left $ "Unknown flag: " <> flagDisp) pure $
        Map.lookup name longFlags
    (arg, rest') <- validateArg spec flagDisp mArg rest
    pure (name, arg, rest')

  shortFlags = Map.fromList [(c, (name, spec)) | (name, Just c, spec) <- flagInfos]
  parseShortFlags rawFlag rest = do
    (char, rawFlag') <-
      maybe (Left "Invalid flag: -") pure $
        Text.uncons rawFlag
    let flagDisp = renderShortFlag char
    (name, SomeFlagSpec spec) <-
      maybe (Left $ "Unknown flag: " <> flagDisp) pure $
        Map.lookup char shortFlags
    let mArg =
          if (not . Text.null) rawFlag' && expectsArg spec
            then Just rawFlag'
            else Nothing
        parseNext =
          if (not . Text.null) rawFlag' && (not . expectsArg) spec
            then parseShortFlags rawFlag'
            else \rest' -> pure ([], rest')
    (arg, rest') <- validateArg spec flagDisp mArg rest
    (args, rest'') <- parseNext rest'
    pure ((name, arg) : args, rest'')

  validateArg spec flagDisp mArg rest = do
    if expectsArg spec
      then case (mArg, rest) of
        (Just arg, _) -> pure (arg, rest)
        (Nothing, arg : rest') -> pure (arg, rest')
        (Nothing, []) -> Left $ "Flag '" <> flagDisp <> "' requires argument"
      else case (mArg, rest) of
        (Just arg, _) -> Left $ "Flag '" <> flagDisp <> "' does not take arguments, got: " <> arg
        _ -> pure ("", rest)

  expectsArg :: FlagSpec a -> Bool
  expectsArg = \case
    SwitchFlag{} -> False
    RequiredFlag{} -> True
    OptionalFlag{} -> True
    MultiFlag{type_} ->
      case type_ of
        FlagType_Switch -> False
        FlagType_Arg -> True

  addFlag :: Map Text (Seq Text) -> (Text, Text) -> Map Text (Seq Text)
  addFlag flagVals (name, arg) =
    Map.alter
      (Just . (Seq.|> arg) . fromMaybe Seq.empty)
      name
      flagVals

parseCLIFlags :: FlagInfos -> Map Text [Text] -> Either CLIParseResult CLIFlagStore
parseCLIFlags flagInfos flagVals = first CLIParseFailure $ foldlM go Map.empty flagInfos
 where
  go flagStore (name, _, SomeFlagSpec spec0) = do
    let vals = Map.findWithDefault [] name flagVals
    val <- first Text.pack . parse name spec0 . map Text.unpack $ vals
    pure $ insertFlagStore val flagStore

  parse :: Text -> FlagSpec a -> [String] -> Either String a
  parse name spec0 vals =
    case spec0 of
      spec@SwitchFlag{} -> do
        pure (spec.flagFromBool $ (not . null) vals)
      spec@RequiredFlag{} -> do
        val <- maybe (throwRequired name) pure $ getLast vals
        spec.flagParse val
      spec@OptionalFlag{} -> do
        case getLast vals of
          Nothing -> pure spec.flagDefault
          Just val -> spec.flagParse val
      MultiFlag{type_, parseMulti} -> do
        parseMulti $
          case type_ of
            FlagType_Switch -> True <$ vals
            FlagType_Arg -> vals

  throwRequired name = Left $ "Flag '" <> (Text.unpack . renderLongFlag) name <> "' is required"
  getLast = fmap NonEmpty.last . NonEmpty.nonEmpty

renderLongFlag :: Text -> Text
renderLongFlag = ("--" <>)

renderShortFlag :: Char -> Text
renderShortFlag c = Text.pack ['-', c]

{----- CLIFlagStore -----}

type CLIFlagStore = Map TypeRep Dynamic

insertFlagStore :: (Typeable a) => a -> CLIFlagStore -> CLIFlagStore
insertFlagStore x = Map.insert (typeOf x) (toDyn x)

cliFlagStoreRef :: IORef CLIFlagStore
cliFlagStoreRef = unsafePerformIO $ newIORef Map.empty
{-# NOINLINE cliFlagStoreRef #-}

setCliFlagStore :: CLIFlagStore -> IO ()
setCliFlagStore = writeIORef cliFlagStoreRef

lookupCliFlag :: TypeRep -> IO (Maybe Dynamic)
lookupCliFlag rep = Map.lookup rep <$> readIORef cliFlagStoreRef

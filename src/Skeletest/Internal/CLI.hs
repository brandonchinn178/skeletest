{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE NoFieldSelectors #-}

module Skeletest.Internal.CLI (
  Flag (..),
  flag,
  IsFlag (..),
  FlagSpec (..),
  getFlag,
  loadCliArgs,

  -- * Internal
  parseCliArgs,
  CLIParseResult (..),
  CLIFlagStore,
) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bifunctor (first)
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.Foldable (foldlM)
import Data.Foldable qualified as Seq (toList)
import Data.Foldable1 qualified as Foldable1
import Data.Function (on)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Typeable (TypeRep, Typeable, typeOf, typeRep)
import Skeletest.Internal.Error (SkeletestError (..), invariantViolation)
import Skeletest.Internal.TestTargets (TestTargets, parseTestTargets)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (stderr)
import System.IO.Unsafe (unsafePerformIO)
import UnliftIO.Exception (throwIO)

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

instance Eq Flag where
  (==) = (==) `on` (\(Flag proxy) -> typeRep proxy)
instance Ord Flag where
  compare = compare `on` (\(Flag proxy) -> typeRep proxy)

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

-- TODO(breaking-change): Remove 'flag' prefix from these fields
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
      Nothing -> throwIO $ CliFlagNotFound (Text.pack $ flagName @a)
 where
  rep = typeRep (Proxy @a)

{----- Load CLI arguments -----}

-- | Parse the CLI arguments using the given user-defined flags, then
-- stores the flags in the global state and returns the positional
-- arguments.
loadCliArgs :: [Flag] -> [Flag] -> IO TestTargets
loadCliArgs builtinFlags flags = do
  args0 <- getArgs
  case parseCliArgs (builtinFlags <> flags) args0 of
    CLISetupFailure msg -> do
      Text.hPutStrLn stderr $ "ERROR: " <> msg
      exitFailure
    CLIHelpRequested -> do
      Text.putStrLn helpText
      exitSuccess
    CLIParseFailure msg -> do
      Text.hPutStrLn stderr $ msg <> "\n\n" <> helpText
      exitFailure
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

parseCliArgs :: [Flag] -> [String] -> CLIParseResult
parseCliArgs flags args = either id id $ do
  longFlags <- extractLongFlags
  shortFlags <- extractShortFlags

  -- quick sweep for --help/-h after flag validation; skip parsing flags if so
  when (any (`elem` ["--help", "-h"]) args) $ Left CLIHelpRequested

  (rawFlags, args') <- collectCLIArgs longFlags shortFlags $ map Text.pack args
  testTargets <- first CLIParseFailure $ parseTestTargets args'
  flagStore <- parseCLIFlags flags rawFlags
  pure CLIParseSuccess{testTargets, flagStore}
 where
  extractLongFlags =
    toFlagMap renderLongFlag $
      [ (Text.pack $ flagName @a, f)
      | f@(Flag (Proxy :: Proxy a)) <- flags
      ]

  extractShortFlags =
    toFlagMap renderShortFlag $
      [ (shortFlag, f)
      | f@(Flag (Proxy :: Proxy a)) <- flags
      , Just shortFlag <- pure $ flagShort @a
      ]

  toFlagMap :: (Ord name) => (name -> Text) -> [(name, a)] -> Either CLIParseResult (Map name a)
  toFlagMap renderFlag vals =
    let go seen = \case
          [] -> Right $ Map.fromList vals
          (name, _) : xs
            | name `Set.member` seen -> Left . CLISetupFailure $ "Flag registered multiple times: " <> renderFlag name
            | otherwise -> go (Set.insert name seen) xs
     in go Set.empty vals

collectCLIArgs ::
  Map Text Flag ->
  Map Char Flag ->
  [Text] ->
  Either CLIParseResult (Map Flag [Text], [Text])
collectCLIArgs longFlags shortFlags args0 = first CLIParseFailure $ do
  (flagMap, posArgs) <- go Map.empty Seq.empty args0
  pure (Map.map Seq.toList flagMap, Seq.toList posArgs)
 where
  go :: Map Flag (Seq Text) -> Seq Text -> [Text] -> Either Text (Map Flag (Seq Text), Seq Text)
  go flagMap posArgs = \case
    [] -> Right (flagMap, posArgs)
    "--" : rest -> Right (flagMap, posArgs <> Seq.fromList rest)
    curr : rest
      | Just name0 <- Text.stripPrefix "--" curr -> do
          let (name, mArg) =
                case Text.breakOn "=" name0 of
                  (_, "") -> (name0, Nothing)
                  (n, post) -> (n, Just $ Text.drop 1 post)
          flag_ <- lookupFlag renderLongFlag longFlags name
          (arg, rest') <- validateArg flag_ (renderLongFlag name) mArg rest
          go (addFlag flagMap flag_ arg) posArgs rest'
      | Just chars <- Text.stripPrefix "-" curr -> do
          char <-
            case Text.unpack chars of
              [c] -> pure c
              _ -> Left $ "Invalid flag: -" <> chars
          flag_ <- lookupFlag renderShortFlag shortFlags char
          (arg, rest') <- validateArg flag_ (renderShortFlag char) Nothing rest
          go (addFlag flagMap flag_ arg) posArgs rest'
      | otherwise -> do
          go flagMap (posArgs Seq.|> curr) rest

  lookupFlag :: (Ord name) => (name -> Text) -> Map name Flag -> name -> Either Text Flag
  lookupFlag renderFlag flags name =
    case Map.lookup name flags of
      Nothing -> Left $ "Unknown flag: " <> renderFlag name
      Just f -> pure f

  validateArg :: Flag -> Text -> Maybe Text -> [Text] -> Either Text (Text, [Text])
  validateArg (Flag (Proxy @a)) name mArg rest = do
    let expectsArg =
          case flagSpec @a of
            SwitchFlag{} -> False
            RequiredFlag{} -> True
            OptionalFlag{} -> True
    if expectsArg
      then case (mArg, rest) of
        (Just arg, _) -> pure (arg, rest)
        (Nothing, arg : rest') -> pure (arg, rest')
        (Nothing, []) -> Left $ "Flag '" <> name <> "' requires argument"
      else case (mArg, rest) of
        (Just arg, _) -> Left $ "Flag '" <> name <> "' does not take arguments, got: " <> arg
        _ -> pure ("", rest)

  addFlag :: Map Flag (Seq Text) -> Flag -> Text -> Map Flag (Seq Text)
  addFlag flagMap flag_ arg =
    Map.alter
      (Just . (Seq.|> arg) . fromMaybe Seq.empty)
      flag_
      flagMap

parseCLIFlags :: [Flag] -> Map Flag [Text] -> Either CLIParseResult CLIFlagStore
parseCLIFlags flags flagMap = first CLIParseFailure $ foldlM go Map.empty flags
 where
  go flagStore flag_@(Flag (Proxy @a)) = do
    let vals = Map.findWithDefault [] flag_ flagMap
        name = renderLongFlag . Text.pack $ flagName @a
        parseWith f = first Text.pack . f . Text.unpack
    val <-
      case flagSpec @a of
        spec@SwitchFlag{} ->
          pure (spec.flagFromBool $ (not . null) vals)
        spec@RequiredFlag{} ->
          case getLast vals of
            Nothing -> Left $ "Flag '" <> name <> "' is required"
            Just val -> parseWith spec.flagParse val
        spec@OptionalFlag{} ->
          case getLast vals of
            Nothing -> pure spec.flagDefault
            Just val -> parseWith spec.flagParse val
    pure $ insertFlagStore val flagStore

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

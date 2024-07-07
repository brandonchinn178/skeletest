{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedStrings #-}

module Skeletest.Internal.CLI (
  Flag (..),
  flag,
  IsFlag (..),
  FlagSpec (..),
  getFlag,
  loadCliArgs,

  -- * Internal
  parseCliArgs,
) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class qualified as Trans
import Control.Monad.Trans.Except qualified as Trans
import Control.Monad.Trans.State qualified as Trans
import Data.Bifunctor (first, second)
import Data.Dynamic (fromDynamic, toDyn)
import Data.Foldable1 qualified as Foldable1
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Proxy (Proxy (..))
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Typeable (Typeable, typeOf, typeRep)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (stderr)

import Skeletest.Internal.Error (invariantViolation)
import Skeletest.Internal.State (CLIFlagStore, lookupCliFlag, setCliFlagStore)

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
data Flag = forall a. IsFlag a => Flag (Proxy a)

flag :: forall a. IsFlag a => Flag
flag = Flag (Proxy @a)

class Typeable a => IsFlag a where
  flagName :: String

  flagShort :: Maybe Char
  flagShort = Nothing

  -- | The placeholder for the flag to show in the help text, if
  -- the flag takes an argument.
  flagMetaVar :: String
  flagMetaVar = "VAR"

  flagHelp :: String

  flagSpec :: FlagSpec a

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
      -- TODO: better error
      Nothing -> error "Flag was not registered. Did you add it to cliFlags in Main.hs?"
  where
    rep = typeRep (Proxy @a)

{----- Load CLI arguments -----}

-- | Parse the CLI arguments using the given user-defined flags, then
-- stores the flags in the global state and returns the positional
-- arguments.
loadCliArgs :: [Flag] -> [Flag] -> IO [Text]
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
    CLIParseSuccess{args, flagStore} -> do
      setCliFlagStore flagStore
      pure args
  where
    helpText = getHelpText builtinFlags flags

getHelpText :: [Flag] -> [Flag] -> Text
getHelpText builtinFlags customFlags =
  -- TODO: rewrap to terminal width
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
        , "    * Tests in file, relative to CWD:     'test/MyLib/FooSpec.hs'"
        , "    * Tests matching pattern in file:     'test/MyLib/FooSpec.hs:{myFooFunc}'"
        , "    * Tests matching pattern in any file: '{myFooFunc}'"
        , "    * Tests matching both targets:        '{func1} and {func2}'"
        , "    * Tests matching either target:       '{func1} or {func2}'"
        , "    * Tests not matching target:          'not {func1}"
        , ""
        , "When multiple targets are specified, they are joined with 'or'."
        , ""
        , "Tests can also be tagged with markers, which can be specified with the following syntax:"
        , "    * Tests with given marker:  --marker 'nightly'"
        , "    * Tests without marker:     --marker 'not nightly'"
        , "    * Tests with both markers:  --marker 'nightly and fast'"
        , "    * Tests with either marker: --marker 'not nightly or fast'"
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
  | CLIParseSuccess { args :: [Text], flagStore :: CLIFlagStore }

parseCliArgs :: [Flag] -> [String] -> CLIParseResult
parseCliArgs flags args = either id id $ do
  longFlags <- extractLongFlags
  shortFlags <- extractShortFlags

  -- quick sweep for --help/-h after flag validation; skip parsing flags if so
  when (any (`elem` ["--help", "-h"]) args) $ Left CLIHelpRequested

  (args', flagStore) <- first CLIParseFailure $ parseCliArgsWith longFlags shortFlags args
  flagStore' <- first CLIParseFailure $ resolveFlags flags flagStore
  pure CLIParseSuccess{args = args', flagStore = flagStore'}
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

    toFlagMap :: Ord name => (name -> Text) -> [(name, a)] -> Either CLIParseResult (Map name a)
    toFlagMap renderFlag vals =
      let go seen = \case
            [] -> Right $ Map.fromList vals
            (name, _) : xs
              | name `Set.member` seen -> Left . CLISetupFailure $ "Flag registered multiple times: " <> renderFlag name
              | otherwise -> go (Set.insert name seen) xs
       in go Set.empty vals

type ArgParserM = Trans.StateT ([Text], CLIFlagStore) (Trans.Except Text)

parseCliArgsWith :: Map Text Flag -> Map Char Flag -> [String] -> Either Text ([Text], CLIFlagStore)
parseCliArgsWith longFlags shortFlags = Trans.runExcept . flip Trans.execStateT ([], Map.empty) . parseArgs
  where
    parseArgs = \case
      [] -> pure ()
      "--" : rest -> addArgs rest
      curr : rest
        | Just longFlag <- Text.stripPrefix "--" (Text.pack curr) -> parseLongFlag longFlag rest
        | Just chars <- Text.stripPrefix "-" (Text.pack curr) ->
            case Text.unpack chars of
              [] -> argError "Invalid flag: -"
              [shortFlag] -> parseShortFlag shortFlag rest
              -- TODO: allow multiple short flags at once?
              _ -> argError $ "Invalid flag: -" <> chars
        | otherwise -> addArgs [curr] >> parseArgs rest

    parseLongFlag = parseFlag renderLongFlag longFlags
    parseShortFlag = parseFlag renderShortFlag shortFlags

    parseFlag :: Ord name => (name -> Text) -> Map name Flag -> name -> [String] -> ArgParserM ()
    parseFlag renderFlag flagMap name args = do
      Flag (Proxy :: Proxy a) <-
        case Map.lookup name flagMap of
          Nothing -> argError $ "Unknown flag: " <> renderFlag name
          Just f -> pure f
      let parseFlagArg parseArg =
            case args of
              [] -> argError $ "Flag requires argument: " <> renderFlag name
              curr : rest -> parseArg curr >>= addFlagStore >> parseArgs rest
      case flagSpec @a of
        SwitchFlag{flagFromBool} -> addFlagStore (flagFromBool True) >> parseArgs args
        RequiredFlag{flagParse} -> parseFlagArg (Trans.lift . Trans.except . first Text.pack . flagParse)
        OptionalFlag{flagParse} -> parseFlagArg (Trans.lift . Trans.except . first Text.pack . flagParse)

    argError = Trans.lift . Trans.throwE

    addArgs :: [String] -> ArgParserM ()
    addArgs args = Trans.modify (first (<> map Text.pack args))

    addFlagStore :: Typeable a => a -> ArgParserM ()
    addFlagStore x = Trans.modify (second (insertFlagStore x))

resolveFlags :: [Flag] -> CLIFlagStore -> Either Text CLIFlagStore
resolveFlags = flip (foldlM go)
  where
    go flagStore (Flag (Proxy :: Proxy a)) = do
      let rep = typeRep (Proxy @a)
      case flagSpec @a of
        SwitchFlag{flagFromBool} ->
          pure $
            if rep `Map.member` flagStore
              then flagStore
              else insertFlagStore (flagFromBool False) flagStore
        RequiredFlag{} ->
          if rep `Map.member` flagStore
            then pure flagStore
            else Left $ "Required flag not set: " <> renderLongFlag (Text.pack $ flagName @a)
        OptionalFlag{flagDefault} ->
          pure $
            if rep `Map.member` flagStore
              then flagStore
              else insertFlagStore flagDefault flagStore

    foldlM f z = \case
      [] -> pure z
      x : xs -> do
        z' <- f z x
        foldlM f z' xs

insertFlagStore :: Typeable a => a -> CLIFlagStore -> CLIFlagStore
insertFlagStore x = Map.insert (typeOf x) (toDyn x)

renderLongFlag :: Text -> Text
renderLongFlag = ("--" <>)

renderShortFlag :: Char -> Text
renderShortFlag c = Text.pack ['-', c]

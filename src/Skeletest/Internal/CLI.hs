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
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Proxy (Proxy (..))
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Typeable (Typeable, typeOf, typeRep)
import System.Environment (getArgs)
import System.Exit (exitSuccess)

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

  flagHelp :: String

  flagSpec :: FlagSpec a

data FlagSpec a
  = SwitchFlag
      { flagFromBool :: Bool -> a
      }
  | RequiredFlag
      { flagParse :: String -> Either Text a
      }
  | OptionalFlag
      { flagDefault :: a
      , flagParse :: String -> Either Text a
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

-- | Parse the CLI arguments using the given user-defined flags, then
-- stores the flags in the global state and returns the positional
-- arguments.
loadCliArgs :: [Flag] -> IO [Text]
loadCliArgs flags = do
  args <- getArgs

  -- quick sweep for --help/-h; skip parsing flags if so
  when (any (`elem` ["--help", "-h"]) args) $ do
    printHelpText flags
    exitSuccess

  (args', flagStore) <- either (error . Text.unpack) pure $ parseCliArgs flags args
  setCliFlagStore flagStore
  pure args'

printHelpText :: [Flag] -> IO ()
printHelpText _ = error "TODO: --help"

{----- Parse args -----}

parseCliArgs :: [Flag] -> [String] -> Either Text ([Text], CLIFlagStore)
parseCliArgs flags args = do
  longFlags <- extractLongFlags
  shortFlags <- extractShortFlags
  (args', flagStore) <- parseCliArgsWith longFlags shortFlags args
  flagStore' <- resolveFlags flags flagStore
  pure (args', flagStore')
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

    toFlagMap :: Ord name => (name -> Text) -> [(name, a)] -> Either Text (Map name a)
    toFlagMap renderFlag vals =
      let go seen = \case
            [] -> Right $ Map.fromList vals
            (name, _) : xs
              | name `Set.member` seen -> Left $ "Flag registered multiple times: " <> renderFlag name
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
        RequiredFlag{flagParse} -> parseFlagArg (Trans.lift . Trans.except . flagParse)
        OptionalFlag{flagParse} -> parseFlagArg (Trans.lift . Trans.except . flagParse)

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

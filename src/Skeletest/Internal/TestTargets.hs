{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Skeletest.Internal.TestTargets (
  TestTargets,
  TestTarget (..),
  TestAttrs (..),
  matchesTest,
  parseTestTargets,
) where

import Control.Monad.Combinators.Expr qualified as Parser
import Data.Bifunctor (first)
import Data.Char (isAlphaNum)
import Data.Foldable1 qualified as Foldable1
import Data.Text (Text)
import Data.Text qualified as Text
import Data.List.NonEmpty qualified as NonEmpty
import Data.Void (Void)
import Text.Megaparsec (Parsec)
import Text.Megaparsec qualified as Parser
import Text.Megaparsec.Char qualified as Parser
import Text.Megaparsec.Char.Lexer qualified as Parser.L

type TestTargets = Maybe TestTarget

data TestTarget
  = TestTargetFile FilePath
  | TestTargetName Text
  | TestTargetMarker Text
  | TestTargetNot TestTarget
  | TestTargetAnd TestTarget TestTarget
  | TestTargetOr TestTarget TestTarget
  deriving (Eq)

data TestAttrs = TestAttrs
  { testPath :: FilePath
  , testIdentifier :: [Text]
  , testMarkers :: [Text]
  }

matchesTest :: TestTarget -> TestAttrs -> Bool
matchesTest selection TestAttrs{..} = go selection
  where
    go = \case
      TestTargetFile path -> testPath == path
      TestTargetName s -> s `Text.isInfixOf` Text.unwords testIdentifier
      TestTargetMarker marker -> marker `elem` testMarkers
      TestTargetNot e -> not $ go e
      TestTargetAnd l r -> go l && go r
      TestTargetOr l r -> go l || go r

{----- Parsing -----}

parseTestTargets :: [Text] -> Either Text TestTargets
parseTestTargets args =
  case NonEmpty.nonEmpty args of
    Nothing -> pure Nothing
    Just args' -> Just . Foldable1.foldr1 TestTargetOr <$> mapM parseTestTarget args'
  where
    parseTestTarget = first showTestTargetParseError . Parser.parse (testTargetParser <* Parser.eof) ""

type Parser = Parsec Void Text
type ParseErrorBundle = Parser.ParseErrorBundle Text Void

testTargetParser :: Parser TestTarget
testTargetParser =
  Parser.makeExprParser
    ( Parser.choice
        [ parens testTargetParser
        , nameParser
        , markerParser
        , do
            selectFile <- fileParser
            -- syntax sugar: FooSpec.hs[abc] == (FooSpec.hs and [abc])
            withName <- maybe id (flip TestTargetAnd) <$> Parser.optional nameParser
            pure $ withName selectFile
        ]
    )
    [ [prefix "not" TestTargetNot]
    , [binary "and" TestTargetAnd, binary "or" TestTargetOr]
    ]
  where
    prefix name f = Parser.Prefix (f <$ symbol name)
    binary name f = Parser.InfixL (f <$ symbol name)

    symbol = Parser.L.symbol Parser.space
    parens = Parser.between (symbol "(") (symbol ")")

    nameParser :: Parser TestTarget
    nameParser =
      Parser.label "test name" $
        fmap TestTargetName . Parser.between (symbol "[") (symbol "]") $
          Parser.takeWhile1P Nothing (/= ']')

    markerParser :: Parser TestTarget
    markerParser =
      Parser.label "marker" $ do
        _ <- symbol "@"
        fmap TestTargetMarker . Parser.takeWhile1P Nothing $
          (||) <$> isAlphaNum <*> (`elem` ("-_." :: [Char]))

    fileParser :: Parser TestTarget
    fileParser =
      Parser.label "test file" $
        fmap (TestTargetFile . Text.unpack) . Parser.takeWhile1P Nothing $
          (||) <$> isAlphaNum <*> (`elem` ("-_./" :: [Char]))

showTestTargetParseError :: ParseErrorBundle -> Text
showTestTargetParseError bundle =
  let
    line = Parser.pstateInput $ Parser.bundlePosState bundle
    err = NonEmpty.head $ Parser.bundleErrors bundle
    pointerLen =
      case err of
        Parser.TrivialError _ (Just (Parser.Tokens s)) _ -> length s
        _ -> 1
  in
    Text.concat
      [ "Could not parse test target: " <> Text.pack (Parser.parseErrorTextPretty err)
      , " |\n"
      , " | " <> line <> "\n"
      , " | " <> Text.replicate (Parser.errorOffset err) " " <> Text.replicate pointerLen "^"
      ]

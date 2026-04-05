{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

{-| Simulate an ANSI terminal.

TODO: Break out into separate package?
-}
module Skeletest.TestUtils.ANSI (
  Terminal (..),
  TerminalType (..),
  defaultTerminal,
) where

import Control.Monad.Trans.State.Strict qualified as State
import Data.Char (isDigit)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Records (HasField (..))
import Skeletest.Internal.Utils.Text (showT)
import Text.Read (readMaybe)

data Terminal = Terminal
  { type_ :: TerminalType
  , width :: Int
  }

data TerminalType
  = -- | "\r" as CR, "\ESC" as "^["
    DumbTerminal
  | -- | Strip colors, "\r" as CR, cursor movements as "\uFFFD"
    LogTerminal
  | -- | Strip colors, apply "\r" and cursor movements
    AnsiTerminal

defaultTerminal :: Terminal
defaultTerminal =
  Terminal
    { type_ = DumbTerminal
    , width = 80
    }

-- | Transform a text stream to only contain printable characters you'd actually
-- see in a terminal.
instance HasField "resolve" Terminal (Text -> Text) where
  getField term = resolve term . parse
   where
    resolve =
      case term.type_ of
        DumbTerminal -> resolveDumb
        LogTerminal -> resolveLog
        AnsiTerminal -> resolveAnsi

data Token
  = Token_Text Text
  | Token_CR
  | -- | Contains everything after the "\ESC[" sequence
    Token_Color Text
  | Token_Cursor CursorCode

data CursorCode
  = -- | "\ESC[#F" - Cursor Previous Line
    CSI_CPL Int
  | -- | "\ESC[#K" - Erase in Line
    CSI_EL Int

parse :: Text -> [Token]
parse = go0
 where
  go0 s =
    let (pre, post) = Text.break (`elem` ['\r', '\ESC']) s
     in concat
          [ if Text.null pre then [] else [Token_Text pre]
          , go1 post
          ]

  go1 s0 =
    case Text.uncons s0 of
      Nothing -> []
      Just ('\r', s1) -> Token_CR : go0 s1
      Just ('\ESC', s1) -> fromMaybe (error $ "unsupported escape code: " <> show s0) $ do
        ('[', s2) <- Text.uncons s1
        let (params, s3) = Text.span (\c -> isDigit c || c == ';') s2
        (c, s4) <- Text.uncons s3
        token <-
          case c of
            'm' -> Just $ Token_Color (params <> Text.singleton c)
            'F' -> Token_Cursor . CSI_CPL <$> readParamOr 1 params
            'K' -> Token_Cursor . CSI_EL <$> readParamOr 0 params
            _ -> Nothing
        Just $ token : go0 s4
      _ -> error $ "failed to parse: " <> show s0

  readParamOr n = \case
    "" -> Just n
    params -> readMaybe (Text.unpack params)

wrapLines :: Terminal -> Text -> Text
wrapLines term = Text.unlines . concatMap (rewrap term) . Text.lines

rewrap :: Terminal -> Text -> [Text]
rewrap term s = if Text.null s then [s] else Text.chunksOf term.width s

resolveDumb :: Terminal -> [Token] -> Text
resolveDumb term = wrapLines term . Text.concat . map resolve
 where
  resolve = \case
    Token_Text s -> s
    Token_CR -> "\n"
    Token_Color s -> resolveCSI s
    Token_Cursor csi -> resolveCSI $ toCode csi
  resolveCSI s = "^[[" <> s
  toCode = \case
    CSI_CPL n -> showT n <> "F"
    CSI_EL n -> showT n <> "K"

resolveLog :: Terminal -> [Token] -> Text
resolveLog term = wrapLines term . Text.concat . mapMaybe resolve
 where
  resolve = \case
    Token_Text s -> Just s
    Token_CR -> Just "\n"
    Token_Color _ -> Nothing
    Token_Cursor _ -> Just "\xFFFD"

resolveAnsi :: Terminal -> [Token] -> Text
resolveAnsi term = runTerm . mapM_ interpretToken
 where
  -- State tracked during resolution:
  --   ( [Text]     -- Terminal lines, 0th element is at bottom of screen
  --   , (Int, Int) -- Cursor position as (Row, Col)
  --                --   Row 0 = bottom of the screen
  --                --   Col 0 = left of the screen
  --   )
  --
  -- Invariants:
  --   * Every terminal line has length at most term.width
  --   * Row < length buf
  runTerm m = Text.unlines . reverse . fst $ State.execState m initialState
  initialState = ([""], (0, 0))

  updateCoord f = State.modify $ \(buf, pos) -> (buf, f pos)
  updateCurrLine f = State.modify $ \(buf, pos@(row, _)) ->
    let (linesBelow, line, linesAbove) = splitLines row buf
        buf' = linesBelow <> (f line : linesAbove)
     in (buf', pos)
  splitLines row buf =
    case splitAt row buf of
      (_, []) -> error "row out of bounds"
      (linesBelow, line : linesAbove) -> (linesBelow, line, linesAbove)

  interpretToken = \case
    Token_Text s -> addText s
    Token_CR -> updateCoord $ \(row, _) -> (row, 0)
    Token_Color _ -> pure ()
    Token_Cursor csi -> runCursor csi

  addText s = State.modify $ \(buf, (row, col)) ->
    let (linesBelow, line, linesAbove) = splitLines row buf
        newChunks =
          wrapLast . concatMap (rewrap term) . Text.splitOn "\n" $
            Text.take col line <> s
        newLines = overwrite newChunks (line : reverse linesBelow)
        buf' = reverse newLines <> linesAbove
        row' = max 0 $ row - length newChunks + 1
        col' =
          maybe 0 (Text.length . NonEmpty.last) $
            NonEmpty.nonEmpty newChunks
     in (buf', (row', col'))
   where
    overwrite = \cases
      newLines [] -> newLines
      [] oldLines -> oldLines
      (newLine : newLines) (oldLine : oldLines) ->
        let oldLine' =
              -- If this is the last new line, it might be a partial overwrite
              if null newLines
                then Text.drop (Text.length newLine) oldLine
                else ""
         in (newLine <> oldLine') : overwrite newLines oldLines
    -- If last chunk happens to exactly match term.width,
    -- the cursor should wrap to a new line
    wrapLast chunks =
      case NonEmpty.last <$> NonEmpty.nonEmpty chunks of
        Just lastChunk
          | Text.length lastChunk == term.width -> chunks <> [""]
        _ -> chunks

  runCursor = \case
    CSI_CPL n -> do
      updateCoord $ \(row, _) -> (row + n, 0)
    CSI_EL 0 -> do
      (_, (_, col)) <- State.get
      updateCurrLine $ \line -> Text.take col line
    CSI_EL _ -> error "TODO: handle EL with numeric param"

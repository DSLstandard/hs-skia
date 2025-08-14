module Skia.SKParagraph.Metrics where

import Skia.Bindings.Types
import Skia.SKFont qualified as SKFont
import Skia.SKParagraph.Internal.Prelude

data StyleMetrics = StyleMetrics
  { textStyle :: TextStyle
  , fontMetrics :: SKFont.FontMetrics
  }
  deriving (Show)

data LineMetrics = LineMetrics
  { startIndex :: Int
  -- ^ The indexes in the text buffer the line begins and ends.
  , endIndex :: Int
  , endExcludingWhitespaces :: Int
  , endIncludingNewline :: Int
  , hardBreak :: Bool
  , ascent :: Double
  -- ^ The final computed ascent and descent for the line. This can be impacted by
  -- the strut, height, scaling, as well as outlying runs that are very tall.
  --
  -- The top edge is `baseline - ascent` and the bottom edge is `baseline +
  -- descent`. Ascent and descent are provided as positive numbers. Raw numbers
  -- for specific runs of text can be obtained in run_metrics_map. These values
  -- are the cumulative metrics for the entire line.
  , descent :: Double
  , unscaledAscent :: Double
  , height :: Double
  -- ^ Total height of the paragraph including the current line.
  --
  -- The height of the current line is `round(ascent + descent)`.
  , width :: Double
  -- ^ Width of the line.
  , left :: Double
  -- ^ The left edge of the line. The right edge can be obtained with `left +
  -- width`
  , baseline :: Double
  -- ^ The y position of the baseline for this line from the top of the paragraph.
  , lineNumber :: Int
  -- ^ Zero indexed line number
  }
  -- TODO: Add std::map<size_t, StyleMetrics> fLineMetrics;

  deriving (Show)

-- * Marshal utils

unmarshalStyleMetrics :: Skparagraph_style_metrics -> StyleMetrics
unmarshalStyleMetrics metrics =
  StyleMetrics
    { textStyle = fromPtr metrics.text_style
    , fontMetrics = SKFont.marshalFontMetrics metrics.font_metrics
    }

unmarshalLineMetrics :: Skparagraph_line_metrics -> LineMetrics
unmarshalLineMetrics metrics =
  LineMetrics
    { startIndex = fromIntegral metrics.fStartIndex
    , endIndex = fromIntegral metrics.fEndIndex
    , endExcludingWhitespaces = fromIntegral metrics.fEndExcludingWhitespaces
    , endIncludingNewline = fromIntegral metrics.fEndIncludingNewline
    , hardBreak = toBool metrics.fHardBreak
    , ascent = coerce metrics.fAscent
    , descent = coerce metrics.fAscent
    , unscaledAscent = coerce metrics.fUnscaledAscent
    , height = coerce metrics.fHeight
    , width = coerce metrics.fWidth
    , left = coerce metrics.fLeft
    , baseline = coerce metrics.fBaseline
    , lineNumber = fromIntegral metrics.fLineNumber
    }

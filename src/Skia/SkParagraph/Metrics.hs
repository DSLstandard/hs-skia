module Skia.SkParagraph.Metrics where

import Skia.SkParagraph.Internal.Prelude
import qualified Language.C.Inline.Cpp as C

C.context $ C.cppCtx <> cppSkiaObjectTypes <> cppSkParagraphObjectTypes

C.include "modules/skparagraph/include/Metrics.h"

-- data StyleMetrics = StyleMetrics
--   { textStyle :: TextStyle
--   , fontMetrics :: FontMetrics
--   }
--   deriving (Show)

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

-- unmarshalStyleMetrics :: Skparagraph_style_metrics -> StyleMetrics
-- unmarshalStyleMetrics metrics =
--   StyleMetrics
--     { textStyle = fromPtr metrics.text_style
--     , fontMetrics = SkFont.marshalFontMetrics metrics.font_metrics
--     }

peekLineMetrics :: (MonadIO m) => Ptr C'LineMetricsRaw -> m LineMetrics
peekLineMetrics p = liftIO do
  fStartIndex <- [C.exp| size_t { $(skia::textlayout::LineMetrics* p)->fStartIndex } |]
  fEndIndex <- [C.exp| size_t { $(skia::textlayout::LineMetrics* p)->fEndIndex } |]
  fEndExcludingWhitespaces <- [C.exp| size_t { $(skia::textlayout::LineMetrics* p)->fEndExcludingWhitespaces } |]
  fEndIncludingNewline <- [C.exp| size_t { $(skia::textlayout::LineMetrics* p)->fEndIncludingNewline } |]
  fHardBreak <- [C.exp| bool { $(skia::textlayout::LineMetrics* p)->fHardBreak } |]
  fAscent <- [C.exp| double { $(skia::textlayout::LineMetrics* p)->fAscent } |]
  fDescent <- [C.exp| double { $(skia::textlayout::LineMetrics* p)->fDescent } |]
  fUnscaledAscent <- [C.exp| double { $(skia::textlayout::LineMetrics* p)->fUnscaledAscent } |]
  fHeight <- [C.exp| double { $(skia::textlayout::LineMetrics* p)->fHeight } |]
  fWidth <- [C.exp| double { $(skia::textlayout::LineMetrics* p)->fWidth } |]
  fLeft <- [C.exp| double { $(skia::textlayout::LineMetrics* p)->fLeft } |]
  fBaseline <- [C.exp| double { $(skia::textlayout::LineMetrics* p)->fBaseline } |]
  fLineNumber <- [C.exp| size_t { $(skia::textlayout::LineMetrics* p)->fLineNumber } |]

  pure LineMetrics
    { startIndex = fromIntegral fStartIndex
    , endIndex = fromIntegral fEndIndex
    , endExcludingWhitespaces = fromIntegral fEndExcludingWhitespaces
    , endIncludingNewline = fromIntegral fEndIncludingNewline
    , hardBreak = toBool fHardBreak
    , ascent = realToFrac fAscent
    , descent = realToFrac fDescent
    , unscaledAscent = realToFrac fUnscaledAscent
    , height = realToFrac fHeight
    , width = realToFrac fWidth
    , left = realToFrac fLeft
    , baseline = realToFrac fBaseline
    , lineNumber = fromIntegral fLineNumber
    }

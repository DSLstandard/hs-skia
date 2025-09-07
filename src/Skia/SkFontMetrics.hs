module Skia.SkFontMetrics where

import Data.Bits
import Language.C.Inline.Cpp qualified as C
import Skia.Internal.Prelude
import Skia.SkRect

C.context $ C.cppCtx <> cppSkiaObjectTypes

C.include "core/SkFontMetrics.h"

{- | The metrics of an SkFont.

The metric values are consistent with the Skia y-down coordinate system.
-}
data FontMetrics = FontMetrics
  { glyphBounds :: Maybe (Rect Float)
  -- ^ TOP = greatest extent above origin of any glyph bounding box, typically
  -- negative; deprecated with variable fonts
  --
  -- BOTTOM = greatest extent below origin of any glyph bounding box,
  -- typically positive; deprecated with variable fonts
  --
  -- LEFT = (also called xMin) greatest extent to left of origin of any glyph
  -- bounding box, typically negative; deprecated with variable fonts
  --
  -- RIGHT = (also called xMax) greatest extent to right of origin of any
  -- glyph bounding box, typically negative; deprecated with variable fonts
  , ascent :: Float
  -- ^ Distance to reserve above baseline, typically negative.
  , descent :: Float
  -- ^ Distance to reserve below baseline, typically positive.
  , leading :: Float
  -- ^ Distance to add between lines, typically positive or zero.
  , avgCharWidth :: Maybe Float
  -- ^ Average character width, 'Nothing' if unknown.
  , maxCharWidth :: Maybe Float
  -- ^ Maximum character width, 'Nothing' if unknown.
  , xHeight :: Maybe Float
  -- ^ Height of lower-case 'x', typically negative, 'Nothing' if unknown.
  , capHeight :: Maybe Float
  -- ^ Height of an upper-case letter, typically negative, 'Nothing' if unknown,
  , underlineThickness :: Maybe Float
  -- ^ Underline thickness, 'Nothing' if invalid.
  , underlinePosition :: Maybe Float
  -- ^ Distance from baseline to top of stroke, typically positive, 'Nothing' if invalid.
  , strikeoutThickness :: Maybe Float
  -- ^ Strikeout thickness, 'Nothing' if invalid.
  , strikeoutPosition :: Maybe Float
  -- ^ Distance from baseline to bottom of stroke, typically negative, 'Nothing' if invalid.
  }
  deriving (Show)

-- * Marshal utils

allocaSkFontMetrics :: Managed (Ptr C'SkFontMetrics)
allocaSkFontMetrics =
  managed $ bracket
    [C.exp|SkFontMetrics* { new SkFontMetrics() }|]
    (\p -> [C.block| void { delete $(SkFontMetrics* p); } |])

peekSkFontMetrics :: MonadIO m => Ptr C'SkFontMetrics -> m FontMetrics
peekSkFontMetrics input = evalManaged do
  fFlags' <- managed $ alloca @Word32
  fTop' <- managed $ alloca @CFloat
  fAscent' <- managed $ alloca @CFloat
  fDescent' <- managed $ alloca @CFloat
  fLeading' <- managed $ alloca @CFloat
  fAvgCharWidth' <- managed $ alloca @CFloat
  fMaxCharWidth' <- managed $ alloca @CFloat
  fXMin' <- managed $ alloca @CFloat
  fXMax' <- managed $ alloca @CFloat
  fBottom' <- managed $ alloca @CFloat
  fXHeight' <- managed $ alloca @CFloat
  fCapHeight' <- managed $ alloca @CFloat
  fUnderlineThickness' <- managed $ alloca @CFloat
  fUnderlinePosition' <- managed $ alloca @CFloat
  fStrikeoutThickness' <- managed $ alloca @CFloat
  fStrikeoutPosition' <- managed $ alloca @CFloat

  liftIO
    [C.block| void {
    auto p = $(SkFontMetrics* input);
    *$(uint32_t* fFlags') = p->fFlags;
    *$(float* fTop') = p->fTop;
    *$(float* fAscent') = p->fAscent;
    *$(float* fDescent') = p->fDescent;
    *$(float* fLeading') = p->fLeading;
    *$(float* fAvgCharWidth') = p->fAvgCharWidth;
    *$(float* fMaxCharWidth') = p->fMaxCharWidth;
    *$(float* fXMin') = p->fXMin;
    *$(float* fXMax') = p->fXMax;
    *$(float* fBottom') = p->fBottom;
    *$(float* fXHeight') = p->fXHeight;
    *$(float* fCapHeight') = p->fCapHeight;
    *$(float* fUnderlineThickness') = p->fUnderlineThickness;
    *$(float* fUnderlinePosition') = p->fUnderlinePosition;
    *$(float* fStrikeoutThickness') = p->fStrikeoutThickness;
    *$(float* fStrikeoutPosition') = p->fStrikeoutPosition;
  }|]

  fFlags <- liftIO $ peek fFlags'
  fTop <- liftIO $ peek fTop'
  fAscent <- liftIO $ peek fAscent'
  fDescent <- liftIO $ peek fDescent'
  fLeading <- liftIO $ peek fLeading'
  fAvgCharWidth <- liftIO $ peek fAvgCharWidth'
  fMaxCharWidth <- liftIO $ peek fMaxCharWidth'
  fXMin <- liftIO $ peek fXMin'
  fXMax <- liftIO $ peek fXMax'
  fBottom <- liftIO $ peek fBottom'
  fXHeight <- liftIO $ peek fXHeight'
  fCapHeight <- liftIO $ peek fCapHeight'
  fUnderlineThickness <- liftIO $ peek fUnderlineThickness'
  fUnderlinePosition <- liftIO $ peek fUnderlinePosition'
  fStrikeoutThickness <- liftIO $ peek fStrikeoutThickness'
  fStrikeoutPosition <- liftIO $ peek fStrikeoutPosition'

  let
    underlineThicknessIsValid = testBit fFlags $ fromIntegral [C.pure| int { (int) SkFontMetrics::FontMetricsFlags::kUnderlineThicknessIsValid_Flag } |]
    underlinePositionIsValid = testBit fFlags $ fromIntegral [C.pure| int { (int) SkFontMetrics::FontMetricsFlags::kUnderlinePositionIsValid_Flag } |]
    strikeThicknessIsValid = testBit fFlags $ fromIntegral [C.pure| int { (int) SkFontMetrics::FontMetricsFlags::kStrikeoutThicknessIsValid_Flag } |]
    strikePositionIsValid = testBit fFlags $ fromIntegral [C.pure| int { (int) SkFontMetrics::FontMetricsFlags::kStrikeoutPositionIsValid_Flag } |]
    boundsInvalid = testBit fFlags $ fromIntegral [C.pure| int { (int) SkFontMetrics::FontMetricsFlags::kBoundsInvalid_Flag } |]

  pure
    FontMetrics
      { glyphBounds = do
          guard (not boundsInvalid)
          pure
            Rect
              { left = realToFrac fXMin
              , right = realToFrac fXMax
              , top = realToFrac fTop
              , bottom = realToFrac fBottom
              }
      , ascent = realToFrac fAscent
      , descent = realToFrac fDescent
      , leading = realToFrac fLeading
      , avgCharWidth = validUnlessZero fAvgCharWidth
      , maxCharWidth = validUnlessZero fMaxCharWidth
      , xHeight = validUnlessZero fXHeight
      , capHeight = validUnlessZero fCapHeight
      , underlineThickness = guard underlineThicknessIsValid $> coerce fUnderlineThickness
      , underlinePosition = guard underlinePositionIsValid $> coerce fUnderlinePosition
      , strikeoutThickness = guard strikeThicknessIsValid $> coerce fStrikeoutThickness
      , strikeoutPosition = guard strikePositionIsValid $> coerce fStrikeoutPosition
      }
 where
  -- Some values are set to "zero" when unknown. Example: Google Skia's
  -- comment on fXHeight: height of lower-case 'x', zero if unknown,
  -- typically negative.
  validUnlessZero :: CFloat -> Maybe Float
  validUnlessZero value =
    -- TODO: It should be fine to simply do (== 0)... right?
    if value == 0
      then Nothing
      else Just (coerce value)

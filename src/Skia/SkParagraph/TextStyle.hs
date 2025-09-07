module Skia.SkParagraph.TextStyle where

import Data.Text qualified as T
import Data.Traversable
import Data.Vector.Storable qualified as VS
import Language.C.Inline.Cpp qualified as C
import Skia.Internal.THUtils
import Skia.SkColor
import Skia.SkParagraph.DartTypes
import Skia.SkParagraph.Internal.Prelude
import Skia.SkParagraph.TextShadow
import Skia.SkString as SkString

C.context $ C.cppCtx <> C.vecCtx <> cppSkiaObjectTypes <> cppSkParagraphObjectTypes

C.include "modules/skparagraph/include/TextStyle.h"

createEmpty :: (MonadResource m) => m (ReleaseKey, TextStyle)
createEmpty =
  allocateSkObjectNeverNull
    [C.exp| skia::textlayout::TextStyle* { new skia::textlayout::TextStyle() } |]
    (\p -> [C.block| void { delete $(skia::textlayout::TextStyle* p); }|])

setColor :: (MonadIO m) => TextStyle -> SkColor -> m ()
setColor (ptr -> style) (SkColor color) = liftIO do
  [C.block| void {
    $(skia::textlayout::TextStyle* style)->setColor((SkColor) $(uint32_t color));
  }|]

setForegroundPaint :: (MonadIO m) => TextStyle -> SkPaint -> m ()
setForegroundPaint (ptr -> style) (ptr -> paint) = liftIO do
  [C.block| void {
    $(skia::textlayout::TextStyle* style)->setForegroundPaint(*$(SkPaint* paint));
  }|]

clearForegroundColor :: (MonadIO m) => TextStyle -> m ()
clearForegroundColor (ptr -> style) = liftIO do
  [C.block| void {
    $(skia::textlayout::TextStyle* style)->clearForegroundColor();
  }|]

setBackgroundPaint :: (MonadIO m) => TextStyle -> SkPaint -> m ()
setBackgroundPaint (ptr -> style) (ptr -> paint) = liftIO do
  [C.block| void {
    $(skia::textlayout::TextStyle* style)->setBackgroundPaint(*$(SkPaint* paint));
  }|]

clearBackgroundColor :: (MonadIO m) => TextStyle -> m ()
clearBackgroundColor (ptr -> style) = liftIO do
  [C.block| void {
    $(skia::textlayout::TextStyle* style)->clearBackgroundColor();
  }|]

data TextDecoration = TextDecoration
  { underline :: Bool
  , overline :: Bool
  , lineThrough :: Bool
  }
  deriving (Show)

marshalTextDecoration :: TextDecoration -> CInt
marshalTextDecoration d =
  makeBitFlags
    [ (d.underline, [C.pure| int { (int) skia::textlayout::TextDecoration::kUnderline } |])
    , (d.overline, [C.pure| int { (int) skia::textlayout::TextDecoration::kOverline } |])
    , (d.lineThrough, [C.pure| int { (int) skia::textlayout::TextDecoration::kLineThrough } |])
    ]

setDecoration :: (MonadIO m) => TextStyle -> TextDecoration -> m ()
setDecoration (ptr -> style) decoration = liftIO do
  let cdecoration = marshalTextDecoration decoration
  [C.block| void {
    $(skia::textlayout::TextStyle* style)->setDecoration(
      (skia::textlayout::TextDecoration) $(int cdecoration)
    );
  }|]

$( qGenerateSkEnum
  "TextDecorationMode"
  ""
  [ ("kGaps", "skia::textlayout::TextDecorationMode::kGaps", "")
  , ("kThrough", "skia::textlayout::TextDecorationMode::kThrough", "")
  ]
 )

setDecorationMode :: (MonadIO m) => TextStyle -> TextDecorationMode -> m ()
setDecorationMode (ptr -> style) (marshalSkEnum -> cmode) = liftIO do
  [C.block| void {
    $(skia::textlayout::TextStyle* style)->setDecorationMode(
      (skia::textlayout::TextDecorationMode) $(int cmode)
    );
  }|]

$( qGenerateSkEnum
  "TextDecorationStyle"
  ""
  [ ("kSolid", "skia::textlayout::TextDecorationStyle::kSolid", "")
  , ("kDouble", "skia::textlayout::TextDecorationStyle::kDouble", "")
  , ("kDotted", "skia::textlayout::TextDecorationStyle::kDotted", "")
  , ("kDashed", "skia::textlayout::TextDecorationStyle::kDashed", "")
  , ("kWavy", "skia::textlayout::TextDecorationStyle::kWavy", "")
  ]
 )

setDecorationStyle :: (MonadIO m) => TextStyle -> TextDecorationStyle -> m ()
setDecorationStyle (ptr -> style) (marshalSkEnum -> cstyle) = liftIO do
  [C.block| void {
    $(skia::textlayout::TextStyle* style)->setDecorationStyle(
      (skia::textlayout::TextDecorationStyle) $(int cstyle)
    );
  }|]

setDecorationColor :: (MonadIO m) => TextStyle -> SkColor -> m ()
setDecorationColor (ptr -> style) (SkColor color) = liftIO do
  [C.block| void {
    $(skia::textlayout::TextStyle* style)->setDecorationColor((SkColor) $(uint32_t color));
  }|]

setDecorationThicknessMultipler :: (MonadIO m) => TextStyle -> Float -> m ()
setDecorationThicknessMultipler (ptr -> style) (coerce -> multiplier) = liftIO do
  [C.block| void {
    $(skia::textlayout::TextStyle* style)->setDecorationThicknessMultiplier($(float multiplier));
  }|]

addShadow :: (MonadIO m) => TextStyle -> TextShadow -> m ()
addShadow (ptr -> style) shadow = evalManaged do
  shadow' <- marshalTextShadow shadow
  liftIO [C.block| void {
    $(skia::textlayout::TextStyle* style)->addShadow(*$(skia::textlayout::TextShadow* shadow'));
  }|]

resetShadow :: (MonadIO m) => TextStyle -> m ()
resetShadow (ptr -> style) = liftIO do
  [C.block| void {
    $(skia::textlayout::TextStyle* style)->resetShadows();
  }|]

addFontFeature ::
  (MonadIO m) =>
  TextStyle ->
  -- | Font feature name
  T.Text ->
  -- | Font feature value
  Int ->
  m ()
addFontFeature (ptr -> style) name (fromIntegral -> value) = liftIO $ runResourceT do
  (_, ptr -> nameStr) <- SkString.createFromText name
  liftIO [C.block| void {
    $(skia::textlayout::TextStyle* style)->addFontFeature(
      *$(SkString* nameStr), $(int value)
    );
  }|]

resetFontFeatures :: (MonadIO m) => TextStyle -> m ()
resetFontFeatures (ptr -> style) = liftIO do
  [C.block| void {
    $(skia::textlayout::TextStyle* style)->resetFontFeatures();
  }|]

setFontSize :: (MonadIO m) => TextStyle -> Float -> m ()
setFontSize (ptr -> style) (coerce -> size) = liftIO do
  [C.block| void {
    $(skia::textlayout::TextStyle* style)->setFontSize($(float size));
  }|]

setFontFamilies :: (MonadIO m) => TextStyle -> [T.Text] -> m ()
setFontFamilies (ptr -> style) families = liftIO $ runResourceT do
  familyStrings <- VS.fromList <$> for families \family -> do
    (_, ptr -> familyStr) <- SkString.createFromText family
    pure familyStr
  
  liftIO [C.block| void {
    std::vector<SkString> fontFamilies($vec-len:familyStrings);
    
    for (int i = 0; i < $vec-len:familyStrings; ++i) {
      auto str = $vec-ptr:(SkString** familyStrings)[i];
      fontFamilies.emplace_back(*str);
    }
    
    $(skia::textlayout::TextStyle* style)->setFontFamilies(fontFamilies);
  }|]

setBaselineShift :: (MonadIO m) => TextStyle -> Float -> m ()
setBaselineShift (ptr -> style) (coerce -> shift) = liftIO do
  [C.block| void {
    $(skia::textlayout::TextStyle* style)->setBaselineShift($(float shift));
  }|]

setHeight :: (MonadIO m) => TextStyle -> Float -> m ()
setHeight (ptr -> style) (coerce -> height) = liftIO do
  [C.block| void {
    $(skia::textlayout::TextStyle* style)->setHeight($(float height));
  }|]

setHeightOverride :: (MonadIO m) => TextStyle -> Bool -> m ()
setHeightOverride (ptr -> style) (fromBool -> override) = liftIO do
  [C.block| void {
    $(skia::textlayout::TextStyle* style)->setHeightOverride($(bool override));
  }|]

setHalfLeading :: (MonadIO m) => TextStyle -> Bool -> m ()
setHalfLeading (ptr -> style) (fromBool -> halfLeading) = liftIO do
  [C.block| void {
    $(skia::textlayout::TextStyle* style)->setHalfLeading($(bool halfLeading));
  }|]

setLetterSpacing :: (MonadIO m) => TextStyle -> Float -> m ()
setLetterSpacing (ptr -> style) (coerce -> spacing) = liftIO do
  [C.block| void {
    $(skia::textlayout::TextStyle* style)->setLetterSpacing($(float spacing));
  }|]

setWordSpacing :: (MonadIO m) => TextStyle -> Float -> m ()
setWordSpacing (ptr -> style) (coerce -> spacing) = liftIO do
  [C.block| void {
    $(skia::textlayout::TextStyle* style)->setWordSpacing($(float spacing));
  }|]

setTypeface :: (MonadIO m) => TextStyle -> SkTypeface -> m ()
setTypeface (ptr -> style) (ptr -> typeface) = liftIO do
  [C.block| void {
    $(skia::textlayout::TextStyle* style)->setTypeface(sk_ref_sp($(SkTypeface* typeface)));
  }|]

setLocale :: (MonadIO m) => TextStyle -> T.Text -> m ()
setLocale (ptr -> style) locale = liftIO $ runResourceT do
  (_, ptr -> localeStr) <- SkString.createFromText locale
  liftIO [C.block| void {
    $(skia::textlayout::TextStyle* style)->setLocale(*$(SkString* localeStr));
  }|]

setTextBaseline :: (MonadIO m) => TextStyle -> TextBaseline -> m ()
setTextBaseline (ptr -> style) (marshalSkEnum -> cbaseline) = liftIO do
  [C.block| void {
    $(skia::textlayout::TextStyle* style)->setTextBaseline(
      (skia::textlayout::TextBaseline) $(int cbaseline)
    );
  }|]

setPlaceholder :: (MonadIO m) => TextStyle -> m ()
setPlaceholder (ptr -> style) = liftIO do
  [C.block| void {
    $(skia::textlayout::TextStyle* style)->setPlaceholder();
  }|]

$( qGenerateSkEnum
  "PlaceholderAlignment"
  ""
  [ ("kBaseline", "skia::textlayout::PlaceholderAlignment::kBaseline", "Match the baseline of the placeholder with the baseline.")
  , ("kAboveBaseline", "skia::textlayout::PlaceholderAlignment::kAboveBaseline", "Align the bottom edge of the placeholder with the baseline.")
  , ("kBelowBaseline", "skia::textlayout::PlaceholderAlignment::kBelowBaseline", "Align the top edge of the placeholder with the baseline.")
  , ("kTop", "skia::textlayout::PlaceholderAlignment::kTop", "Align the top edge of the placeholder with the top edge of the font.")
  , ("kBottom", "skia::textlayout::PlaceholderAlignment::kBottom", "Align the bottom edge of the placeholder with the top edge of the font.")
  , ("kMiddle", "skia::textlayout::PlaceholderAlignment::kMiddle", "Align the middle of the placeholder with the middle of the text.")
  ]
 )

data PlaceholderStyle = PlaceholderStyle
  { width :: Float
  , height :: Float
  , alignment :: PlaceholderAlignment
  , baseline :: TextBaseline
  , baselineOffset :: Float
  }

marshalPlaceholderStyle :: PlaceholderStyle -> Managed (Ptr C'PlaceholderStyleRaw)
marshalPlaceholderStyle style = do
  managed $ bracket
    ( do
        let width :: CFloat = coerce style.width
        let height :: CFloat = coerce style.height
        let alignment = marshalSkEnum style.alignment
        let baseline = marshalSkEnum style.baseline
        let baselineOffset :: CFloat = coerce style.baselineOffset
        [C.exp| skia::textlayout::PlaceholderStyle* {
          new skia::textlayout::PlaceholderStyle(
            $(float width),
            $(float height),
            (skia::textlayout::PlaceholderAlignment) $(int alignment),
            (skia::textlayout::TextBaseline) $(int baseline),
            $(float baselineOffset)
          )
        }|]
    )
    (\p -> [C.block| void { delete $(skia::textlayout::PlaceholderStyle* p); } |])

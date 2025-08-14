module Skia.SKParagraph.TextStyle where

import Data.Text qualified as T
import Data.Traversable
import Skia.Bindings.Skparagraph
import Skia.Bindings.Types
import Skia.Color
import Skia.Internal.Prelude
import Skia.SKParagraph.Internal.Prelude
import Skia.SKParagraph.TextShadow
import Skia.SKString as SKString

createEmpty :: (MonadResource m) => m (ReleaseKey, TextStyle)
createEmpty =
  allocateSKObjectNeverNull
    skparagraph_text_style_new
    skparagraph_text_style_delete

setColor :: (MonadIO m) => TextStyle -> SKColor -> m ()
setColor style color = liftIO do
  skparagraph_text_style_set_color (ptr style) (unSKColor color)

setForegroundPaint :: (MonadIO m) => TextStyle -> SKPaint -> m ()
setForegroundPaint style paint = liftIO do
  skparagraph_text_style_set_foreground_paint (ptr style) (ptr paint)

clearForegroundPaint :: (MonadIO m) => TextStyle -> m ()
clearForegroundPaint style = liftIO do
  skparagraph_text_style_clear_foreground_paint (ptr style)

setBackgroundPaint :: (MonadIO m) => TextStyle -> SKPaint -> m ()
setBackgroundPaint style paint = liftIO do
  skparagraph_text_style_set_background_paint (ptr style) (ptr paint)

clearBackgroundPaint :: (MonadIO m) => TextStyle -> m ()
clearBackgroundPaint style = liftIO do
  skparagraph_text_style_clear_background_paint (ptr style)

data TextDecoration = TextDecoration
  { underline :: Bool
  , overline :: Bool
  , lineThrough :: Bool
  }
  deriving (Show)

setDecoration :: (MonadIO m) => TextStyle -> TextDecoration -> m ()
setDecoration style decoration = liftIO do
  skparagraph_text_style_set_decoration (ptr style) (marshalTextDecoration decoration)

setDecorationMode :: (MonadIO m) => TextStyle -> TextDecorationMode -> m ()
setDecorationMode style mode = liftIO do
  skparagraph_text_style_set_decoration_mode (ptr style) (marshalSKEnum mode)

setDecorationStyle :: (MonadIO m) => TextStyle -> TextDecorationStyle -> m ()
setDecorationStyle style decoStyle = liftIO do
  skparagraph_text_style_set_decoration_style (ptr style) (marshalSKEnum decoStyle)

setDecorationColor :: (MonadIO m) => TextStyle -> SKColor -> m ()
setDecorationColor style color = liftIO do
  skparagraph_text_style_set_decoration_color (ptr style) (unSKColor color)

setDecorationThicknessMultipler :: (MonadIO m) => TextStyle -> Float -> m ()
setDecorationThicknessMultipler style multipler = liftIO do
  skparagraph_text_style_set_decoration_thickness_multipler (ptr style) (coerce multipler)

addShadow :: (MonadIO m) => TextStyle -> TextShadow -> m ()
addShadow style shadow = evalManaged do
  shadow' <- marshalTextShadow shadow
  liftIO $ skparagraph_text_style_add_shadow (ptr style) shadow'

resetShadow :: (MonadIO m) => TextStyle -> m ()
resetShadow style =
  liftIO $ skparagraph_text_style_reset_shadow (ptr style)

addFontFeature ::
  (MonadIO m) =>
  TextStyle ->
  -- | Font feature name
  T.Text ->
  -- | Font feature value
  Int ->
  m ()
addFontFeature style name value = liftIO $ runResourceT do
  (_, nameStr) <- SKString.createFromText name
  liftIO $ skparagraph_text_style_add_font_feature (ptr style) (ptr nameStr) (fromIntegral value)

resetFontFeatures :: (MonadIO m) => TextStyle -> m ()
resetFontFeatures style =
  liftIO $ skparagraph_text_style_reset_font_features (ptr style)

setFontArguments :: (MonadIO m) => TextStyle -> SKFontArguments -> m ()
setFontArguments style args =
  liftIO $ skparagraph_text_style_set_font_arguments (ptr style) (ptr args)

setFontSize :: (MonadIO m) => TextStyle -> Float -> m ()
setFontSize style size = liftIO do
  liftIO $ skparagraph_text_style_set_font_size (ptr style) (coerce size)

setFontFamilies :: (MonadIO m) => TextStyle -> [T.Text] -> m ()
setFontFamilies style families = liftIO $ runResourceT do
  families :: [Ptr Sk_string] <- for families \family -> do
    (_, family) <- SKString.createFromText family
    pure (ptr family)

  liftIO $ Foreign.Marshal.Array.withArrayLen families \len families' ->
    skparagraph_text_style_set_font_families (ptr style) families' (fromIntegral len)

setBaselineShift :: (MonadIO m) => TextStyle -> Float -> m ()
setBaselineShift style shift = liftIO do
  liftIO $ skparagraph_text_style_set_baseline_shift (ptr style) (coerce shift)

setHeight :: (MonadIO m) => TextStyle -> Float -> m ()
setHeight style height = liftIO do
  liftIO $ skparagraph_text_style_set_height (ptr style) (coerce height)

setHeightOverride :: (MonadIO m) => TextStyle -> Bool -> m ()
setHeightOverride style v = liftIO do
  liftIO $ skparagraph_text_style_set_height_override (ptr style) (fromBool v)

setHalfLeading :: (MonadIO m) => TextStyle -> Bool -> m ()
setHalfLeading style v = liftIO do
  liftIO $ skparagraph_text_style_set_half_leading (ptr style) (fromBool v)

setLetterSpacing :: (MonadIO m) => TextStyle -> Float -> m ()
setLetterSpacing style spacing = liftIO do
  liftIO $ skparagraph_text_style_set_letter_spacing (ptr style) (coerce spacing)

setWordSpacing :: (MonadIO m) => TextStyle -> Float -> m ()
setWordSpacing style spacing = liftIO do
  liftIO $ skparagraph_text_style_set_word_spacing (ptr style) (coerce spacing)

setTypeface :: (MonadIO m) => TextStyle -> SKTypeface -> m ()
setTypeface style typeface = liftIO do
  liftIO $ skparagraph_text_style_set_typeface (ptr style) (ptr typeface)

setLocale :: (MonadIO m) => TextStyle -> T.Text -> m ()
setLocale style locale = liftIO $ runResourceT do
  (_, localeStr) <- SKString.createFromText locale
  liftIO $ skparagraph_text_style_set_locale (ptr style) (ptr localeStr)

setTextBaseline :: (MonadIO m) => TextStyle -> TextBaseline -> m ()
setTextBaseline style baseline = liftIO do
  skparagraph_text_style_set_text_baseline (ptr style) (marshalSKEnum baseline)

setPlaceholder :: (MonadIO m) => TextStyle -> m ()
setPlaceholder style = liftIO do
  skparagraph_text_style_set_placeholder (ptr style)

-- * Marshal utils

marshalTextDecoration :: TextDecoration -> Skparagraph_text_decoration_flags
marshalTextDecoration d =
  makeBitFlags
    [ (d.underline, UNDERLINE_SKPARAGRAPH_TEXT_DECORATION_FLAGS)
    , (d.overline, OVERLINE_SKPARAGRAPH_TEXT_DECORATION_FLAGS)
    , (d.lineThrough, LINETHROUGH_SKPARAGRAPH_TEXT_DECORATION_FLAGS)
    ]

module Skia.SkFont where

import Language.C.Inline.Cpp qualified as C
import Skia.Internal.Prelude
import Skia.Internal.THUtils
import Skia.SkFontMetrics
import Skia.SkFontTypes
import Skia.SkRefCnt qualified as SkRefCnt

C.context $ C.cppCtx <> cppSkiaObjectTypes

C.include "core/SkFont.h"

-- | Creates an 'SkFont' with default internal settings.
createDefault :: (MonadResource m) => m (ReleaseKey, SkFont)
createDefault =
  allocateSkObjectNeverNull
    [C.exp| SkFont* { new SkFont() }|]
    (\p -> [C.exp| void { delete $(SkFont* p); } |])

isForceAutoHinting :: (MonadIO m) => SkFont -> m Bool
isForceAutoHinting (ptr -> font) =
  liftIO $
    toBool
      <$> [C.exp| bool { $(SkFont* font)->isForceAutoHinting() } |]

setForceAutoHinting :: (MonadIO m) => SkFont -> Bool -> m ()
setForceAutoHinting (ptr -> font) (fromBool -> v) =
  liftIO $
    [C.exp| void { $(SkFont* font)->setForceAutoHinting($(bool v)) } |]

isEmbeddedBitmaps :: (MonadIO m) => SkFont -> m Bool
isEmbeddedBitmaps (ptr -> font) =
  liftIO $
    toBool
      <$> [C.exp| bool { $(SkFont* font)->isEmbeddedBitmaps() } |]

setEmbeddedBitmaps :: (MonadIO m) => SkFont -> Bool -> m ()
setEmbeddedBitmaps (ptr -> font) (fromBool -> v) =
  liftIO $
    [C.exp| void { $(SkFont* font)->setEmbeddedBitmaps($(bool v)) } |]

isSubpixel :: (MonadIO m) => SkFont -> m Bool
isSubpixel (ptr -> font) =
  liftIO $
    toBool
      <$> [C.exp| bool { $(SkFont* font)->isSubpixel() } |]

setSubpixel :: (MonadIO m) => SkFont -> Bool -> m ()
setSubpixel (ptr -> font) (fromBool -> v) =
  liftIO $
    [C.exp| void { $(SkFont* font)->setSubpixel($(bool v)) } |]

isLinearMetrics :: (MonadIO m) => SkFont -> m Bool
isLinearMetrics (ptr -> font) =
  liftIO $
    toBool
      <$> [C.exp| bool { $(SkFont* font)->isLinearMetrics() } |]

setLinearMetrics :: (MonadIO m) => SkFont -> Bool -> m ()
setLinearMetrics (ptr -> font) (fromBool -> v) =
  liftIO $
    [C.exp| void { $(SkFont* font)->setLinearMetrics($(bool v)) } |]

isEmbolden :: (MonadIO m) => SkFont -> m Bool
isEmbolden (ptr -> font) =
  liftIO $
    toBool
      <$> [C.exp| bool { $(SkFont* font)->isEmbolden() } |]

setEmbolden :: (MonadIO m) => SkFont -> Bool -> m ()
setEmbolden (ptr -> font) (fromBool -> v) =
  liftIO $
    [C.exp| void { $(SkFont* font)->setEmbolden($(bool v)) } |]

isBaselineSnap :: (MonadIO m) => SkFont -> m Bool
isBaselineSnap (ptr -> font) =
  liftIO $
    toBool
      <$> [C.exp| bool { $(SkFont* font)->isBaselineSnap() } |]

setBaselineSnap :: (MonadIO m) => SkFont -> Bool -> m ()
setBaselineSnap (ptr -> font) (fromBool -> v) =
  liftIO $
    [C.exp| void { $(SkFont* font)->setBaselineSnap($(bool v)) } |]

$( qGenerateSkEnum
    "Edging"
    "Whether edge pixels draw opaque or with partial transparency."
    [ ("Alias", "SkFont::Edging::kAlias", "no transparent pixels on glyph edges")
    , ("AntiAlias", "SkFont::Edging::kAntiAlias", "may have transparent pixels on glyph edges")
    , ("SubpixelAntiAlias", "SkFont::Edging::kSubpixelAntiAlias", "glyph positioned in pixel using transparency")
    ]
 )

getEdging :: (MonadIO m) => SkFont -> m Edging
getEdging (ptr -> font) =
  liftIO $
    unmarshalSkEnumOrDie
      =<< [C.exp| int { (int) $(SkFont* font)->getEdging() } |]

setEdging :: (MonadIO m) => SkFont -> Edging -> m ()
setEdging (ptr -> font) (marshalSkEnum -> v) =
  liftIO $
    [C.exp| void { $(SkFont* font)->setEdging((SkFont::Edging) $(int v)) } |]

getHinting :: (MonadIO m) => SkFont -> m SkFontHinting
getHinting (ptr -> font) =
  liftIO $
    unmarshalSkEnumOrDie
      =<< [C.exp| int { (int) $(SkFont* font)->getHinting() } |]

setHinting :: (MonadIO m) => SkFont -> SkFontHinting -> m ()
setHinting (ptr -> font) (marshalSkEnum -> v) =
  liftIO $
    [C.exp| void { $(SkFont* font)->setHinting((SkFontHinting) $(int v)) } |]

{- | Returns SkTypeface if set, or 'Nothing'.

This function increments the reference counter of the returned typeface by one,
and decrements the reference counter once the returned typeface is finalized in
Haskell.
-}
getTypeface :: (MonadResource m) => SkFont -> m (Maybe (ReleaseKey, SkTypeface))
getTypeface (ptr -> font) =
  allocateSkObjectOrNothingIfNull'
    [C.exp| SkTypeface* { $(SkFont* font)->getTypeface() } |]
    SkRefCnt.decrement

{- | Sets SkTypeface to typeface, decreasing SkRefCnt of the previous
SkTypeface. Pass 'Nothing' to clear SkTypeface and use the default typeface.

Increments the reference counter of the input typeface by one by 'SkFont'.
-}
setTypeface :: (MonadIO m) => SkFont -> Maybe SkTypeface -> m ()
setTypeface (ptr -> font) (maybe nullPtr ptr -> tf) =
  liftIO $
    [C.exp| void { $(SkFont* font)->setTypeface(sk_ref_sp($(SkTypeface* tf))) } |]

-- | Returns text size in points.
getSize :: (MonadIO m) => SkFont -> m Float
getSize (ptr -> font) =
  liftIO $
    coerce
      <$> [C.exp| float { $(SkFont* font)->getSize() } |]

{- | Sets text size in points. Has no effect if textSize is not greater than or
equal to zero.
-}
setSize :: (MonadIO m) => SkFont -> Float -> m ()
setSize (ptr -> font) (coerce -> v) =
  liftIO $
    [C.exp| void { $(SkFont* font)->setSize($(float v)) } |]

-- | Returns text scale on x-axis. Default value is 1.
getScaleX :: (MonadIO m) => SkFont -> m Float
getScaleX (ptr -> font) =
  liftIO $
    coerce
      <$> [C.exp| float { $(SkFont* font)->getScaleX() } |]

-- | Sets text scale on x-axis. Default value is 1.
setScaleX :: (MonadIO m) => SkFont -> Float -> m ()
setScaleX (ptr -> font) (coerce -> v) =
  liftIO $
    [C.exp| void { $(SkFont* font)->setScaleX($(float v)) } |]

-- | Returns text skew on x-axis. Default value is zero.
getSkewX :: (MonadIO m) => SkFont -> m Float
getSkewX (ptr -> font) =
  liftIO $
    coerce
      <$> [C.exp| float { $(SkFont* font)->getSkewX() } |]

-- | Sets text skew on x-axis. Default value is zero.
setSkewX ::
  (MonadIO m) =>
  SkFont ->
  -- | \"skewX\". Additional shear on x-axis relative to y-axis
  Float ->
  m ()
setSkewX (ptr -> font) (coerce -> v) =
  liftIO $
    [C.exp| void { $(SkFont* font)->setSkewX($(float v)) } |]

{- | Returns SkFontMetrics associated with SkTypeface.

The return value is the recommended spacing between lines: the sum of metrics
descent, ascent, and leading.

Results are scaled by text size but does not take into account dimensions
required by text scale, text skew, fake bold, style stroke, and SkPathEffect.
-}
getMetrics ::
  (MonadIO m) =>
  SkFont ->
  -- | (Font metrics, Recommended spacing between lines)
  m (FontMetrics, Float)
getMetrics (ptr -> font') = evalManaged do
  metrics' <- allocaSkFontMetrics
  spacing <- liftIO [C.block| float { return $(SkFont* font')->getMetrics($(SkFontMetrics* metrics')); }|]
  metrics <- peekSkFontMetrics metrics'
  pure (metrics, coerce spacing)

{- | Returns the recommended spacing between lines: the sum of metrics descent,
ascent, and leading.

Result is scaled by text size but does not take into account dimensions required
by stroking and SkPathEffect.

Returns the same result as getMetrics().
-}
getSpacing ::
  (MonadIO m) =>
  SkFont ->
  -- | Recommended spacing between lines
  m Float
getSpacing (ptr -> font') = evalManaged do
  spacing <- liftIO [C.block| float { return $(SkFont* font')->getSpacing(); } |]
  pure (coerce spacing)

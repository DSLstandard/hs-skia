module Skia.SKPaint where

import Control.Monad.Trans.Resource
import Linear
import Skia.Bindings.Sk_paint
import Skia.Color
import Skia.Internal.Prelude
import Skia.Rect
import Skia.SKRefCnt qualified as SKRefCnt

-- | Constructs SkPaint with default values.
create :: (MonadResource m) => m (ReleaseKey, SKPaint)
create =
  allocateSKObjectNeverNull
    sk_paint_new
    sk_paint_delete

{- | Makes a shallow copy of SkPaint. SkPathEffect, SkShader, SkMaskFilter,
SkColorFilter, and SkImageFilter are shared between the original paint and
the copy. Objects containing SkRefCnt increment their references by one.

The referenced objects SkPathEffect, SkShader, SkMaskFilter, SkColorFilter,
and SkImageFilter cannot be modified after they are created. This prevents
objects with SkRefCnt from being modified once SkPaint refers to them.
-}
clone :: (MonadResource m) => SKPaint -> m (ReleaseKey, SKPaint)
clone paint =
  allocateSKObjectNeverNull
    (sk_paint_clone (ptr paint))
    sk_paint_delete

{- | Sets all SkPaint contents to their initial values. This is equivalent to
replacing SkPaint with the result of 'create'.
-}
reset :: (MonadIO m) => SKPaint -> m ()
reset paint = evalManaged do
  liftIO $ sk_paint_reset (ptr paint)

{- | Returns true if pixels on the active edges of SkPath may be drawn with
partial transparency.
-}
isAntialias :: (MonadIO m) => SKPaint -> m Bool
isAntialias paint = evalManaged do
  r <- liftIO $ sk_paint_is_antialias (ptr paint)
  pure $ toBool r

{- | Requests, but does not require, that edge pixels draw opaque or with
partial transparency.
-}
setAntialias :: (MonadIO m) => SKPaint -> Bool -> m ()
setAntialias paint antialias = evalManaged do
  liftIO $ sk_paint_set_antialias (ptr paint) (fromBool antialias)

-- | Returns true if color error may be distributed to smooth color transition.
isDither :: (MonadIO m) => SKPaint -> m Bool
isDither paint = evalManaged do
  r <- liftIO $ sk_paint_is_dither (ptr paint)
  pure $ toBool r

-- | Requests, but does not require, to distribute color error.
setDither :: (MonadIO m) => SKPaint -> Bool -> m ()
setDither paint dither = evalManaged do
  liftIO $ sk_paint_set_dither (ptr paint) (fromBool dither)

-- | Returns whether the geometry is filled, stroked, or filled and stroked.
getStyle :: (MonadIO m) => SKPaint -> m SKPaintStyle
getStyle paint = evalManaged do
  r <- liftIO $ sk_paint_get_style (ptr paint)
  unmarshalSKEnumOrDie r

{- | Sets whether the geometry is filled, stroked, or filled and stroked. Has no
effect if style is not a legal SkPaint::Style value.
-}
setStyle :: (MonadIO m) => SKPaint -> SKPaintStyle -> m ()
setStyle paint style = evalManaged do
  liftIO $ sk_paint_set_style (ptr paint) (marshalSKEnum style)

-- | Retrieves alpha and RGB, unpremultiplied, packed into 32 bits.
getColor :: (MonadIO m) => SKPaint -> m SKColor
getColor paint = liftIO do
  fmap coerce $ sk_paint_get_color (ptr paint)

{- | Sets alpha and RGB used when stroking and filling. The color is a 32-bit
value, unpremultiplied, packing 8-bit components for alpha, red, blue, and
green.
-}
setColor :: (MonadIO m) => SKPaint -> SKColor -> m ()
setColor paint color = liftIO do
  sk_paint_set_color (ptr paint) (coerce color)

{- | Retrieves alpha and RGB, unpremultiplied, as four floating point values.
RGB are extended sRGB values (sRGB gamut, and encoded with the sRGB transfer
function).
-}
getColorRGBA :: (MonadIO m) => SKPaint -> m (RGBA Float)
getColorRGBA paint = evalManaged do
  color' <- managed alloca
  liftIO $ sk_paint_get_color4f (ptr paint) color'
  liftIO $ fromSKColor4f <$> peek color'

-- | 'setColorRGBA' is to 'setColor' as 'getColorRGBA' is to 'getColor'.
setColorRGBA ::
  (MonadIO m) =>
  SKPaint ->
  RGBA Float ->
  Maybe SKColorSpace ->
  m ()
setColorRGBA paint rgba colorspace = evalManaged do
  rgba' <- storable (toSKColor4f rgba)
  liftIO $ sk_paint_set_color4f (ptr paint) rgba' (ptrOrNull colorspace)

-- | Returns the thickness of the pen used by SkPaint to outline the shape.
getStrokeWidth :: (MonadIO m) => SKPaint -> m Float
getStrokeWidth paint = evalManaged do
  r <- liftIO $ sk_paint_get_stroke_width (ptr paint)
  pure $ coerce r

{- | Sets the thickness of the pen used by the paint to outline the shape.
A stroke-width of zero is treated as "hairline" width. Hairlines are always exactly one
pixel wide in device space (their thickness does not change as the canvas is scaled).
Negative stroke-widths are invalid; setting a negative width will have no effect.
-}
setStrokeWidth ::
  (MonadIO m) =>
  SKPaint ->
  -- | zero thickness for hairline; greater than zero for pen thickness
  Float ->
  m ()
setStrokeWidth paint width = evalManaged do
  liftIO $ sk_paint_set_stroke_width (ptr paint) (coerce width)

-- | Returns the limit at which a sharp corner is drawn beveled.
getStrokeMiter :: (MonadIO m) => SKPaint -> m Float
getStrokeMiter paint = evalManaged do
  r <- liftIO $ sk_paint_get_stroke_miter (ptr paint)
  pure $ coerce r

{- | Sets the limit at which a sharp corner is drawn beveled. Valid values are
zero and greater. Has no effect if miter is less than zero.
-}
setStrokeMiter :: (MonadIO m) => SKPaint -> Float -> m ()
setStrokeMiter paint miter = evalManaged do
  liftIO $ sk_paint_set_stroke_miter (ptr paint) (coerce miter)

-- | Returns the geometry drawn at the beginning and end of strokes.
getStrokeCap :: (MonadIO m) => SKPaint -> m SKStrokeCap
getStrokeCap paint = evalManaged do
  r <- liftIO $ sk_paint_get_stroke_cap (ptr paint)
  unmarshalSKEnumOrDie r

-- | Sets the geometry drawn at the beginning and end of strokes.
setStrokeCap :: (MonadIO m) => SKPaint -> SKStrokeCap -> m ()
setStrokeCap paint cap = evalManaged do
  liftIO $ sk_paint_set_stroke_cap (ptr paint) (marshalSKEnum cap)

-- | Returns the geometry drawn at the corners of strokes.
getStrokeJoin :: (MonadIO m) => SKPaint -> m SKStrokeJoin
getStrokeJoin paint = evalManaged do
  r <- liftIO $ sk_paint_get_stroke_join (ptr paint)
  unmarshalSKEnumOrDie r

-- | Sets the geometry drawn at the corners of strokes.
setStrokeJoin :: (MonadIO m) => SKPaint -> SKStrokeJoin -> m ()
setStrokeJoin paint join = evalManaged do
  liftIO $ sk_paint_set_stroke_join (ptr paint) (marshalSKEnum join)

-- | Returns optional colors used when filling a path, such as a gradient.
getShader ::
  (MonadResource m) =>
  SKPaint ->
  -- | returns SkShader if previously set, 'Nothing' otherwise
  m (Maybe (ReleaseKey, SKShader))
getShader paint = do
  -- NOTE: sk_paint_get_shader does refShader()
  allocateSKObjectOrNothingIfNull'
    (sk_paint_get_shader (ptr paint))
    SKRefCnt.decrement

-- | Sets optional colors used when filling a path, such as a gradient.
setShader ::
  (MonadIO m) =>
  SKPaint ->
  -- shader; how geometry is filled with color; if 'Nothing', color is used
  -- instead
  Maybe SKShader ->
  m ()
setShader paint shader = liftIO do
  -- NOTE: Previous item's refcnt is decremented, the input item's refcnt is
  -- incremented.
  sk_paint_set_shader (ptr paint) (ptrOrNull shader)

-- | Returns SkMaskFilter if set, or 'Nothing'.
getMaskFilter :: (MonadResource m) => SKPaint -> m (Maybe (ReleaseKey, SKMaskFilter))
getMaskFilter paint =
  -- NOTE: sk_paint_get_maskfilter does refMaskFilter()
  allocateSKObjectOrNothingIfNull'
    (sk_paint_get_maskfilter (ptr paint))
    SKRefCnt.decrement

{- | Sets SkMaskFilter to maskFilter or to nothing. decreasing SkRefCnt of the
previous SkMaskFilter. Pass 'Nothing' to clear SkMaskFilter and leave
SkMaskFilter effect on mask alpha unaltered.
-}
setMaskFilter :: (MonadIO m) => SKPaint -> Maybe SKMaskFilter -> m ()
setMaskFilter paint maskFilter = liftIO do
  -- NOTE: Previous item's refcnt is decremented, the input item's refcnt is
  -- incremented.
  sk_paint_set_maskfilter (ptr paint) (ptrOrNull maskFilter)

-- | Returns SkImageFilter if set, or 'Nothing'.
getImageFilter :: (MonadResource m) => SKPaint -> m (Maybe (ReleaseKey, SKImageFilter))
getImageFilter paint =
  -- NOTE: sk_paint_get_colorfilter does ref()
  allocateSKObjectOrNothingIfNull'
    (sk_paint_get_imagefilter (ptr paint))
    SKRefCnt.decrement

-- | Sets SkImageFilter to dictate how SkImage is sampled when transformed
setImageFilter :: (MonadIO m) => SKPaint -> Maybe SKImageFilter -> m ()
setImageFilter paint imageFilter = liftIO do
  -- NOTE: Previous item's refcnt is decremented, the input item's refcnt is
  -- incremented.
  liftIO $ sk_paint_set_imagefilter (ptr paint) (ptrOrNull imageFilter)

-- | Returns SkColorFilter if set, otherwise 'Nothing'.
getColorFilter :: (MonadResource m) => SKPaint -> m (Maybe (ReleaseKey, SKColorFilter))
getColorFilter paint =
  -- NOTE: sk_paint_get_colorfilter does ref()
  allocateSKObjectOrNothingIfNull'
    (sk_paint_get_colorfilter (ptr paint))
    SKRefCnt.decrement

-- | Sets SkColorFilter to filter,
setColorFilter :: (MonadIO m) => SKPaint -> Maybe SKColorFilter -> m ()
setColorFilter paint colorFilter = evalManaged do
  -- NOTE: Previous item's refcnt is decremented, the input item's refcnt is
  -- incremented.
  liftIO $ sk_paint_set_colorfilter (ptr paint) (ptrOrNull colorFilter)

{- | Queries the blender, and if it can be represented as a SkBlendMode, return that mode,
else return the 'SKBlendMode'SrcOver' provided.
-}
getBlendMode :: (MonadIO m) => SKPaint -> m SKBlendMode
getBlendMode paint = liftIO do
  r <- sk_paint_get_blendmode (ptr paint)
  unmarshalSKEnumOrDie r

{- | This sets a blender that implements the specified blendmode enum.

Note from Google Skia: This is a helper method for calling 'setBlender'.
-}
setBlendMode :: (MonadIO m) => SKPaint -> SKBlendMode -> m ()
setBlendMode paint blendMode = liftIO do
  liftIO $ sk_paint_set_blendmode (ptr paint) (marshalSKEnum blendMode)

-- | Returns the user-supplied blend function, if one has been set.
getBlender :: (MonadResource m) => SKPaint -> m (Maybe (ReleaseKey, SKBlender))
getBlender paint =
  -- sk_paint_get_blender uses refBlender()
  allocateSKObjectOrNothingIfNull'
    (sk_paint_get_blender (ptr paint))
    SKRefCnt.decrement

{- | Sets the current blender.

For convenience, you can call 'setBlendMode' if the blend effect can be
expressed as one of those values.
-}
setBlender :: (MonadIO m) => SKPaint -> Maybe SKBlender -> m ()
setBlender paint blender = liftIO do
  -- NOTE: Previous item's refcnt is decremented, the input item's refcnt is
  -- incremented.
  sk_paint_set_blender (ptr paint) (ptrOrNull blender)

-- | Returns SkPathEffect is set, or 'Nothing'.
getPathEffect :: (MonadResource m) => SKPaint -> m (Maybe (ReleaseKey, SKPathEffect))
getPathEffect paint =
  -- sk_paint_get_path_effect uses refPathEffect()
  allocateSKObjectOrNothingIfNull'
    (sk_paint_get_path_effect (ptr paint))
    SKRefCnt.decrement

-- Sets SkPathEffect.
setPathEffect :: (MonadIO m) => SKPaint -> Maybe SKPathEffect -> m ()
setPathEffect paint pathEffect = liftIO do
  -- NOTE: Previous item's refcnt is decremented, the input item's refcnt is
  -- incremented.
  sk_paint_set_path_effect (ptr paint) (ptrOrNull pathEffect)

{- | Applies any and all effects to a source path, returning the result in the
destination.

Returns true if the path should be filled, or false if it should be drawn with a
hairline.
-}
getFillPath ::
  (MonadIO m) =>
  SKPaint ->
  -- | Source path
  SKPath ->
  -- | Destination path
  SKPath ->
  -- | The destination path may be culled to this rectangle.
  Maybe (Rect Float) ->
  -- | Transformation matrix
  M33 Float ->
  m Bool
getFillPath paint src dst cullRect transform = evalManaged do
  cullRect' <- useNullIfNothing storable $ toSKRect <$> cullRect
  transform' <- storable $ toSKMatrix transform
  r <- liftIO $ sk_paint_get_fill_path (ptr paint) (ptr src) (ptr dst) cullRect' transform'
  pure $ toBool r

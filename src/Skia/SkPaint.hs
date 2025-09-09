module Skia.SkPaint where

import Language.C.Inline.Cpp qualified as C
import NeatInterpolation
import Skia.Internal.Prelude
import Skia.Internal.THUtils
import Skia.Linear
import Skia.SkBlendMode
import Skia.SkColor
import Skia.SkRect
import Skia.SkRefCnt qualified as SkRefCnt

C.context $
  mconcat
    [ C.cppCtx
    , cppSkiaObjectTypes
    ]

C.include "core/SkBlender.h"
C.include "core/SkColorFilter.h"
C.include "core/SkImageFilter.h"
C.include "core/SkMaskFilter.h"
C.include "core/SkMatrix.h"
C.include "core/SkPaint.h"
C.include "core/SkPathEffect.h"
C.include "core/SkRect.h"
C.include "core/SkShader.h"
C.include "core/SkPathUtils.h"

create :: (MonadResource m) => m (ReleaseKey, SkPaint)
create =
  allocateSkObjectNeverNull
    [C.exp| SkPaint* { new SkPaint() }|]
    (\p -> [C.exp| void { delete $(SkPaint* p); }|])

{- | Returns true if pixels on the active edges of SkPath may be drawn with
partial transparency.
-}
isAntialias :: (MonadIO m) => SkPaint -> m Bool
isAntialias (ptr -> cpaint) = liftIO do
  toBool
    <$> [C.block| bool {
    return $(SkPaint* cpaint)->isAntiAlias();
  }|]

{- | Requests, but does not require, that edge pixels draw opaque or with
partial transparency.
-}
setAntialias :: (MonadIO m) => SkPaint -> Bool -> m ()
setAntialias (ptr -> cpaint) (fromBool -> antialias) = liftIO do
  [C.block| void {
    $(SkPaint* cpaint)->setAntiAlias($(bool antialias));
  }|]

-- | Returns true if color error may be distributed to smooth color transition.
isDither :: (MonadIO m) => SkPaint -> m Bool
isDither (ptr -> cpaint) = liftIO do
  toBool
    <$> [C.block| bool {
    return $(SkPaint* cpaint)->isDither();
  }|]

-- | Requests, but does not require, to distribute color error.
setDither :: (MonadIO m) => SkPaint -> Bool -> m ()
setDither (ptr -> cpaint) (fromBool -> dither) = liftIO do
  [C.block| void {
    $(SkPaint* cpaint)->setDither($(bool dither));
  }|]

-- TODO: Is this an enum abused as a flag?
$( qGenerateSkEnum
    "Style"
    [text|
    Set Style to fill, stroke, or both fill and stroke geometry.
    
    The stroke and fill share all paint attributes; for instance, they are drawn with the same color.

    Use 'Style'StrokeAndFill' to avoid hitting the same pixels twice with a stroke draw and
    a fill draw.
  |]
    [ ("Fill", "SkPaint::Style::kFill_Style", "set to fill geometry")
    , ("Stroke", "SkPaint::Style::kStroke_Style", "set to stroke geometry")
    , ("StrokeAndFill", "SkPaint::Style::kStrokeAndFill_Style", "sets to stroke and fill geometry")
    ]
 )

{- | Sets whether the geometry is filled, stroked, or filled and stroked. Has no
effect if style is not a legal SkPaint::Style value.
-}
setStyle :: (MonadIO m) => SkPaint -> Style -> m ()
setStyle (ptr -> cpaint) (marshalSkEnum -> style) = liftIO do
  [C.block| void {
    $(SkPaint* cpaint)->setStyle((SkPaint::Style) $(int style));
  }|]

-- | Returns whether the geometry is filled, stroked, or filled and stroked.
getStyle :: (MonadIO m) => SkPaint -> m Style
getStyle (ptr -> cpaint) = liftIO do
  style <-
    [C.block| int {
    return $(SkPaint* cpaint)->getStyle();
  }|]
  unmarshalSkEnumOrDie style

-- | Retrieves alpha and RGB, unpremultiplied, packed into 32 bits.
getColor :: (MonadIO m) => SkPaint -> m SkColor
getColor (ptr -> cpaint) = liftIO do
  SkColor
    <$> [C.block| uint32_t {
    return $(SkPaint* cpaint)->getColor();
  }|]

{- | Sets alpha and RGB used when stroking and filling. The color is a 32-bit
value, unpremultiplied, packing 8-bit components for alpha, red, blue, and
green.
-}
setColor :: (MonadIO m) => SkPaint -> SkColor -> m ()
setColor (ptr -> cpaint) (SkColor color) = liftIO do
  [C.block| void {
    $(SkPaint* cpaint)->setColor( $(uint32_t color) );
  }|]

{- | Retrieves alpha and RGB, unpremultiplied, as four floating point values.
RGB are extended sRGB values (sRGB gamut, and encoded with the sRGB transfer
function).
-}
getColorRGBA :: (MonadIO m) => SkPaint -> m (RGBA Float)
getColorRGBA (ptr -> cpaint) = evalManaged do
  c <- allocaSkColor4f
  liftIO
    [C.block| void {
    *$(SkColor4f* c) = $(SkPaint* cpaint)->getColor4f();
  }|]
  coerce <$> peekSkColor4f c

-- | 'setColorRGBA' is to 'setColor' as 'getColorRGBA' is to 'getColor'.
setColorRGBA ::
  (MonadIO m) =>
  SkPaint ->
  RGBA Float ->
  Maybe SkColorSpace ->
  m ()
setColorRGBA (ptr -> cpaint) (coerce -> RGBA r g b a) (ptrOrNull -> colorspace') = evalManaged do
  liftIO
    [C.block| void {
    SkColor4f color { $(float r), $(float g), $(float b), $(float a) };
    $(SkPaint* cpaint)->setColor4f(color, $(SkColorSpace* colorspace'));
  }|]

-- | Returns the thickness of the pen used by SkPaint to outline the shape.
getStrokeWidth :: (MonadIO m) => SkPaint -> m Float
getStrokeWidth (ptr -> cpaint) = liftIO do
  coerce
    <$> [C.block| float {
    return $(SkPaint* cpaint)->getStrokeWidth();
  }|]

{- | Sets the thickness of the pen used by the paint to outline the shape.
A stroke-width of zero is treated as "hairline" width. Hairlines are always exactly one
pixel wide in device space (their thickness does not change as the canvas is scaled).
Negative stroke-widths are invalid; setting a negative width will have no effect.
-}
setStrokeWidth ::
  (MonadIO m) =>
  SkPaint ->
  -- | zero thickness for hairline; greater than zero for pen thickness
  Float ->
  m ()
setStrokeWidth (ptr -> cpaint) (coerce -> width) = liftIO do
  [C.block| void {
    $(SkPaint* cpaint)->setStrokeWidth($(float width));
  }|]

-- | Returns the limit at which a sharp corner is drawn beveled.
getStrokeMiter :: (MonadIO m) => SkPaint -> m Float
getStrokeMiter (ptr -> cpaint) = liftIO do
  coerce
    <$> [C.block| float {
    return $(SkPaint* cpaint)->getStrokeMiter();
  }|]

{- | Sets the limit at which a sharp corner is drawn beveled. Valid values are
zero and greater. Has no effect if miter is less than zero.
-}
setStrokeMiter :: (MonadIO m) => SkPaint -> Float -> m ()
setStrokeMiter (ptr -> cpaint) (coerce -> miter) = liftIO do
  [C.block| void {
    $(SkPaint* cpaint)->setStrokeMiter($(float miter));
  }|]

$( qGenerateSkEnum
    "Cap"
    [trimming|
    This enum is relevant in 'Skia.SkPaint'.

    Cap draws at the beginning and end of an open path contour.
  |]
    [ ("Butt", "SkPaint::Cap::kButt_Cap", "No stroke extension")
    , ("Round", "SkPaint::Cap::kRound_Cap", "Adds circle")
    , ("Square", "SkPaint::Cap::kSquare_Cap", "Adds square")
    ]
 )

-- | Returns the geometry drawn at the beginning and end of strokes.
getStrokeCap :: (MonadIO m) => SkPaint -> m Cap
getStrokeCap (ptr -> cpaint) = liftIO do
  r <-
    [C.block| int {
    return $(SkPaint* cpaint)->getStrokeCap();
  }|]
  unmarshalSkEnumOrDie r

-- | Sets the geometry drawn at the beginning and end of strokes.
setStrokeCap :: (MonadIO m) => SkPaint -> Cap -> m ()
setStrokeCap (ptr -> cpaint) (marshalSkEnum -> cap) = liftIO do
  [C.block| void {
    $(SkPaint* cpaint)->setStrokeCap((SkPaint::Cap) $(int cap));
  }|]

$( qGenerateSkEnum
    "Join"
    [trimming|
    This enum is relevant in 'Skia.SkPaint'.

    Join specifies how corners are drawn when a shape is stroked. Join
    affects the four corners of a stroked rectangle, and the connected segments in a
    stroked path.

    Choose miter join to draw sharp corners. Choose round join to draw a circle with a
    radius equal to the stroke width on top of the corner. Choose bevel join to minimally
    connect the thick strokes.

    The fill path constructed to describe the stroked path respects the join setting but may
    not contain the actual join. For instance, a fill path constructed with round joins does
    not necessarily include circles at each connected segment.
  |]
    [ ("Miter", "SkPaint::Join::kMiter_Join", "Extends to miter limit")
    , ("Round", "SkPaint::Join::kRound_Join", "Adds circle")
    , ("Bevel", "SkPaint::Join::kBevel_Join", "Connects outside edges")
    ]
 )

-- | Returns the geometry drawn at the corners of strokes.
getStrokeJoin :: (MonadIO m) => SkPaint -> m Join
getStrokeJoin (ptr -> cpaint) = liftIO do
  r <-
    [C.block| int {
    return $(SkPaint* cpaint)->getStrokeJoin();
  }|]
  unmarshalSkEnumOrDie r

-- | Sets the geometry drawn at the corners of strokes.
setStrokeJoin :: (MonadIO m) => SkPaint -> Join -> m ()
setStrokeJoin (ptr -> cpaint) (marshalSkEnum -> join) = liftIO do
  [C.block| void {
    $(SkPaint* cpaint)->setStrokeJoin((SkPaint::Join) $(int join));
  }|]

-- | Returns optional colors used when filling a path, such as a gradient.
getShader ::
  (MonadResource m) =>
  SkPaint ->
  -- | returns SkShader if previously set, 'Nothing' otherwise
  m (Maybe (ReleaseKey, SkShader))
getShader (ptr -> cpaint) = do
  -- NOTE: sk_paint_get_shader does refShader()
  allocateSkObjectOrNothingIfNull'
    [C.exp| SkShader* {
      $(SkPaint* cpaint)->refShader().release()
    }|]
    SkRefCnt.decrement

-- | Sets optional colors used when filling a path, such as a gradient.
setShader ::
  (MonadIO m) =>
  SkPaint ->
  -- shader; how geometry is filled with color; if 'Nothing', color is used
  -- instead
  Maybe SkShader ->
  m ()
setShader (ptr -> cpaint) (ptrOrNull -> cshader) = liftIO do
  -- NOTE: Previous item's refcnt is decremented, the input item's refcnt is
  -- incremented.
  [C.block| void {
    $(SkPaint* cpaint)->setShader(sk_ref_sp($(SkShader* cshader)));
  }|]

-- | Returns SkMaskFilter if set, or 'Nothing'.
getMaskFilter :: (MonadResource m) => SkPaint -> m (Maybe (ReleaseKey, SkMaskFilter))
getMaskFilter (ptr -> cpaint) =
  -- NOTE: sk_paint_get_maskfilter does refMaskFilter()
  allocateSkObjectOrNothingIfNull'
    [C.exp| SkMaskFilter* {
      $(SkPaint* cpaint)->refMaskFilter().release()
    }|]
    SkRefCnt.increment

{- | Sets SkMaskFilter to maskFilter or to nothing. decreasing SkRefCnt of the
previous SkMaskFilter. Pass 'Nothing' to clear SkMaskFilter and leave
SkMaskFilter effect on mask alpha unaltered.
-}
setMaskFilter :: (MonadIO m) => SkPaint -> Maybe SkMaskFilter -> m ()
setMaskFilter (ptr -> cpaint) (ptrOrNull -> cmaskFilter) = liftIO do
  -- NOTE: Previous item's refcnt is decremented, the input item's refcnt is
  -- incremented.
  [C.block| void {
    $(SkPaint* cpaint)->setMaskFilter(sk_ref_sp($(SkMaskFilter* cmaskFilter)));
  }|]

-- | Returns SkImageFilter if set, or 'Nothing'.
getImageFilter :: (MonadResource m) => SkPaint -> m (Maybe (ReleaseKey, SkImageFilter))
getImageFilter (ptr -> cpaint) =
  allocateSkObjectOrNothingIfNull'
    [C.exp| SkImageFilter* {
      $(SkPaint* cpaint)->refImageFilter().release()
    }|]
    SkRefCnt.decrement

-- | Sets SkImageFilter to dictate how SkImage is sampled when transformed
setImageFilter :: (MonadIO m) => SkPaint -> Maybe SkImageFilter -> m ()
setImageFilter (ptr -> cpaint) (ptrOrNull -> cimageFilter) = liftIO do
  [C.block| void {
    $(SkPaint* cpaint)->setImageFilter(sk_ref_sp($(SkImageFilter* cimageFilter)));
  }|]

-- | Returns SkColorFilter if set, otherwise 'Nothing'.
getColorFilter :: (MonadResource m) => SkPaint -> m (Maybe (ReleaseKey, SkColorFilter))
getColorFilter (ptr -> cpaint) =
  -- NOTE: sk_paint_get_colorfilter does ref()
  allocateSkObjectOrNothingIfNull'
    [C.exp| SkColorFilter* {
      $(SkPaint* cpaint)->refColorFilter().release()
    }|]
    SkRefCnt.decrement

-- | Sets SkColorFilter to filter,
setColorFilter :: (MonadIO m) => SkPaint -> Maybe SkColorFilter -> m ()
setColorFilter (ptr -> cpaint) (ptrOrNull -> cfilter) = liftIO do
  [C.block| void {
    $(SkPaint* cpaint)->setColorFilter(sk_ref_sp($(SkColorFilter* cfilter)));
  }|]

{- | This sets a blender that implements the specified blendmode enum.

Note from Google Skia: This is a helper method for calling 'setBlender'.
-}
setBlendMode :: (MonadIO m) => SkPaint -> SkBlendMode -> m ()
setBlendMode (ptr -> cpaint) (marshalSkEnum -> cblendMode) = liftIO do
  [C.block| void {
    $(SkPaint* cpaint)->setBlendMode((SkBlendMode) $(int cblendMode));
  }|]

-- | Returns the user-supplied blend function, if one has been set.
getBlender :: (MonadResource m) => SkPaint -> m (Maybe (ReleaseKey, SkBlender))
getBlender (ptr -> cpaint) =
  allocateSkObjectOrNothingIfNull'
    [C.exp| SkBlender* {
      $(SkPaint* cpaint)->refBlender().release()
    }|]
    SkRefCnt.decrement

{- | Sets the current blender.

For convenience, you can call 'setBlendMode' if the blend effect can be
expressed as one of those values.
-}
setBlender :: (MonadIO m) => SkPaint -> Maybe SkBlender -> m ()
setBlender (ptr -> cpaint) (ptrOrNull -> cblender) = liftIO do
  [C.block| void {
    $(SkPaint* cpaint)->setBlender(sk_ref_sp($(SkBlender* cblender)));
  }|]

-- | Returns SkPathEffect is set, or 'Nothing'.
getPathEffect :: (MonadResource m) => SkPaint -> m (Maybe (ReleaseKey, SkPathEffect))
getPathEffect (ptr -> cpaint) =
  allocateSkObjectOrNothingIfNull'
    [C.exp| SkPathEffect* {
      $(SkPaint* cpaint)->refPathEffect().release()
    }|]
    SkRefCnt.decrement

-- | Sets SkPathEffect.
setPathEffect :: (MonadIO m) => SkPaint -> Maybe SkPathEffect -> m ()
setPathEffect (ptr -> cpaint) (ptrOrNull -> cpathEffect) = liftIO do
  [C.block| void {
    $(SkPaint* cpaint)->setPathEffect(sk_ref_sp($(SkPathEffect* cpathEffect)));
  }|]

{- | Applies any and all effects to a source path, returning the result in the
destination.

Returns true if the path should be filled, or false if it should be drawn with a
hairline.
-}
getFillPath ::
  (MonadIO m) =>
  SkPaint ->
  -- | Source path
  SkPath ->
  -- | Destination path
  SkPath ->
  -- | The destination path may be culled to this rectangle.
  Maybe (Rect Float) ->
  -- | Transformation matrix
  M33 Float ->
  m Bool
getFillPath (ptr -> cpaint) (ptr -> csrc) (ptr -> cdst) cullRect transform = evalManaged do
  ccullRect <- maybe (pure nullPtr) marshalSkRect cullRect
  ctransform <- marshalSkMatrix transform
  toBool
    <$> liftIO
      [C.exp| bool {
    skpathutils::FillPathWithPaint(
      *$(SkPath* csrc),
      *$(SkPaint* cpaint),
      $(SkPath* cdst),
      $(SkRect* ccullRect),
      *$(SkMatrix* ctransform)
    )
  }|]
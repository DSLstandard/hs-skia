module Skia.SkCanvas where

import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Language.C.Inline.Cpp qualified as C
import Skia.SkColor
import Skia.Internal.Prelude
import Skia.Linear
import Skia.SkRect
import Skia.SkBlendMode
import Skia.SkClipOp
import Skia.SkImageInfo
import Skia.SkSamplingOptions
import Skia.SkSurfaceProps
import Skia.SkFontTypes

C.context $ mconcat
  [ C.cppCtx
  , cppSkiaObjectTypes
  ]

C.include "core/SkCanvas.h"
C.include "core/SkBitmap.h"

{- | Constructs a canvas that draws into bitmap. Sets kUnknown_SkPixelGeometry
in constructed SkSurface.

SkBitmap is copied so that subsequently editing bitmap will not affect
constructed SkCanvas.

When 'Acquire' releases, the returned 'SkCanvas' is destroyed with 'sk_canvas_destroy'.

**Comment from Google Skia: May be deprecated in the future.**
-}
createFromBitmap :: (MonadResource m) => SkBitmap -> m (ReleaseKey, SkCanvas)
createFromBitmap (ptr -> cbitmap) =
  allocateSkObjectNeverNull
    [C.exp| SkCanvas* { new SkCanvas(*$(SkBitmap* cbitmap)) } |]
    (\ccanvas -> [C.exp| void { delete $(SkCanvas* ccanvas) } |])

{- | Allocates raster SkCanvas that will draw directly into pixels.

SkCanvas is returned if all parameters are valid. Valid parameters include: info
dimensions are zero or positive; info contains SkColorType and SkAlphaType
supported by raster surface; pixels is not nullptr; rowBytes is zero or large
enough to contain info width pixels of SkColorType.

Pass 'Nothing' for rowBytes to compute rowBytes from info width and size of
pixel. If rowBytes is 'Just', it must be equal to or greater than info width
times bytes required for SkColorType.

Pixel buffer size should be info height times computed rowBytes. Pixels are not
initialized. To access pixels after drawing, call flush() or peekPixels().
-}
createFromRaster ::
  (MonadResource m) =>
  -- | width, height, SkColorType, SkAlphaType, SkColorSpace, of raster
  -- surface;
  --
  -- width, or height, or both, may be zero
  ImageInfo ->
  -- | pixels. Pointer to destination pixels buffer
  Ptr Word8 ->
  -- | rowBytes. Optional. Interval from one SkSurface row to the next.
  Maybe Int ->
  -- | props. Optional. LCD striping orientation and setting for device independent fonts;
  Maybe SurfaceProps ->
  m (ReleaseKey, SkCanvas)
createFromRaster iminfo pixels rowBytes surfaceProps =
  allocateSkObjectOrErrorIfNull
    "Cannot create canvas from input"
    ( evalManaged do
        let cpixels :: Ptr () = castPtr pixels
        iminfo' <- marshalSKImageInfo iminfo
        surfaceProps' <- maybe (pure nullPtr) marshalSkSurfaceProps surfaceProps
        let crowBytes :: CInt = maybe 0 fromIntegral rowBytes
        liftIO [C.block|SkCanvas* {
          return SkCanvas::MakeRasterDirect(
            *$(SkImageInfo* iminfo'),
            $(void* cpixels),
            $(int crowBytes),
            $(SkSurfaceProps* surfaceProps')
          ).release();
        }|]
    )
    (\ccanvas -> [C.exp| void { delete $(SkCanvas* ccanvas) } |])

newtype StackDepth = StackDepth {unStackDepth :: Int}
  deriving (Show, Eq, Ord)
  deriving newtype (Num)

{- | Saves the current transform and clip.

Calling 'restore' discards changes to the current transform and clip, restoring
the current transform and clip to their state when 'save' was called.

The current transform may be changed by 'translate', 'scale', 'rotate', 'skew',
'concat', 'setMatrix', and 'resetMatrix'. Clip may be changed by 'clipRect',
'clipRRect', 'clipPath', 'clipRegion'.

Saved 'SkCanvas' state is put on a stack; multiple calls to 'save' should be
balance by an equal number of calls to 'restore'.

Call 'restoreToCount' with result to restore this and subsequent saves.
-}
save :: (MonadIO m, IsSkCanvas canvas) => canvas -> m StackDepth
save (ptr . toA SkCanvas -> canvas) = liftIO do
  fromIntegral <$> [C.block|int {
    return $(SkCanvas* canvas)->save();
  }|]

{- | Saves the current transform and clip, and allocates a 'SkSurface' for
subsequent drawing. Calling 'restore' discards changes to the transform and
clip, and draws the 'SkSurface'.

The current transform may be changed by 'translate', 'scale', 'rotate', 'skew',
'concat', 'setMatrix', and 'resetMatrix'.

Clip may be changed by 'clipRect', 'clipRRect', 'clipPath', 'clipRegion'.

\"bounds\" suggests but does not define the 'SkSurface' size. To clip drawing to
a specific rectangle, use 'clipRect'.

Optional 'SkPaint' paint applies alpha, SkColorFilter, SkImageFilter, and
SkBlendMode when 'restore' is called.

Call 'restoreToCount' with returned value to restore this and subsequent saves.
-}
saveLayer ::
  (MonadIO m, IsSkCanvas canvas) =>
  canvas ->
  -- | bounds. Optional. Hint to limit the size of the layer
  Maybe (Rect Float) ->
  -- | paint. Optional. Graphics state for layer
  Maybe SkPaint ->
  m StackDepth
saveLayer (ptr . toA SkCanvas -> ccanvas) bounds paint = evalManaged do
  cbounds <- maybe (pure nullPtr) marshalSkRect bounds
  let cpaint = ptrOrNull paint

  fromIntegral <$> liftIO [C.block|int {
    return $(SkCanvas* ccanvas)->saveLayer($(SkRect* cbounds), $(SkPaint* cpaint));
  }|]

-- | Auxillary structure in 'SaveLayerRec'.
data SaveLayerRecFlags = SaveLayerRecFlags
  { preserveLcdText :: Bool
  , initializeWithPrevious :: Bool
  -- ^ Initializes with previous contents?
  , useF16ColorType :: Bool
  -- ^ instead of matching previous layer's colortype, use F16
  }
  deriving (Show, Eq, Ord)

{- | 'SaveLayerRec' contains the state used to create a layer in 'SkCanvas'.

See function 'saveLayerRec'.
-}
data SaveLayerRec = SaveLayerRec
  { bounds :: Maybe (Rect Float)
  -- ^ Optional. Hint to limit the size of the layer
  , paint :: Maybe SkPaint
  -- ^ Optional. Modifies overlay.
  , backdrop :: Maybe SkImageFilter
  -- ^ If not 'Nothing', this triggers the same initialization behavior as
  -- setting kInitWithPrevious_SaveLayerFlag on fSaveLayerFlags: the current
  -- layer is copied into the new layer, rather than initializing the new
  -- layer with transparent-black. This is then filtered by fBackdrop
  -- (respecting the current clip).
  , preserveLCDText :: Bool
  -- ^ Preserves LCD text, creates with prior layer contents
  , flags :: SaveLayerRecFlags
  }

marshalSaveLayerRec :: SaveLayerRec -> Managed (Ptr C'SkCanvas'SaveLayerRec)
marshalSaveLayerRec i = evalManaged do
  fBounds <- maybe (pure nullPtr) marshalSkRect i.bounds
  let fPaint = ptrOrNull i.paint
  let fBackdrop = ptrOrNull i.backdrop

  let fPreserveLcdText :: CBool = fromBool i.flags.preserveLcdText
  let fInitializeWithPrevious :: CBool = fromBool i.flags.initializeWithPrevious
  let fUseF16ColorType :: CBool = fromBool i.flags.useF16ColorType

  managed $ bracket
    [C.block| SkCanvas::SaveLayerRec* {
      SkCanvas::SaveLayerRec* layer = new SkCanvas::SaveLayerRec;
      layer->fBounds = $(SkRect* fBounds);
      layer->fPaint = $(SkPaint* fPaint);
      layer->fBackdrop = $(SkImageFilter* fBackdrop);

      if ($(bool fPreserveLcdText)) {
        layer->fSaveLayerFlags |= SkCanvas::SaveLayerFlagsSet::kPreserveLCDText_SaveLayerFlag;
      }

      if ($(bool fInitializeWithPrevious)) {
        layer->fSaveLayerFlags |= SkCanvas::SaveLayerFlagsSet::kInitWithPrevious_SaveLayerFlag;
      }

      if ($(bool fUseF16ColorType)) {
        layer->fSaveLayerFlags |= SkCanvas::SaveLayerFlagsSet::kF16ColorType;
      }

      return layer;
    }|]
    ( \p -> do
      [C.block| void {
        delete $(SkCanvas::SaveLayerRec* p);
      }|]
    )

{- | Saves the current transform and clip, and allocates 'SkSurface' for
subsequent drawing.

Calling 'restore' discards changes to the current transform and clip, and blends
'SkSurface' with alpha opacity onto the prior layer.

The current transform may be changed by 'translate', 'scale', 'rotate', 'skew',
'concat', 'setMatrix', and 'resetMatrix'. Clip may be changed by 'clipRect',
'clipRRect', 'clipPath', 'clipRegion'.

The input SaveLayerRec contains the state used to create the layer.

Call 'restoreToCount' with returned value to restore this and subsequent saves.
-}
saveLayerRec :: 
  (MonadIO m, IsSkCanvas canvas) =>
  canvas ->
  SaveLayerRec ->
  m ()
saveLayerRec (ptr . toA SkCanvas -> ccanvas) savelayer = evalManaged do
  savelayer' <- marshalSaveLayerRec savelayer
  liftIO $ [C.block| void {
    $(SkCanvas* ccanvas)->saveLayer(*$(SkCanvas::SaveLayerRec* savelayer'));
  }|]

-- | Removes changes to SkMatrix and clip since SkCanvas state was last saved.
-- The state is removed from the stack.
--
-- Does nothing if the stack is empty.
restore :: (MonadIO m, IsSkCanvas canvas) => canvas -> m ()
restore (ptr . toA SkCanvas -> canvas) = liftIO do
  [C.block|void {
    $(SkCanvas* canvas)->restore();
  }|]

{- | Returns the number of saved states, each containing: SkMatrix and clip.
Equals the number of 'save' calls less the number of restore() calls plus
one. The save count of a new canvas is one.
-}
getSaveCount :: (MonadIO m, IsSkCanvas canvas) => canvas -> m StackDepth
getSaveCount (ptr . toA SkCanvas -> canvas) = liftIO do
  fmap StackDepth $ fromIntegral <$> [C.block|int {
    return $(SkCanvas* canvas)->getSaveCount();
  }|]

-- | Restores state to SkMatrix and clip values when save(), saveLayer(),
-- saveLayerPreserveLCDTextRequests(), or saveLayerAlpha() returned saveCount.
--
-- Does nothing if saveCount is greater than state stack count.
-- Restores state to initial values if saveCount is less than or equal to one.
--
-- @param 
restoreToCount ::
  (MonadIO m, IsSkCanvas canvas) =>
  canvas ->
  -- | saveCount; depth of state stack to restore
  StackDepth ->
  m ()
restoreToCount (ptr . toA SkCanvas -> canvas) (StackDepth (fromIntegral -> saveCount)) = liftIO do
  [C.block|void {
    $(SkCanvas* canvas)->restoreToCount($(int saveCount));
  }|]

-- | Translates the current transform by (dx, dy).
translate ::
  (MonadIO m, IsSkCanvas canvas) =>
  canvas ->
  -- | (dx, dy)
  V2 Float ->
  m ()
translate (ptr . toA SkCanvas -> ccanvas) (coerce -> V2 x y) = liftIO do
  [C.block|void {
    $(SkCanvas* ccanvas)->translate($(float x), $(float y));
  }|]

-- | Scales the current transform by (sx, sy).
scale ::
  (MonadIO m, IsSkCanvas canvas) =>
  canvas ->
  -- | (sx, sy)
  V2 Float ->
  m ()
scale (ptr . toA SkCanvas -> ccanvas) (coerce -> V2 x y) = liftIO do
  [C.block|void {
    $(SkCanvas* ccanvas)->scale($(float x), $(float y));
  }|]

{- | Rotates the current transform by degrees. Positive degrees rotates
clockwise.
-}
rotate ::
  (MonadIO m, IsSkCanvas canvas) =>
  canvas ->
  -- | degrees
  Float ->
  m ()
rotate (ptr . toA SkCanvas -> ccanvas) (coerce -> degrees) = do
  liftIO [C.block|void {
    $(SkCanvas* ccanvas)->rotate($(float degrees));
  }|]

{- | Skews the current transform by sx on the x-axis and sy on the y-axis. A
positive value of sx skews the drawing right as y-axis values increase; a
positive value of sy skews the drawing down as x-axis values increase.
-}
skew :: (MonadIO m, IsSkCanvas canvas) => canvas -> V2 Float -> m ()
skew (ptr . toA SkCanvas -> ccanvas) (coerce -> V2 x y) = do
  liftIO [C.block|void {
    $(SkCanvas* ccanvas)->skew($(float x), $(float y));
  }|]

{- | Premultiplies the input 4x4 matrix into the current transform.

TIP: If you only have a 'Linear.M33', consider using 'Linear.m33_to_m44'.
-}
concat :: (MonadIO m, IsSkCanvas canvas) => canvas -> M44 Float -> m ()
concat (ptr . toA SkCanvas -> canvas) matrix = evalManaged do
  matrix' <- marshalSkM44 matrix
  liftIO [C.block|void {
    $(SkCanvas* canvas)->concat(*$(SkM44* matrix'));
  }|]

{- | Replaces the current transform with the input 4x4 matrix.

Unlike 'concat', any prior matrix state is overwritten.

TIP: If you only have a 'Linear.M33', consider using 'Linear.m33_to_m44'.
-}
setMatrix :: (MonadIO m, IsSkCanvas canvas) => canvas -> M44 Float -> m ()
setMatrix (ptr . toA SkCanvas -> ccanvas) matrix = evalManaged do
  matrix' <- marshalSkM44 matrix
  liftIO [C.block|void {
    $(SkCanvas* ccanvas)->setMatrix(*$(SkM44* matrix'));
  }|]

{- | Sets the current transform to the identity matrix.

Any prior matrix state is overwritten.
-}
resetMatrix :: (MonadIO m, IsSkCanvas canvas) => canvas -> m ()
resetMatrix (ptr . toA SkCanvas -> ccanvas) = liftIO do
  [C.block|void {
    $(SkCanvas* ccanvas)->resetMatrix();
  }|]

{- | Replaces clip with the intersection or difference of clip and rect, with an
aliased or anti-aliased clip edge.

\"rect\" is transformed by the current transform before it is combined with clip.
-}
clipRect ::
  (MonadIO m, IsSkCanvas canvas) =>
  canvas ->
  -- | \"rect\". 'Rect' to combine with clip.
  Rect Float ->
  -- | Operation to apply to clip
  SkClipOp ->
  -- | true if clip is to be anti-aliased
  Bool ->
  m ()
clipRect (ptr . toA SkCanvas -> canvas) rect (marshalSkEnum -> cop) (fromBool -> doAA) = evalManaged do
  crect <- marshalSkRect rect
  liftIO [C.block|void {
    $(SkCanvas* canvas)->clipRect(*$(SkRect* crect), (SkClipOp) $(int cop), $(bool doAA));
  }|]

{- | Replaces clip with the intersection or difference of clip and rrect, with
an aliased or anti-aliased clip edge. rrect is transformed by SkMatrix before
it is combined with clip.
-}
clipRRect ::
  (MonadIO m, IsSkCanvas canvas) =>
  canvas ->
  -- | rrect. SkRRect to combine with clip
  SkRRect ->
  -- | op. SkClipOp to apply to clip
  SkClipOp ->
  -- | doAntialias. true if clip is to be anti-aliased
  Bool ->
  m ()
clipRRect (ptr . toA SkCanvas -> ccanvas) (ptr -> crrect) (marshalSkEnum -> cop) (fromBool -> doAA) = liftIO do
  [C.block|void {
    $(SkCanvas* ccanvas)->clipRRect(*$(SkRRect* crrect), (SkClipOp) $(int cop), $(bool doAA));
  }|]


{- | Replaces clip with the intersection or difference of clip and path, with an
aliased or anti-aliased clip edge.

The 'Skia.SkPath.getFillType' of the path determines if path describes the
area inside or outside its contours; and if path contour overlaps itself or
another path contour, whether the overlaps form part of the area. path is
transformed by the current transform before it is combined with clip.
-}
clipPath ::
  (MonadIO m, IsSkCanvas canvas) =>
  canvas ->
  -- | path to combine with clip
  SkPath ->
  -- | Operation to apply to clip
  SkClipOp ->
  -- | true if clip is to be anti-aliased
  Bool ->
  m ()
clipPath (ptr . toA SkCanvas -> ccanvas) (ptr -> cpath) (marshalSkEnum -> cop) (fromBool -> doAA) = liftIO do
  [C.block|void {
    $(SkCanvas* ccanvas)->clipPath(*$(SkPath* cpath), (SkClipOp) $(int cop), $(bool doAA));
  }|]

{- | Replaces clip with the intersection or difference of clip and SkRegion
deviceRgn. Resulting clip is aliased; pixels are fully contained by the clip.
deviceRgn is unaffected by SkMatrix.
-}
clipRegion ::
  (MonadIO m, IsSkCanvas canvas) =>
  canvas ->
  -- | deviceRgn. SkRegion to combine with clip
  SkRegion ->
  -- | SkClipOp to apply to clip
  SkClipOp ->
  m ()
clipRegion (ptr . toA SkCanvas -> ccanvas) (ptr -> cregion) (marshalSkEnum -> cop) = liftIO do
  [C.block|void {
    $(SkCanvas* ccanvas)->clipRegion(*$(SkRegion* cregion), (SkClipOp) $(int cop));
  }|]


{- | Returns true if SkRect rect, transformed by SkMatrix, can be quickly determined to be
outside of clip. May return false even though rect is outside of clip.

Use to check if an area to be drawn is clipped out, to skip subsequent draw calls.
-}
quickReject ::
  (MonadIO m, IsSkCanvas canvas) =>
  canvas ->
  -- | SkRect to compare with clip
  Rect Float ->
  m Bool
quickReject (ptr . toA SkCanvas -> ccanvas) rect = evalManaged do
  crect <- marshalSkRect rect
  liftIO $ fmap toBool [C.block|bool {
    return $(SkCanvas* ccanvas)->quickReject(*$(SkRect* crect));
  }|]

{- | Returns bounds of clip, transformed by inverse of SkMatrix. If clip is
empty, return 'Nothing'..

The returned bounds is outset by one to account for partial pixel coverage if
clip is anti-aliased.
-}
getLocalClipBounds :: (MonadIO m, IsSkCanvas canvas) => canvas -> m (Maybe (Rect Float))
getLocalClipBounds (ptr . toA SkCanvas -> ccanvas) = evalManaged do
  cbounds <- allocaSkRect

  isempty <- liftIO [C.block|bool {
    return $(SkCanvas* ccanvas)->getLocalClipBounds($(SkRect* cbounds));
  }|]

  if toBool isempty
    then pure Nothing
    else Just <$> peekSkRect cbounds

{- | Returns SkIRect bounds of clip, unaffected by SkMatrix. If clip is empty,
return 'Nothing'.

Unlike 'getLocalClipBounds', returned SkIRect is not outset.
-}
getDeviceClipBounds :: (MonadIO m, IsSkCanvas canvas) => canvas -> m (Maybe (Rect Int))
getDeviceClipBounds (ptr . toA SkCanvas -> ccanvas) = evalManaged do
  cbounds <- allocaSkIRect

  isempty <- liftIO [C.block|bool {
    return $(SkCanvas* ccanvas)->getDeviceClipBounds($(SkIRect* cbounds));
  }|]

  if toBool isempty
    then pure Nothing
    else Just <$> peekSkIRect cbounds

{- | Fills clip with color color. Input blend mode determines how ARGB is
combined with destination.
-}
drawColor :: (MonadIO m, IsSkCanvas canvas) => canvas -> SkColor -> SkBlendMode -> m ()
drawColor (ptr . toA SkCanvas -> ccanvas) (SkColor color) (marshalSkEnum -> blendMode) = liftIO do
  [C.block|void {
    $(SkCanvas* ccanvas)->drawColor((SkColor) $(uint32_t color), (SkBlendMode) $(int blendMode));
  }|]

-- | Like 'drawColor' but accepts 'RGBA'.
drawColorRGBA :: (MonadIO m, IsSkCanvas canvas) => canvas -> RGBA Float -> SkBlendMode -> m ()
drawColorRGBA (ptr . toA SkCanvas -> ccanvas) rgba (marshalSkEnum -> blendMode) = evalManaged do
  color4f <- marshalSkColor4f rgba
  liftIO [C.block|void {
    $(SkCanvas* ccanvas)->drawColor(*$(SkColor4f* color4f), (SkBlendMode) $(int blendMode));
  }|]

{- | Fills clip with the input color using 'SkBlendMode'Src'. This has the
effect of replacing all pixels contained by clip with color.
-}
clear :: (MonadIO m, IsSkCanvas canvas) => canvas -> SkColor -> m ()
clear (ptr . toA SkCanvas -> ccanvas) (SkColor color) = liftIO do
  [C.block|void {
    $(SkCanvas* ccanvas)->clear((SkColor) $(uint32_t color));
  }|]

-- | Like 'clear' but takes in 'RGBA'.
clearRGBA :: (MonadIO m, IsSkCanvas canvas) => canvas -> RGBA Float -> m ()
clearRGBA (ptr . toA SkCanvas -> ccanvas) rgba = evalManaged do
  color4f <- marshalSkColor4f rgba
  liftIO [C.block|void {
    $(SkCanvas* ccanvas)->clear(*$(SkColor4f* color4f));
  }|]

{- | Makes 'SkCanvas' contents undefined. Subsequent calls that read 'SkCanvas'
pixels, such as drawing with 'SkBlendMode', return undefined results. 'discard'
does not change clip or the transform.

'discard' may do nothing, depending on the implementation of 'SkSurface' or the
underlying Skia @SkDevice@ that created 'SkCanvas'.

'discard' allows optimized performance on subsequent draws by removing cached
data associated with 'SkSurface' or the underlying Skia @SkDevice@. It is not
necessary to call 'discard' once done with 'SkCanvas'; any cached data is
deleted when owning 'SkSurface' or the underlying Skia @SkDevice@ is deleted.
-}
discard :: (MonadIO m, IsSkCanvas canvas) => canvas -> m ()
discard (ptr . toA SkCanvas -> ccanvas) = liftIO do
  [C.block|void {
    $(SkCanvas* ccanvas)->discard();
  }|]

{- | Fills clip with SkPaint paint. SkPaint components, SkShader, SkColorFilter,
SkImageFilter, and SkBlendMode affect drawing; SkMaskFilter and SkPathEffect
in paint are ignored.
-}
drawPaint :: (MonadIO m, IsSkCanvas canvas) => canvas -> SkPaint -> m ()
drawPaint (ptr . toA SkCanvas -> ccanvas) (ptr -> cpaint) = liftIO do
  [C.block|void {
    $(SkCanvas* ccanvas)->drawPaint(*$(SkPaint* cpaint));
  }|]

{- | Draws point at (x, y) using clip, SkMatrix and SkPaint paint.

The shape of point drawn depends on paint SkPaint::Cap. If paint is set to
SkPaint::kRound_Cap, draw a circle of diameter SkPaint stroke width. If paint
is set to SkPaint::kSquare_Cap or SkPaint::kButt_Cap, draw a square of width
and height SkPaint stroke width. SkPaint::Style is ignored, as if were set to
SkPaint::kStroke_Style.
-}
drawPoint ::
  (MonadIO m, IsSkCanvas canvas) =>
  canvas ->
  -- | (x, y). left and top edge of circle or square
  V2 Float ->
  -- | Stroke, blend, color, and so on, used to draw
  SkPaint ->
  m ()
drawPoint (ptr . toA SkCanvas -> ccanvas) (coerce -> V2 x y) (ptr -> cpaint) = liftIO do
  [C.block|void {
    $(SkCanvas* ccanvas)->drawPoint($(float x), $(float y), *$(SkPaint* cpaint));
  }|]

{- | Draws line segment from (x0, y0) to (x1, y1) using clip, SkMatrix, and
SkPaint paint. In paint: SkPaint stroke width describes the line thickness;
SkPaint::Cap draws the end rounded or square; SkPaint::Style is ignored, as if
were set to SkPaint::kStroke_Style.
-}
drawLine ::
  (MonadIO m, IsSkCanvas canvas) =>
  canvas ->
  -- | (x0, y0). start of the line segment
  V2 Float ->
  -- | (x1, y1). end of the line segment
  V2 Float ->
  -- | Stroke, blend, color, and so on, used to draw
  SkPaint ->
  m ()
drawLine (ptr . toA SkCanvas -> ccanvas) (coerce -> V2 x0 y0) (coerce -> V2 x1 y1) (ptr -> cpaint) = liftIO do
  [C.block|void {
    $(SkCanvas* ccanvas)->drawLine($(float x0), $(float y0), $(float x1), $(float y1), *$(SkPaint* cpaint));
  }|]

{- | Draws SkRect rect using clip, SkMatrix, and SkPaint paint.
In paint: SkPaint::Style determines if rectangle is stroked or filled;
if stroked, SkPaint stroke width describes the line thickness, and
SkPaint::Join draws the corners rounded or square.
-}
drawRect ::
  (MonadIO m, IsSkCanvas canvas) =>
  canvas ->
  -- | Rectangle to draw
  Rect Float ->
  -- | Stroke, blend, color, and so on, used to draw
  SkPaint ->
  m ()
drawRect (ptr . toA SkCanvas -> ccanvas) rect (ptr -> cpaint) = evalManaged do
  crect <- marshalSkRect rect
  liftIO [C.block|void {
    $(SkCanvas* ccanvas)->drawRect(*$(SkRect* crect), *$(SkPaint* cpaint));
  }|]

{- | Draws SkRegion region using clip, SkMatrix, and SkPaint paint. In paint:
SkPaint::Style determines if rectangle is stroked or filled; if stroked, SkPaint
stroke width describes the line thickness, and SkPaint::Join draws the corners
rounded or square.
-}
drawRegion ::
  (MonadIO m, IsSkCanvas canvas) =>
  canvas ->
  -- | Region to draw
  SkRegion ->
  -- | SkPaint stroke or fill, blend, color, and so on, used to draw
  SkPaint ->
  m ()
drawRegion (ptr . toA SkCanvas -> ccanvas) (ptr -> cregion) (ptr -> cpaint) = liftIO do
  [C.block|void {
    $(SkCanvas* ccanvas)->drawRegion(*$(SkRegion* cregion), *$(SkPaint* cpaint));
  }|]

{- | Draws oval oval using clip, SkMatrix, and SkPaint. In paint: SkPaint::Style
determines if oval is stroked or filled; if stroked, SkPaint stroke width
describes the line thickness.
-}
drawOval ::
  (MonadIO m, IsSkCanvas canvas) =>
  canvas ->
  -- | Bounds of oval
  Rect Float ->
  -- | SkPaint stroke or fill, blend, color, and so on, used to draw
  SkPaint ->
  m ()
drawOval (ptr . toA SkCanvas -> ccanvas) rect (ptr -> cpaint) = evalManaged do
  crect <- marshalSkRect rect
  liftIO [C.block|void {
    $(SkCanvas* ccanvas)->drawOval(*$(SkRect* crect), *$(SkPaint* cpaint));
  }|]

{- | Draws SkRRect rrect using clip, SkMatrix, and SkPaint paint.
In paint: SkPaint::Style determines if rrect is stroked or filled;
if stroked, SkPaint stroke width describes the line thickness.

rrect may represent a rectangle, circle, oval, uniformly rounded rectangle, or
may have any combination of positive non-square radii for the four corners.
-}
drawRRect ::
  (MonadIO m, IsSkCanvas canvas) =>
  canvas ->
  -- | rrect. Rect with up to eight corner radii to draw
  SkRRect ->
  -- | SkPaint stroke or fill, blend, color, and so on, used to draw
  SkPaint ->
  m ()
drawRRect (ptr . toA SkCanvas -> ccanvas) (ptr -> crrect) (ptr -> cpaint) = liftIO do
  [C.block|void {
    $(SkCanvas* ccanvas)->drawRRect(*$(SkRRect* crrect), *$(SkPaint* cpaint));
  }|]

{- | Draws SkRRect outer and inner using clip, SkMatrix, and SkPaint paint.
outer must contain inner or the drawing is undefined. In paint:
SkPaint::Style determines if SkRRect is stroked or filled; if stroked,
SkPaint stroke width describes the line thickness. If stroked and SkRRect
corner has zero length radii, SkPaint::Join can draw corners rounded or
square.

GPU-backed platforms optimize drawing when both outer and inner are concave
and outer contains inner. These platforms may not be able to draw SkPath
built with identical data as fast.
-}
drawDRRect ::
  (MonadIO m, IsSkCanvas canvas) =>
  canvas ->
  -- | Outer
  SkRRect ->
  -- | Inner
  SkRRect ->
  -- | Stroke, blend, color, and so on, used to draw
  SkPaint ->
  m ()
drawDRRect (ptr . toA SkCanvas -> ccanvas) (ptr -> couter) (ptr -> cinner) (ptr -> cpaint) = liftIO do
  [C.block|void {
    $(SkCanvas* ccanvas)->drawDRRect(*$(SkRRect* couter), *$(SkRRect* cinner), *$(SkPaint* cpaint));
  }|]

{- | Draws circle at (cx, cy) with radius using clip, SkMatrix, and SkPaint
paint. If radius is zero or less, nothing is drawn. In paint: SkPaint::Style
determines if circle is stroked or filled; if stroked, SkPaint stroke width
describes the line thickness.
-}
drawCircle ::
  (MonadIO m, IsSkCanvas canvas) =>
  canvas ->
  -- | (cx, cy). Center center
  V2 Float ->
  -- | Circle radius
  Float ->
  -- | Stroke, blend, color, and so on, used to draw
  SkPaint ->
  m ()
drawCircle (ptr . toA SkCanvas -> ccanvas) (coerce -> V2 cx cy) (coerce -> radius) (ptr -> cpaint) = liftIO do
  [C.block|void {
    $(SkCanvas* ccanvas)->drawCircle($(float cx), $(float cy), $(float radius), *$(SkPaint* cpaint));
  }|]

{- | Draws arc using clip, SkMatrix, and SkPaint paint.

Arc is part of oval bounded by oval, sweeping from startAngle to startAngle plus
sweepAngle. startAngle and sweepAngle are in degrees.

startAngle of zero places start point at the right middle edge of oval. A
positive sweepAngle places arc end point clockwise from start point; a negative
sweepAngle places arc end point counterclockwise from start point. sweepAngle
may exceed 360 degrees, a full circle. If useCenter is true, draw a wedge that
includes lines from oval center to arc end points. If useCenter is false, draw
arc between end points.

If SkRect oval is empty or sweepAngle is zero, nothing is drawn.
-}
drawArc ::
  (MonadIO m, IsSkCanvas canvas) =>
  canvas ->
  -- | Oval
  Rect Float ->
  -- | Start angle
  Float ->
  -- | Sweep angle
  Float ->
  -- | Use center
  Bool ->
  SkPaint ->
  m ()
drawArc (ptr . toA SkCanvas -> ccanvas) rect (coerce -> startAngle) (coerce -> sweepAngle) (fromBool -> useCenter) (ptr -> cpaint) = evalManaged do
  crect <- marshalSkRect rect
  liftIO [C.block|void {
    $(SkCanvas* ccanvas)->drawArc(*$(SkRect* crect), $(float startAngle), $(float sweepAngle), $(bool useCenter), *$(SkPaint* cpaint));
  }|]

{- | Draws SkRRect bounded by SkRect rect, with corner radii (rx, ry) using
clip, SkMatrix, and SkPaint paint.

In paint: SkPaint::Style determines if SkRRect is stroked or filled; if stroked,
SkPaint stroke width describes the line thickness.

If rx or ry are less than zero, they are treated as if they are zero.

If rx plus ry exceeds rect width or rect height, radii are scaled down to fit.

If rx and ry are zero, SkRRect is drawn as SkRect and if stroked is affected by
SkPaint::Join.
-}
drawRoundRect ::
  (MonadIO m, IsSkCanvas canvas) =>
  canvas ->
  -- | Bounds of 'SK(ptr rrect) to draw.
  Rect Float ->
  -- | (rx, ry). Axis lengths on x-axis and y-axis of oval describing rounded corners.
  V2 Float ->
  -- | Stroke, blend, color, and so on, used to draw
  SkPaint ->
  m ()
drawRoundRect (ptr . toA SkCanvas -> ccanvas) rect (coerce -> V2 rx ry) (ptr -> cpaint) = evalManaged do
  crect <- marshalSkRect rect
  liftIO [C.block|void {
    $(SkCanvas* ccanvas)->drawRoundRect(*$(SkRect* crect), $(float rx), $(float ry), *$(SkPaint* cpaint));
  }|]

{- | Draws SkPath path using clip, SkMatrix, and SkPaint paint. SkPath contains
an array of path contour, each of which may be open or closed.

In paint: SkPaint::Style determines if SkRRect is stroked or filled: if
filled, SkPath::FillType determines whether path contour describes inside or
outside of fill; if stroked, SkPaint stroke width describes the line
thickness, SkPaint::Cap describes line ends, and SkPaint::Join describes how
corners are drawn.
-}
drawPath ::
  (MonadIO m, IsSkCanvas canvas) =>
  canvas ->
  -- | Path to draw
  SkPath ->
  -- | Stroke, blend, color, and so on, used to draw
  SkPaint ->
  m ()
drawPath (ptr . toA SkCanvas -> ccanvas) (ptr -> cpath) (ptr -> cpaint) = liftIO do
  [C.block|void {
    $(SkCanvas* ccanvas)->drawPath(*$(SkPath* cpath), *$(SkPaint* cpaint));
  }|]

-- | Draws image using clip, SkMatrix, and optional SkPaint paint.
--
-- If paint contains SkShader, SkColorFilter, or SkBlendMode, they are
-- applied to the image before it is drawn. The destination is the 
-- canvas bitmap or device.
drawImage ::
  (MonadIO m, IsSkCanvas canvas) =>
  canvas ->
  -- | Image to draw
  SkImage ->
  -- | (x, y). Left and top edge of image
  V2 Float ->
  -- | Sampling options to improve image drawing quality
  SamplingOptions ->
  -- | Paint containing SkShader, SkColorFilter, SkBlendMode, and so on
  SkPaint ->
  m ()
drawImage (ptr . toA SkCanvas -> ccanvas) (ptr -> cimage) (coerce -> V2 x y) sampling (ptr -> cpaint) = evalManaged do
  sampling' <- marshalSkSamplingOptions sampling
  liftIO [C.block|void {
    $(SkCanvas* ccanvas)->drawImage($(SkImage* cimage), $(float x), $(float y), *$(SkSamplingOptions* sampling'), $(SkPaint* cpaint));
  }|]

-- | Draws the image's subset src to the specified rectangle dst using clip, matrix,
-- and optional paint. If sampling contains MipmapMode, and image does not have
-- mipmap levels, or filtering is not kLinear, a temporary SkImage is created with
-- mipmap levels, and drawn.
drawImageRect ::
  (MonadIO m, IsSkCanvas canvas) =>
  canvas ->
  -- | Image to draw
  SkImage ->
  -- | Source bounds
  Rect Float ->
  -- | Destination bounds
  Rect Float ->
  -- | Sampling options to improve image drawing quality
  SamplingOptions ->
  -- | Paint containing SkShader, SkColorFilter, SkBlendMode, and so on
  SkPaint ->
  m ()
drawImageRect (ptr . toA SkCanvas -> ccanvas) (ptr -> cimage) srcRect dstRect sampling (ptr -> cpaint) = evalManaged do
  csrcRect <- marshalSkRect srcRect
  cdstRect <- marshalSkRect dstRect
  csampling <- marshalSkSamplingOptions sampling
  liftIO [C.block|void {
    $(SkCanvas* ccanvas)->drawImageRect($(SkImage* cimage), *$(SkRect* csrcRect), *$(SkRect* cdstRect), *$(SkSamplingOptions* csampling), $(SkPaint* cpaint), SkCanvas::kStrict_SrcRectConstraint);
  }|]

{- | Draws SkImage image stretched proportionally to fit into SkRect dst.
SkIRect center divides the image into nine sections: four sides, four corners, and
the center. Corners are unmodified or scaled down proportionately if their sides
are larger than dst; center and four sides are scaled to fit remaining space, if any.

Additionally transform draw using clip, SkMatrix, and optional SkPaint paint.

If SkPaint paint is supplied, apply SkColorFilter, alpha, SkImageFilter, and
SkBlendMode. If image is kAlpha_8_SkColorType, apply SkShader.
If paint contains SkMaskFilter, generate mask from image bounds.
Any SkMaskFilter on paint is ignored as is paint anti-aliasing state.

If generated mask extends beyond image bounds, replicate image edge colors, just
as SkShader made from SkImage::makeShader with SkShader::kClamp_TileMode set
replicates the image edge color when it samples outside of its bounds.
-}
drawImageNine ::
  (MonadIO m, IsSkCanvas canvas) =>
  canvas ->
  -- | image; SkImage containing pixels, dimensions, and format
  SkImage ->
  -- | center; SkIRect edge of image corners and sides
  Rect Int ->
  -- | dst; destination SkRect of image to draw to
  Rect Float ->
  -- | filter; what technique to use when sampling the image
  SKFilterMode ->
  -- | paint; Optional SkPaint containing SkBlendMode, SkColorFilter, SkImageFilter, and so on.
  Maybe SkPaint ->
  m ()
drawImageNine (ptr . toA SkCanvas -> ccanvas) (ptr -> cimage) center dst (marshalSkEnum -> filterMode) paint = evalManaged do
  ccenter <- marshalSkIRect center
  cdst <- marshalSkRect dst
  let cpaint = ptrOrNull paint
  liftIO [C.block|void {
    $(SkCanvas* ccanvas)->drawImageNine(
      $(SkImage* cimage), 
      *$(SkIRect* ccenter), 
      *$(SkRect* cdst), 
      (SkFilterMode) $(int filterMode),
      $(SkPaint* cpaint)
    );
  }|]

{- | Draws text, with origin at (x, y), using clip, SkMatrix, SkFont font, and
SkPaint paint.

When encoding is SkTextEncoding::kUTF8, SkTextEncoding::kUTF16, or
SkTextEncoding::kUTF32, this function uses the default character-to-glyph
mapping from the SkTypeface in font.  It does not perform typeface fallback for
characters not found in the SkTypeface. It does not perform kerning or other
complex shaping; glyphs are positioned based on their default advances.

Text meaning depends on SkTextEncoding.

Text size is affected by SkMatrix and SkFont text size. Default text size is 12
point.

All elements of paint: SkPathEffect, SkMaskFilter, SkShader, SkColorFilter, and
SkImageFilter; apply to text. By default, draws filled black glyphs.
-}
drawSimpleTextEncoding ::
  (MonadIO m, IsSkCanvas canvas) =>
  canvas ->
  -- | Text data
  BS.ByteString ->
  -- | Text encoding used in text data
  SkTextEncoding ->
  -- | Position
  V2 Float ->
  -- | Typeface, text size and so, used to describe the text
  SkFont ->
  -- | Blend, color, and so on, used to draw
  SkPaint ->
  m ()
drawSimpleTextEncoding (ptr . toA SkCanvas -> ccanvas) textData (marshalSkEnum -> encoding) (coerce -> V2 x y) (ptr -> cfont) (ptr -> cpaint) = evalManaged do
  (castPtr -> text', fromIntegral -> len) <- storableByteStringLen textData
  liftIO [C.block|void {
    $(SkCanvas* ccanvas)->drawSimpleText(
      $(void* text'),
      $(size_t len),
      (SkTextEncoding) $(int encoding),
      $(float x),
      $(float y),
      *$(SkFont* cfont),
      *$(SkPaint* cpaint)
    );
  }|]

-- | Convenience function. Like 'drawSimpleTextEncoding' but accepts 'T.Text'.
drawSimpleText ::
  (MonadIO m, IsSkCanvas canvas) =>
  canvas ->
  -- | Text to draw
  T.Text ->
  -- | Position
  V2 Float ->
  SkFont ->
  SkPaint ->
  m ()
drawSimpleText canvas text =
  -- FIXME: T.encodeUtf8 is O(n)...
  -- https://hackage-content.haskell.org/package/text-2.1.2/docs/src/Data.Text.Encoding.html#encodeUtf8
  drawSimpleTextEncoding canvas (T.encodeUtf8 text) SkTextEncoding'UTF8

{- | Draws SkPicture picture, using clip and SkMatrix; transforming picture with
SkMatrix matrix, if provided; and use 'SkPaint' paint alpha, 'SkColorFilter',
'SkImageFilter', and 'SkBlendMode', if provided.

If paint is not 'Nothing', then the picture is always drawn into a temporary
layer before actually landing on the canvas. Note that drawing into a layer can
also change its appearance if there are any non-associative blendModes inside
any of the pictures elements.
-}
drawPicture ::
  (MonadIO m, IsSkCanvas canvas) =>
  canvas ->
  -- | picture. The recorded drawing commands to play.
  SkPicture ->
  -- | matrix. Optional.
  Maybe (M33 Float) ->
  -- | paint. Optional.
  Maybe SkPaint ->
  m ()
drawPicture (ptr . toA SkCanvas -> ccanvas) (ptr -> cpicture) matrix paint = evalManaged do
  let cpaint = ptrOrNull paint
  cmatrix <- case matrix of
    Nothing -> pure nullPtr
    Just m -> marshalSkMatrix m
  liftIO [C.block|void {
    $(SkCanvas* ccanvas)->drawPicture($(SkPicture* cpicture), $(SkMatrix* cmatrix), $(SkPaint* cpaint));
  }|]

-- * Queries

{- | Returns true if clip is empty; that is, nothing will draw.

May do work when called; it should not be called more often than needed.
However, once called, subsequent calls perform no work until clip changes.
-}
isClipEmpty :: (MonadIO m, IsSkCanvas canvas) => canvas -> m Bool
isClipEmpty (ptr . toA SkCanvas -> ccanvas) = liftIO do
  fmap toBool [C.block|bool {
    return $(SkCanvas* ccanvas)->isClipEmpty();
  }|]

{- | Returns true if clip is 'Rect' and not empty.

Returns false if the clip is empty, or if it is not 'Rect'.
-}
isClipRect :: (MonadIO m, IsSkCanvas canvas) => canvas -> m Bool
isClipRect (ptr . toA SkCanvas -> ccanvas) = liftIO do
  fmap toBool [C.block|bool {
    return $(SkCanvas* ccanvas)->isClipRect();
  }|]

{- | Returns the current transform from local coordinates to the 'device', which
for most purposes means pixels.
-}
getLocalToDevice :: (MonadIO m, IsSkCanvas canvas) => canvas -> m (M44 Float)
getLocalToDevice (ptr . toA SkCanvas -> ccanvas) = evalManaged do
  matrix' <- allocaSkM44
  liftIO [C.block|void {
    *$(SkM44* matrix') = $(SkCanvas* ccanvas)->getLocalToDevice();
  }|]
  peekSkM44 matrix'

-- | Returns Ganesh context of the GPU surface associated with SkCanvas.
getRecordingContext ::
  (MonadIO m, IsSkCanvas canvas) =>
  canvas ->
  -- | GPU context, if available; 'Nothing' otherwise
  m (Maybe GrRecordingContext)
getRecordingContext (ptr . toA SkCanvas -> ccanvas) = liftIO do
  ctx <- [C.block|GrRecordingContext* {
    return $(SkCanvas* ccanvas)->recordingContext();
  }|]
  pure $ if ctx == nullPtr
    then Nothing
    else Just $ fromPtr ctx

{- | Sometimes a canvas is owned by a surface. If it is, this function will
return surface directly owned by the canvas, else this will return 'Nothing'.
-}
getSurface :: (MonadIO m, IsSkCanvas canvas) => canvas -> m (Maybe SkSurface)
getSurface (ptr . toA SkCanvas -> ccanvas) = liftIO do
  surface <- [C.block|SkSurface* {
    return $(SkCanvas* ccanvas)->getSurface();
  }|]
  pure $ if surface == nullPtr
    then Nothing
    else Just $ fromPtr surface

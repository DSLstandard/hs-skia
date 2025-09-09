module Skia.SkSurface where

import Control.Exception
import Control.Monad.Trans.Resource
import Linear
import Skia.Internal.Prelude
import Skia.SkRect
import Skia.SkImageInfo
import Skia.SkRefCnt qualified as SkRefCnt
import Skia.SkSurfaceProps
import Skia.SkColorType
import Skia.GrTypes
import Language.C.Inline.Cpp qualified as C

C.context $ C.cppCtx <> cppSkiaObjectTypes

C.include "core/SkSurface.h"
C.include "core/SkColorSpace.h"
C.include "gpu/ganesh/SkSurfaceGanesh.h"

-- * Creating 'SkSurface'

{- | Creates an 'SkSurface' without backing pixels. Drawing to 'SkCanvas'
returned by 'getCanvas' has no effect. Calling 'makeImageSnapshot' on returned
SkSurface is an illegal operation.

Throws 'SkiaError' unless the input dimensions are positive.
-}
createNull ::
  (MonadResource m) =>
  -- | Width
  Int ->
  -- | Height
  Int ->
  m (ReleaseKey, SkSurface)
createNull (fromIntegral -> width) (fromIntegral -> height) =
  allocateSkObjectNeverNull'
    ( do
        surface' <- [C.block|SkSurface* {
          return SkSurfaces::Null($(int width), $(int height)).release();
        }|]
        when (surface' == nullPtr) do
          throwIO $ SkiaError "Cannot create SkSurface"
        pure surface'
    )
    SkRefCnt.decrement

{- | Allocates raster 'SkSurface'. 'SkCanvas' returned by 'getCanvas' draws
directly into those allocated pixels, which are zeroed before use. Pixel memory
size is imageInfo.height() times imageInfo.minRowBytes() or rowBytes (if rowBytes is not 'Nothing').

Pixel memory is deleted when SkSurface is deleted.

Validity constraints include:

  * info dimensions are greater than zero;

  * info contains SkColorType and SkAlphaType supported by raster surface.

Throws 'SkiaError' unless parameters are valid and memory was allocated.
-}
createRaster ::
  (MonadResource m) =>
  -- | Width, height, SkColorType, SkAlphaType, SkColorSpace, of raster
  -- surface; width and height must be greater than zero
  ImageInfo ->
  -- | Optional. Row bytes. Interval from one SkSurface row to the next.
  Maybe Int ->
  -- | Optional. Specifies LCD striping orientation and setting for device
  -- independent fonts.
  Maybe SurfaceProps ->
  m (ReleaseKey, SkSurface)
createRaster iminfo rowBytes surfaceProps =
  allocateSkObjectNeverNull'
    ( evalManaged do
        iminfo' <- marshalSKImageInfo iminfo
        surfaceProps' <- maybe (pure nullPtr) marshalSkSurfaceProps surfaceProps
        let crowBytes :: CSize = maybe 0 fromIntegral rowBytes
        surface' <- liftIO $ [C.block|SkSurface* {
          return SkSurfaces::Raster(
            *$(const SkImageInfo* iminfo'),
            $(size_t crowBytes),
            $(const SkSurfaceProps* surfaceProps')
          ).release();
        }|]
        when (surface' == nullPtr) do
          liftIO $ throwIO $ SkiaError "Cannot create SkSurface"
        pure surface'
    )
    SkRefCnt.decrement

{- | Wraps a GPU-backed texture into SkSurface. Caller must ensure the texture
is valid for the lifetime of returned SkSurface. If sampleCnt greater than zero,
creates an intermediate MSAA SkSurface which is used for drawing backendTexture.

SkSurface is returned if all parameters are valid. backendTexture is valid if
its pixel configuration agrees with colorSpace and context; for instance, if
backendTexture has an sRGB configuration, then context must support sRGB, and
colorSpace must be present. Further, backendTexture width and height must not
exceed context capabilities, and the context must be able to support back-end
textures.

Upon success textureReleaseProc is called when it is safe to delete the texture
in the backend API (accounting only for use of the texture by this surface). If
SkSurface creation fails textureReleaseProc is called before this function
returns.

Throws 'SkiaError' unless all parameters are valid.
-}
wrapBackendTexture ::
  (MonadResource m, IsSubclassOf GrRecordingContext context) =>
  context ->
  GrBackendTexture ->
  GrSurfaceOrigin ->
  -- | Sample count
  Int ->
  SkColorType ->
  -- | Optional. Specifies the range of colors.
  Maybe SkColorSpace ->
  -- | Optional. Specifies LCD striping orientation and setting for device
  -- independent fonts.
  Maybe SurfaceProps ->
  m (ReleaseKey, SkSurface)
wrapBackendTexture (ptr . toA GrRecordingContext -> ctx) (ptr -> tex) (marshalSkEnum -> origin) (fromIntegral -> sampleCount) (marshalSkEnum -> colorType) (ptrOrNull -> colorspace) props =
  allocateSkObjectNeverNull'
    ( evalManaged do
        props' <- maybe (pure nullPtr) marshalSkSurfaceProps props

        surface' <- liftIO [C.block| SkSurface* {
          return SkSurfaces::WrapBackendTexture(
            $(GrRecordingContext* ctx),
            *$(GrBackendTexture* tex),
            (GrSurfaceOrigin) $(int origin),
            $(int sampleCount),
            (SkColorType) $(int colorType),
            sk_ref_sp($(SkColorSpace* colorspace)),
            $(const SkSurfaceProps* props')
          ).release();
        }|]

        when (surface' == nullPtr) do
          liftIO $ throwIO $ SkiaError "Cannot create SkSurface"
        pure surface'
    )
    SkRefCnt.decrement

{- | Wraps a GPU-backed buffer into 'SkSurface'. Caller must ensure
backendRenderTarget is valid for the lifetime of returned SkSurface.

SkSurface is returned if all parameters are valid. backendRenderTarget is valid
if its pixel configuration agrees with colorSpace and context; for instance, if
backendRenderTarget has an sRGB configuration, then context must support sRGB,
and colorSpace must be present. Further, backendRenderTarget width and height
must not exceed context capabilities, and the context must be able to support
back-end render targets.

Throws 'SkiaError' unless all parameters are valid.
-}
wrapBackendRenderTarget ::
  (MonadResource m, IsSubclassOf GrRecordingContext context) =>
  context ->
  GrBackendRenderTarget ->
  GrSurfaceOrigin ->
  SkColorType ->
  -- | Optional. Specifies the range of colors.
  Maybe SkColorSpace ->
  -- | Optional. Specifies LCD striping orientation and setting for device
  -- independent fonts.
  Maybe SurfaceProps ->
  m (ReleaseKey, SkSurface)
wrapBackendRenderTarget (ptr . toA GrRecordingContext -> ctx) (ptr -> target) (marshalSkEnum -> origin) (marshalSkEnum -> colorType) (ptrOrNull -> colorspace) props =
  allocateSkObjectNeverNull'
    ( evalManaged do
        props' <- maybe (pure nullPtr) marshalSkSurfaceProps props

        surface' <- liftIO [C.block| SkSurface* {
          return SkSurfaces::WrapBackendRenderTarget(
            $(GrRecordingContext* ctx),
            *$(GrBackendRenderTarget* target),
            (GrSurfaceOrigin) $(int origin),
            (SkColorType) $(int colorType),
            sk_ref_sp($(SkColorSpace* colorspace)),
            $(const SkSurfaceProps* props')
          ).release();
        }|]

        when (surface' == nullPtr) do
          liftIO $ throwIO $ SkiaError "Cannot create SkSurface"
        pure surface'
    )
    SkRefCnt.decrement

-- * Miscellaneous utilities

{- | Returns SkImage capturing SkSurface contents. Subsequent drawing to
SkSurface contents are not captured. SkImage allocation is accounted for if
SkSurface was created with skgpu::Budgeted::kYes.

Throws 'SkiaError' if a snapshot cannot be made (e.g., the 'SkSurface' is
created with 'createNull').
-}
makeImageSnapshot ::
  (MonadResource m) =>
  SkSurface ->
  -- | Optional crop bounds
  Maybe (Rect Int) ->
  m (ReleaseKey, SkImage)
makeImageSnapshot (ptr -> csurf) cropBounds =
  allocateSkObjectNeverNull'
    ( evalManaged do
        image' <- case cropBounds of
          Nothing -> do
            liftIO [C.exp|SkImage* {
              $(SkSurface* csurf)->makeImageSnapshot().release()
            }|]
          Just cropBounds -> do
            cropBounds' <- marshalSkIRect cropBounds
            liftIO [C.exp|SkImage* {
              $(SkSurface* csurf)->makeImageSnapshot(*$(SkIRect* cropBounds')).release()
            }|]
        when (image' == nullPtr) do
          liftIO $ throwIO $ SkiaError "Cannot create image snapshot"
        pure image'
    )
    SkRefCnt.decrement

-- | Draws SkSurface contents to canvas, with its top-left corner at (x, y).
drawToCanvas ::
  (MonadIO m, IsSkCanvas canvas) =>
  SkSurface ->
  -- | Canvas to draw to.
  canvas ->
  -- | (x, y)
  V2 Float ->
  -- | Optional. SkPaint containing SkBlendMode, SkColorFilter, SkImageFilter, and so on
  Maybe SkPaint ->
  m ()
drawToCanvas (ptr -> csurf) (ptr . toA SkCanvas -> ccanvas) (V2 (coerce -> x) (coerce -> y)) (ptrOrNull -> paint) = liftIO do
  [C.exp|void {
    $(SkSurface* csurf)->draw($(SkCanvas* ccanvas), $(float x), $(float y), $(SkPaint* paint))
  }|]

{- | Copies SkSurface pixel address, row bytes, and SkImageInfo to SkPixmap, if
address is available, and returns true. If pixel address is not available,
return false and leave SkPixmap unchanged.
-}
peekPixels ::
  (MonadIO m) =>
  SkSurface ->
  -- | Destination pixmap
  SkPixmap ->
  -- | Returns true on success.
  m Bool
peekPixels (ptr -> csurf) (ptr -> cpixmap) = liftIO do
  toBool <$> [C.exp|bool {
    $(SkSurface* csurf)->peekPixels($(SkPixmap* cpixmap))
  }|]

{- | Copies SkRect of pixels to dst.

Source SkRect corners are (srcX, srcY) and SkSurface (width(), height()).
Destination SkRect corners are (0, 0) and (dst.width(), dst.height()). Copies
each readable pixel intersecting both rectangles, without scaling, converting to
dst.colorType() and dst.alphaType() if required.

Pixels are readable when SkSurface is raster, or backed by a GPU.

The destination pixel storage must be allocated by the caller.

Pixel values are converted only if SkColorType and SkAlphaType do not match.
Only pixels within both source and destination rectangles are copied. dst
contents outside SkRect intersection are unchanged.

Pass negative values for srcX or srcY to offset pixels across or down
destination.

Does not copy, and returns false if:

  * Source and destination rectangles do not intersect.

  * SkPixmap pixels could not be allocated.

  * dst.rowBytes() is too small to contain one row of pixels.
-}
readPixels ::
  (MonadIO m) =>
  SkSurface ->
  -- | Destination's image info
  ImageInfo ->
  -- | Destination pixel buffer
  Ptr Word8 ->
  -- | Destination row bytes
  Int ->
  -- | (X, Y) source position
  V2 Int ->
  -- | Returns true on success.
  m Bool
readPixels (ptr -> csurf) iminfo (castPtr -> pixelBuffer) (fromIntegral -> rowBytes) (V2 (fromIntegral -> x) (fromIntegral -> y)) = evalManaged do
  iminfo' <- marshalSKImageInfo iminfo
  toBool <$> liftIO [C.block|bool {
    return $(SkSurface* csurf)->readPixels(
      *$(const SkImageInfo* iminfo'),
      $(void* pixelBuffer),
      $(size_t rowBytes),
      $(int x),
      $(int y)
    );
  }|]

-- | Returns 'SkSurfaceProps' for surface.
getProps :: (MonadIO m) => SkSurface -> m SurfaceProps
getProps (ptr -> csurf) = evalManaged do
  props' <- liftIO [C.exp|const SkSurfaceProps* {
    &($(SkSurface* csurf)->props())
  }|]
  peekSkSurfaceProps props'

{- | Acquires the 'SkCanvas' that draws into 'SkSurface'.

You **must not** 'Skia.SkCanvas.destroy' the returned 'SkCanvas'. The 'SkCanvas'
returned is managed and owned by 'SkSurface', and is deleted when 'SkSurface' is
deleted. Subsequent calls return the same 'SkCanvas'.

NOTE: 'SkSurface' must always outlive the 'SkCanvas', otherwise performing
operations on a 'SkCanvas' with a dead 'SkSurface' parent will result in a
segfault or a program crash. To mitigate this issue, this Haskell library
designs this function so that this 'Acquire' does a 'holdRef' on the
input 'SkSurface'. The input 'SkSurface' is kept alive as long as the 'Acquire'
of this 'SkCanvas' is alive.
-}
getCanvas :: (MonadResource m) => SkSurface -> m (ReleaseKey, SkCanvas)
getCanvas surf@(ptr -> csurf) = do
  key <- SkRefCnt.holdRef surf
  canvas' <- liftIO [C.exp|SkCanvas* {
    $(SkSurface* csurf)->getCanvas()
  }|]
  pure (key, SkCanvas canvas')

{- | Returns the recording context being used by the 'SkSurface'. Returns
'Nothing' otherwise.

NOTE: The returned 'GrRecordingContext', if it exists, is kept alive with
'holdRef' until paired 'ReleaseKey' is released.
-}
getRecordingContext :: (MonadResource m) => SkSurface -> m (Maybe (ReleaseKey, GrRecordingContext))
getRecordingContext (ptr -> csurf) = do
  ctx' <- liftIO [C.exp|GrRecordingContext* {
    $(SkSurface* csurf)->recordingContext()
  }|]
  if ctx' == nullPtr
    then pure Nothing
    else do
      let ctx = GrRecordingContext ctx'
      releaseKey <- SkRefCnt.holdRef ctx
      pure (Just (releaseKey, ctx))

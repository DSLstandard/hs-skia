module Skia.SKSurface where

import Control.Exception
import Control.Monad.Trans.Resource
import Linear
import Skia.Bindings.Sk_surface
import Skia.Internal.Prelude
import Skia.Rect
import Skia.SKImageInfo
import Skia.SKRefCnt qualified as SKRefCnt
import Skia.SKSurfaceProps

-- * Creating 'SKSurface'

{- | Creates an 'SKSurface' without backing pixels. Drawing to 'SKCanvas'
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
  m (ReleaseKey, SKSurface)
createNull width height =
  allocateSKObjectNeverNull
    ( do
        surface' <- sk_surface_new_null (fromIntegral width) (fromIntegral height)
        when (surface' == nullPtr) do
          throwIO $ SkiaError "Cannot create SKSurface"
        pure surface'
    )
    sk_surface_unref

{- | Allocates raster 'SKSurface'. 'SKCanvas' returned by 'getCanvas' draws
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
  SKImageInfo ->
  -- | Optional. Row bytes. Interval from one SkSurface row to the next.
  Maybe Int ->
  -- | Optional. Specifies LCD striping orientation and setting for device
  -- independent fonts.
  Maybe SKSurfaceProps ->
  m (ReleaseKey, SKSurface)
createRaster iminfo rowBytes surfaceProps =
  allocateSKObjectNeverNull
    ( evalManaged do
        iminfo' <- storable $ marshalSKImageInfo iminfo
        surfaceProps' <- useNullIfNothing useSKSurfaceProps surfaceProps
        surface' <- liftIO $ sk_surface_new_raster iminfo' (maybe 0 fromIntegral rowBytes) surfaceProps'
        when (surface' == nullPtr) do
          liftIO $ throwIO $ SkiaError "Cannot create SKSurface"
        pure surface'
    )
    sk_surface_unref

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
  (MonadResource m, IsSubclassOf GRRecordingContext context) =>
  context ->
  GRBackendTexture ->
  GRSurfaceOrigin ->
  -- | Sample count
  Int ->
  SKColorType ->
  -- | Optional. Specifies the range of colors.
  Maybe SKColorSpace ->
  -- | Optional. Specifies LCD striping orientation and setting for device
  -- independent fonts.
  Maybe SKSurfaceProps ->
  m (ReleaseKey, SKSurface)
wrapBackendTexture (toA GRRecordingContext -> ctx) tex origin sampleCount colorType colorspace props =
  allocateSKObjectNeverNull
    ( evalManaged do
        props' <- useNullIfNothing useSKSurfaceProps props
        surface' <-
          liftIO $
            sk_surface_new_backend_texture
              (ptr ctx)
              (ptr tex)
              (marshalSKEnum origin)
              (fromIntegral sampleCount)
              (marshalSKEnum colorType)
              (ptrOrNull colorspace)
              props'
        when (surface' == nullPtr) do
          liftIO $ throwIO $ SkiaError "Cannot create SKSurface"
        pure surface'
    )
    sk_surface_unref

{- | Wraps a GPU-backed buffer into 'SKSurface'. Caller must ensure
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
  (MonadResource m, IsSubclassOf GRRecordingContext context) =>
  context ->
  GRBackendRenderTarget ->
  GRSurfaceOrigin ->
  SKColorType ->
  -- | Optional. Specifies the range of colors.
  Maybe SKColorSpace ->
  -- | Optional. Specifies LCD striping orientation and setting for device
  -- independent fonts.
  Maybe SKSurfaceProps ->
  m (ReleaseKey, SKSurface)
wrapBackendRenderTarget (toA GRRecordingContext -> ctx) target origin colorType colorspace props =
  allocateSKObjectNeverNull
    ( evalManaged do
        props' <- useNullIfNothing useSKSurfaceProps props
        surface' <-
          liftIO $
            sk_surface_new_backend_render_target
              (ptr ctx)
              (ptr target)
              (marshalSKEnum origin)
              (marshalSKEnum colorType)
              (ptrOrNull colorspace)
              props'
        when (surface' == nullPtr) do
          liftIO $ throwIO $ SkiaError "Cannot create SKSurface"
        pure surface'
    )
    sk_surface_unref

{- |  Returns SkSurface on GPU indicated by context. Allocates memory for
pixels, based on the width, height, and SkColorType in SkImageInfo.  budgeted
selects whether allocation for pixels is tracked by context. imageInfo describes
the pixel format in SkColorType, and transparency in SkAlphaType, and color
matching in SkColorSpace.

sampleCount requests the number of samples per pixel. Pass zero to disable
multi-sample anti-aliasing.  The request is rounded up to the next supported
count, or rounded down if it is larger than the maximum supported count.

surfaceOrigin pins either the top-left or the bottom-left corner to the origin.

shouldCreateWithMips hints that SkImage returned by makeImageSnapshot() is mip
map.

Throws 'SkiaError' unless all parameters are valid.
-}
createFromRenderTarget ::
  (MonadResource m, IsSubclassOf GRRecordingContext context) =>
  context ->
  -- | Budgeted?
  Bool ->
  SKImageInfo ->
  -- | Sample count
  Int ->
  -- | surfaceOrigin
  GRSurfaceOrigin ->
  -- | Optional. Specifies LCD striping orientation and setting for device
  -- independent fonts.
  Maybe SKSurfaceProps ->
  -- | shouldCreateWithMips. Should create with mipmaps?
  Bool ->
  -- | Returns 'Nothing' unless all parameters are valid.
  m (ReleaseKey, SKSurface)
createFromRenderTarget (toA GRRecordingContext -> ctx) budgeted iminfo sampleCount origin surfaceProps shouldCreateWithMipmaps =
  allocateSKObjectNeverNull
    ( evalManaged do
        iminfo' <- storable $ marshalSKImageInfo iminfo
        surfaceProps' <- useNullIfNothing useSKSurfaceProps surfaceProps
        surface' <-
          liftIO $
            sk_surface_new_render_target
              (ptr ctx)
              (fromBool budgeted)
              iminfo'
              (fromIntegral sampleCount)
              (marshalSKEnum origin)
              surfaceProps'
              (fromBool shouldCreateWithMipmaps)
        when (surface' == nullPtr) do
          liftIO $ throwIO $ SkiaError "Cannot create SKSurface"
        pure surface'
    )
    sk_surface_unref

{- | Creates SkSurface from CAMetalLayer.

Returned SkSurface takes a reference on the CAMetalLayer. The ref on the layer
will be released when the SkSurface is destroyed.

Only available when Metal API is enabled.

Will grab the current drawable from the layer and use its texture as a backendRT
to create a renderable surface.
-}
wrapCAMetalLayer ::
  (MonadResource m, IsSubclassOf GRRecordingContext context) =>
  -- | GPU context
  context ->
  -- | Pointer to Metal layer (Expected to be a @CAMetalLayer*@ in C).
  Ptr () ->
  GRSurfaceOrigin ->
  -- | Sample count
  Int ->
  SKColorType ->
  -- | Optional. Specifies the range of colors.
  Maybe SKColorSpace ->
  -- | Optional. Specifies LCD striping orientation and setting for device
  -- independent fonts.
  Maybe SKSurfaceProps ->
  -- | Pointer to drawable to be filled in when this surface is instantiated;
  -- may not be nullptr.
  Ptr (Ptr ()) ->
  -- | Returns 'Nothing' unless all parameters are valid.
  m (ReleaseKey, SKSurface)
wrapCAMetalLayer (toA GRRecordingContext -> ctx) layer origin sampleCount colorType colorspace surfaceProps dstDrawable =
  allocateSKObjectNeverNull
    ( evalManaged do
        surfaceProps' <- useNullIfNothing useSKSurfaceProps surfaceProps
        surface' <-
          liftIO $
            sk_surface_new_metal_layer
              (ptr ctx)
              layer
              (marshalSKEnum origin)
              (fromIntegral sampleCount)
              (marshalSKEnum colorType)
              (ptrOrNull colorspace)
              surfaceProps'
              dstDrawable
        when (surface' == nullPtr) do
          liftIO $ throwIO $ SkiaError "Cannot create SKSurface"
        pure surface'
    )
    sk_surface_unref

{- | Creates SkSurface from MTKView.

Returned SkSurface takes a reference on the MTKView. The ref on the layer
will be released when the SkSurface is destroyed.

Only available when Metal API is enabled.

Will grab the current drawable from the layer and use its texture as a
backendRT to create a renderable surface.
-}
wrapMTKView ::
  (MonadResource m, IsSubclassOf GRRecordingContext context) =>
  context ->
  -- | Pointer to Metal view (Expected to be a @MTKView*@ in C).
  Ptr () ->
  GRSurfaceOrigin ->
  -- | Sample count
  Int ->
  SKColorType ->
  -- | Optional. Specifies the range of colors.
  Maybe SKColorSpace ->
  -- | Optional. Specifies LCD striping orientation and setting for device
  -- independent fonts.
  Maybe SKSurfaceProps ->
  -- | Returns 'Nothing' unless all parameters are valid.
  m (ReleaseKey, SKSurface)
wrapMTKView (toA GRRecordingContext -> ctx) view origin sampleCount colorType colorspace surfaceProps =
  allocateSKObjectNeverNull
    ( evalManaged do
        surfaceProps' <- useNullIfNothing useSKSurfaceProps surfaceProps
        surface' <-
          liftIO $
            sk_surface_new_metal_view
              (ptr ctx)
              view
              (marshalSKEnum origin)
              (fromIntegral sampleCount)
              (marshalSKEnum colorType)
              (ptrOrNull colorspace)
              surfaceProps'
        when (surface' == nullPtr) do
          liftIO $ throwIO $ SkiaError "Cannot create SKSurface"
        pure surface'
    )
    sk_surface_unref

-- * Miscellaneous utilities

{- | Returns SkImage capturing SkSurface contents. Subsequent drawing to
SkSurface contents are not captured. SkImage allocation is accounted for if
SkSurface was created with skgpu::Budgeted::kYes.

Throws 'SkiaError' if a snapshot cannot be made (e.g., the 'SKSurface' is
created with 'createNull').
-}
makeImageSnapshot ::
  (MonadResource m) =>
  SKSurface ->
  -- | Optional crop bounds
  Maybe (Rect Int) ->
  m (ReleaseKey, SKImage)
makeImageSnapshot surface cropBounds =
  allocateSKObjectNeverNull'
    ( evalManaged do
        image' <- case cropBounds of
          Nothing -> do
            liftIO $ sk_surface_new_image_snapshot (ptr surface)
          Just cropBounds -> do
            cropBounds' <- storable $ toSKIRect cropBounds
            liftIO $ sk_surface_new_image_snapshot_with_crop (ptr surface) cropBounds'
        when (image' == nullPtr) do
          liftIO $ throwIO $ SkiaError "Cannot create image snapshot"

        pure image'
    )
    SKRefCnt.decrement

-- | Draws SkSurface contents to canvas, with its top-left corner at (x, y).
drawToCanvas ::
  (MonadIO m, IsSKCanvas canvas) =>
  SKSurface ->
  -- | Canvas to draw to.
  canvas ->
  -- | (x, y)
  V2 Float ->
  -- | Optional. SkPaint containing SkBlendMode, SkColorFilter, SkImageFilter, and so on
  Maybe SKPaint ->
  m ()
drawToCanvas surface (toA SKCanvas -> canvas) (V2 x y) paint = liftIO do
  sk_surface_draw (ptr surface) (ptr canvas) (coerce x) (coerce y) (ptrOrNull paint)

{- | Copies SkSurface pixel address, row bytes, and SkImageInfo to SkPixmap, if
address is available, and returns true. If pixel address is not available,
return false and leave SkPixmap unchanged.
-}
peekPixels ::
  (MonadIO m) =>
  SKSurface ->
  -- | Destination pixmap
  SKPixmap ->
  -- | Returns true on success.
  m Bool
peekPixels surface pixmap = liftIO do
  success <- sk_surface_peek_pixels (ptr surface) (ptr pixmap)
  pure $ toBool success

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
  SKSurface ->
  -- | Destination's image info
  SKImageInfo ->
  -- | Destination pixel buffer
  Ptr Word8 ->
  -- | Destination row bytes
  Int ->
  -- | (X, Y) source position
  V2 Int ->
  -- | Returns true on success.
  m Bool
readPixels surface iminfo pixelBuffer rowBytes (V2 x y) = evalManaged do
  iminfo' <- storable $ marshalSKImageInfo iminfo
  success <-
    liftIO $
      sk_surface_read_pixels
        (ptr surface)
        iminfo'
        (castPtr pixelBuffer)
        (fromIntegral rowBytes)
        (fromIntegral x)
        (fromIntegral y)
  pure $ toBool success

-- | Returns 'SKSurfaceProps' for surface.
getProps :: (MonadIO m) => SKSurface -> m SKSurfaceProps
getProps surface = liftIO do
  props' <- sk_surface_get_props (ptr surface)
  peekSKSurfaceProps props'

{- | Acquires the 'SKCanvas' that draws into 'SKSurface'.

You **must not** 'Skia.SKCanvas.destroy' the returned 'SKCanvas'. The 'SKCanvas'
returned is managed and owned by 'SKSurface', and is deleted when 'SKSurface' is
deleted. Subsequent calls return the same 'SKCanvas'.

NOTE: 'SKSurface' must always outlive the 'SKCanvas', otherwise performing
operations on a 'SKCanvas' with a dead 'SKSurface' parent will result in a
segfault or a program crash. To mitigate this issue, this Haskell library
designs this function so that this 'Acquire' does a 'holdRef' on the
input 'SKSurface'. The input 'SKSurface' is kept alive as long as the 'Acquire'
of this 'SKCanvas' is alive.
-}
getCanvas :: (MonadResource m) => SKSurface -> m (ReleaseKey, SKCanvas)
getCanvas surface = do
  releaseKey <- SKRefCnt.holdRef surface
  canvas' <- liftIO $ sk_surface_get_canvas (ptr surface)
  pure (releaseKey, fromPtr canvas')

{- | Returns the recording context being used by the 'SKSurface'. Returns
'Nothing' otherwise.

NOTE: The returned 'GRRecordingContext', if it exists, is kept alive with
'holdRef' until paired 'ReleaseKey' is released.
-}
getRecordingContext :: (MonadResource m) => SKSurface -> m (Maybe (ReleaseKey, GRRecordingContext))
getRecordingContext surface = do
  -- TODO: Do we actually need to do a sk_refcnt_ref() here?
  ctx' <- liftIO $ sk_surface_get_recording_context (ptr surface)
  if ctx' == nullPtr
    then do
      pure Nothing
    else do
      let ctx = fromPtr ctx'
      releaseKey <- SKRefCnt.holdRef ctx
      pure (Just (releaseKey, ctx))

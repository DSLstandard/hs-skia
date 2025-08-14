module Skia.SKImage where

import Control.Monad.Trans.Resource
import Linear
import Skia.Bindings.Sk_image
import Skia.Internal.Prelude
import Skia.Rect
import Skia.SKImageInfo
import Skia.SKRefCnt qualified as SKRefCnt
import Skia.SKSamplingOptions
import Skia.SKSurfaceProps

{- | Creates a CPU-backed SkImage from pixmap, copying the pixel data. As a
result, pixmap pixels may be modified or deleted without affecting SkImage.

SkImage is returned if SkPixmap is valid. Valid SkPixmap parameters include:
dimensions are greater than zero; each dimension fits in 29 bits; SkColorType
and SkAlphaType are valid, and SkColorType is not kUnknown_SkColorType; row
bytes are large enough to hold one row of pixels; pixel address is not nullptr.
-}
createRasterFromPixmapCopy ::
  (MonadResource m) =>
  SKPixmap ->
  m (ReleaseKey, SKImage)
createRasterFromPixmapCopy pixmap =
  allocateSKObjectOrErrorIfNull'
    "Cannot create raster image from given pixmap"
    (sk_image_new_raster_copy_with_pixmap (ptr pixmap))
    SKRefCnt.decrement

{- | Creates CPU-backed SkImage from pixel data described by info. The pixels
data will *not* be copied.

SkImage is returned if SkImageInfo is valid. Valid SkImageInfo parameters
include: dimensions are greater than zero; each dimension fits in 29 bits;
SkColorType and SkAlphaType are valid, and SkColorType is not
kUnknown_SkColorType; rowBytes are large enough to hold one row of pixels;
pixels is not nullptr, and contains enough data for SkImage.
-}
createRasterFromData ::
  (MonadResource m) =>
  -- | info. Contains width, height, SkAlphaType, SkColorType, SkColorSpace
  SKImageInfo ->
  -- | pixels. Address or pixel storage
  Ptr Word8 ->
  -- | rowBytes. Size of pixel row or larger
  Int ->
  m (ReleaseKey, SKImage)
createRasterFromData iminfo pixels rowBytes =
  allocateSKObjectOrErrorIfNull'
    "Cannot create raster image from given data"
    ( evalManaged do
        iminfo' <- storable $ marshalSKImageInfo iminfo
        liftIO $ sk_image_new_raster_data iminfo' (castPtr pixels) (fromIntegral rowBytes)
    )
    SKRefCnt.decrement

{- | Creates a CPU-backed SkImage from bitmap, sharing or copying bitmap pixels.
If the bitmap is marked immutable, and its pixel memory is shareable, it may be
shared instead of copied.

SkImage is returned if bitmap is valid. Valid SkBitmap parameters include:
dimensions are greater than zero; each dimension fits in 29 bits;

SkColorType and SkAlphaType are valid, and SkColorType is not
kUnknown_SkColorType; row bytes are large enough to hold one row of pixels;
pixel address is not nullptr.
-}
createRasterFromBitmap ::
  (MonadResource m) =>
  -- | bitmap. SkImageInfo, row bytes, and pixels
  SKBitmap ->
  m (ReleaseKey, SKImage)
createRasterFromBitmap bitmap =
  allocateSKObjectOrErrorIfNull'
    "Cannot create SKImage from input SKBitmap"
    (sk_image_new_from_bitmap (ptr bitmap))
    SKRefCnt.decrement

{- | Return a SkImage using the encoded data, but attempts to defer decoding
until the image is actually used/drawn. This deferral allows the system to
cache the result, either on the CPU or on the GPU, depending on where the
image is drawn. If memory is low, the cache may be purged, causing the next
draw of the image to have to re-decode.

If alphaType is nullopt, the image's alpha type will be chosen automatically
based on the image format. Transparent images will default to
kPremul_SkAlphaType. If alphaType contains kPremul_SkAlphaType or
kUnpremul_SkAlphaType, that alpha type will be used. Forcing opaque (passing
kOpaque_SkAlphaType) is not allowed, and will return nullptr.

If the encoded format is not supported, 'Nothing' is returned.
-}
createDeferredFromEncodedData ::
  (MonadResource m) =>
  SKData ->
  m (ReleaseKey, SKImage)
createDeferredFromEncodedData dat =
  allocateSKObjectOrErrorIfNull'
    "Cannot create SKImage from given encoded data"
    (sk_image_new_from_encoded (ptr dat))
    SKRefCnt.decrement

{- | Creates GPU-backed SkImage from backendTexture associated with context.
Skia will assume ownership of the resource and will release it when no longer
needed. A 'Just' SkImage is returned if format of backendTexture is
recognized and supported. Recognized formats vary by GPU backend.
-}
adoptTextureFrom ::
  (MonadResource m, IsGRRecordingContext context) =>
  -- | GPU context
  context ->
  -- | backendTexture. texture residing on GPU
  GRBackendTexture ->
  -- | Origin of backendTexture
  GRSurfaceOrigin ->
  -- | Color type of the resulting image
  SKColorType ->
  -- | Alpha type of the resulting image
  SKAlphaType ->
  -- | Optional. Range of colors.
  Maybe SKColorSpace ->
  m (Maybe (ReleaseKey, SKImage))
adoptTextureFrom (toA GRRecordingContext -> ctx) tex origin colorType alphaType colorspace =
  allocateSKObjectOrNothingIfNull'
    ( sk_image_new_from_adopted_texture
        (ptr ctx)
        (ptr tex)
        (marshalSKEnum origin)
        (marshalSKEnum colorType)
        (marshalSKEnum alphaType)
        (ptrOrNull colorspace)
    )
    SKRefCnt.decrement

data BitDepth = BitDepth'F16 | BitDepth'U8
  deriving (Show, Ord, Eq, Enum, Bounded)

{- | Creates SkImage from picture. Returned SkImage width and height are set by
dimensions. SkImage draws picture with matrix and paint, set to bitDepth and
colorSpace.

The Picture data is not turned into an image (CPU or GPU) until it is drawn.

If matrix is 'Nothing', draws with identity SkMatrix. If paint is 'Nothing',
draws with default SkPaint. colorSpace may be 'Nothing'.
-}
createDeferredFromPicture ::
  (MonadResource m) =>
  SKPicture ->
  -- | Dimensions
  V2 Int ->
  -- | matrix. Optional. SkMatrix to rotate, scale, translate, and so on
  Maybe (M33 Float) ->
  -- | paint. Optional. SkPaint to apply transparency, filtering, and so on.
  Maybe SKPaint ->
  -- | 8-bit integer or 16-bit float: per component
  BitDepth ->
  -- | colorSpace. Optional. Range of colors
  Maybe SKColorSpace ->
  SKSurfaceProps ->
  m (Maybe (ReleaseKey, SKImage))
createDeferredFromPicture picture dimensions matrix paint bitDepth colorspace surfaceProps =
  allocateSKObjectOrNothingIfNull'
    ( evalManaged do
        dimensions' <- storable $ toSKISize $ fmap fromIntegral dimensions
        matrix' <- useNullIfNothing storable $ fmap toSKMatrix $ matrix
        surfaceProps' <- useSKSurfaceProps surfaceProps
        liftIO $
          sk_image_new_from_picture
            (ptr picture)
            dimensions'
            matrix'
            (ptrOrNull paint)
            ( fromBool case bitDepth of
                BitDepth'F16 -> True
                BitDepth'U8 -> False
            )
            (ptrOrNull colorspace)
            surfaceProps'
    )
    SKRefCnt.decrement

-- | Gets the image width.
getWidth :: (MonadIO m) => SKImage -> m Int
getWidth image = evalManaged do
  liftIO $ fromIntegral <$> sk_image_get_width (ptr image)

-- | Gets the image height.
getHeight :: (MonadIO m) => SKImage -> m Int
getHeight image = evalManaged do
  liftIO $ fromIntegral <$> sk_image_get_height (ptr image)

{- | Returns value unique to image. SkImage contents cannot change after SkImage
is created. Any operation to create a new SkImage will receive generate a new
unique number.
-}
getUniqueId ::
  (MonadIO m) =>
  -- | image
  SKImage ->
  m Word32
getUniqueId image = evalManaged do
  liftIO $ sk_image_get_unique_id (ptr image)

-- | Gets the configured SKAlphaType for the bitmap.
getAlphaType :: (MonadIO m) => SKImage -> m SKAlphaType
getAlphaType image = evalManaged do
  r <- liftIO $ sk_image_get_alpha_type (ptr image)
  unmarshalSKEnumOrDie r

-- | Gets the image color type.
getColorType :: (MonadIO m) => SKImage -> m SKColorType
getColorType image = evalManaged do
  r <- liftIO $ sk_image_get_color_type (ptr image)
  unmarshalSKEnumOrDie r

{- | Returns SkColorSpace, the range of colors, associated with SkImage. The
returned SKColorSpace is immutable.

SkColorSpace returned was passed to an SkImage constructor, or was parsed from
encoded data. SkColorSpace returned may be ignored when SkImage is drawn,
depending on the capabilities of the SkSurface receiving the drawing.
-}
getColorSpace :: (MonadResource m) => SKImage -> m (Maybe (ReleaseKey, SKColorSpace))
getColorSpace image =
  -- NOTE: sk_image_get_colorspace uses refColorSpace
  allocateSKObjectOrNothingIfNull'
    (sk_image_get_colorspace (ptr image))
    SKRefCnt.decrementNV

{- | Gets a value indicating whether the image will be drawn as a mask, with no
intrinsic color of its own
-}
isAlphaOnly :: (MonadIO m) => SKImage -> m Bool
isAlphaOnly image = liftIO do
  toBool <$> sk_image_is_alpha_only (ptr image)

-- | Make a shader with the specified tiling and mipmap sampling.
makeShader ::
  (MonadResource m) =>
  SKImage ->
  -- | X tile mode.
  SKShaderTileMode ->
  -- | Y tile mode.
  SKShaderTileMode ->
  SKSamplingOptions ->
  -- | Optional transform.
  Maybe (M33 Float) ->
  m (ReleaseKey, SKShader)
makeShader image tileX tileY sampling matrix =
  allocateSKObjectNeverNull'
    ( evalManaged do
        sampling' <- storable $ marshalSKSamplingOptions sampling
        matrix' <- useNullIfNothing storable $ fmap toSKMatrix $ matrix
        liftIO $ sk_image_make_shader (ptr image) (marshalSKEnum tileX) (marshalSKEnum tileY) sampling' matrix'
    )
    SKRefCnt.decrement

{- | 'makeRawShader' functions like 'makeShader', but for images that contain
non-color data. This includes images encoding things like normals, material
properties (eg, roughness), heightmaps, or any other purely mathematical data
that happens to be stored in an image. These types of images are useful with
some programmable shaders (see: 'SKRuntimeEffect').

Raw image shaders work like regular image shaders (including filtering and
tiling), with a few major differences:

  * No color space transformation is ever applied (the color space of the image
    is ignored).

  * Images with an alpha type of kUnpremul are *not* automatically
    premultiplied.

  * Bicubic filtering is not supported. If SkSamplingOptions::useCubic is true,
    these factories will return nullptr.
-}
makeRawShader ::
  (MonadResource m) =>
  SKImage ->
  -- | X tile mode.
  SKShaderTileMode ->
  -- | Y tile mode.
  SKShaderTileMode ->
  SKSamplingOptions ->
  -- | Optional transform.
  Maybe (M33 Float) ->
  m (ReleaseKey, SKShader)
makeRawShader image tileX tileY sampling matrix =
  -- TODO: Could it return nullptr?
  allocateSKObjectNeverNull'
    ( evalManaged do
        sampling' <- storable $ marshalSKSamplingOptions sampling
        matrix' <- useNullIfNothing storable $ fmap toSKMatrix $ matrix
        liftIO $ sk_image_make_raw_shader (ptr image) (marshalSKEnum tileX) (marshalSKEnum tileY) sampling' matrix'
    )
    SKRefCnt.decrement

{- | Returns true if the contents of SkImage was created on or uploaded to GPU
memory, and is available as a GPU texture.
-}
isTextureBacked :: (MonadIO m) => SKImage -> m Bool
isTextureBacked image = evalManaged do
  liftIO $ toBool <$> sk_image_is_texture_backed (ptr image)

{- | Returns true if SkImage is backed by an image-generator or other service
that creates and caches its pixels or texture on-demand.
-}
isLazyGenerated :: (MonadIO m) => SKImage -> m Bool
isLazyGenerated image = evalManaged do
  liftIO $ toBool <$> sk_image_is_lazy_generated (ptr image)

{- | Returns true if SkImage draws on GPU surface associated with context.

Also see 'isValidOnRaster'.

NOTE: SkImage backed by GPU texture may become invalid if associated context is
invalid. lazy image may be invalid and may not draw to raster surface or GPU
surface or both.
-}
isValidOnGPUContext ::
  (MonadIO m, IsGRRecordingContext context) =>
  SKImage ->
  -- | context. GPU context to test.
  context ->
  m Bool
isValidOnGPUContext image (toA GRRecordingContext -> ctx) = liftIO do
  toBool <$> sk_image_is_valid (ptr image) (ptr ctx)

{- | Analogous to 'isValidOnGPUContext', but tests if the 'SKImage' is drawn on
a raster surface.

NOTE: SkImage backed by GPU texture may become invalid if associated context is
invalid. lazy image may be invalid and may not draw to raster surface or GPU
surface or both.
-}
isValidOnRaster :: (MonadIO m) => SKImage -> m Bool
isValidOnRaster image = liftIO do
  toBool <$> sk_image_is_valid (ptr image) nullPtr

{- | Copies SkImage pixel address, row bytes, and SkImageInfo to pixmap, if
address is available, and returns true. If pixel address is not available,
return false and leave pixmap unchanged.
-}
peekPixels ::
  (MonadIO m) =>
  SKImage ->
  -- | pixmap. Storage for pixel state if pixels are readable; otherwise,
  -- ignored.
  SKPixmap ->
  m Bool
peekPixels image pixmap = liftIO do
  toBool <$> sk_image_peek_pixels (ptr image) (ptr pixmap)

{- | Returns 'Nothing' if the read operation fails.

Returns 'Just' along with the output 'SKImageInfo' if the read operation
succeeds.

Copies SkRect of pixels from SkImage to dstPixels. Copy starts at offset (srcX,
srcY), and does not exceed SkImage (width(), height()).

dstInfo specifies width, height, SkColorType, SkAlphaType, and SkColorSpace of
destination. dstRowBytes specifies the gap from one destination row to the next.
Returns true if pixels are copied. Returns 'Nothing' if:

* dstInfo.addr() equals nullptr

* dstRowBytes is less than dstInfo.minRowBytes()

* SkPixelRef is nullptr

Pixels are copied only if pixel conversion is possible. If SkImage SkColorType
is kGray_8_SkColorType, or kAlpha_8_SkColorType; dstInfo.colorType() must match.
If SkImage SkColorType is kGray_8_SkColorType, dstInfo.colorSpace() must match.
If SkImage SkAlphaType is kOpaque_SkAlphaType, dstInfo.alphaType() must match.
If SkImage SkColorSpace is nullptr, dstInfo.colorSpace() must match. Returns
false if pixel conversion is not possible.

srcX and srcY may be negative to copy only top or left of source. Returns false
if width() or height() is zero or negative. Returns false if abs(srcX) >= Image
width(), or if abs(srcY) >= Image height().

If cachingHint is kAllow_CachingHint, pixels may be retained locally.

If cachingHint is kDisallow_CachingHint, pixels are not added to the local
cache.
-}
readPixelsToBuffer ::
  (MonadIO m) =>
  SKImage ->
  -- | dstPixels. Destination pixel storage.
  Ptr Word8 ->
  -- | rowBytes. Destination row length
  Int ->
  -- | (srcX, srcY). Source position.
  V2 Int ->
  SKImageCachingHint ->
  m (Maybe SKImageInfo)
readPixelsToBuffer image dstPixels dstRowBytes (V2 srcX srcY) cachingHint = evalManaged do
  iminfo' <- managed alloca
  success <-
    liftIO $
      sk_image_read_pixels
        (ptr image)
        iminfo'
        (castPtr dstPixels)
        (fromIntegral dstRowBytes)
        (fromIntegral srcX)
        (fromIntegral srcY)
        (marshalSKEnum cachingHint)
  if toBool success
    then do
      iminfo <- liftIO $ peek iminfo'
      iminfo <- liftIO $ unmarshalSKImageInfo iminfo
      pure $ Just iminfo
    else do
      pure Nothing

{- | Copies SkRect of pixels from SkImage to dstPixels. Copy starts at offset (srcX, srcY),
and does not exceed SkImage (width(), height()).

dstInfo specifies width, height, SkColorType, SkAlphaType, and SkColorSpace of
destination. dstRowBytes specifies the gap from one destination row to the next.
Returns true if pixels are copied. Returns false if:
- dstInfo.addr() equals nullptr
- dstRowBytes is less than dstInfo.minRowBytes()
- SkPixelRef is nullptr

Pixels are copied only if pixel conversion is possible. If SkImage SkColorType is
kGray_8_SkColorType, or kAlpha_8_SkColorType; dstInfo.colorType() must match.
If SkImage SkColorType is kGray_8_SkColorType, dstInfo.colorSpace() must match.
If SkImage SkAlphaType is kOpaque_SkAlphaType, dstInfo.alphaType() must
match. If SkImage SkColorSpace is nullptr, dstInfo.colorSpace() must match. Returns
false if pixel conversion is not possible.

srcX and srcY may be negative to copy only top or left of source. Returns
false if width() or height() is zero or negative.
Returns false if abs(srcX) >= Image width(), or if abs(srcY) >= Image height().

If cachingHint is kAllow_CachingHint, pixels may be retained locally.
If cachingHint is kDisallow_CachingHint, pixels are not added to the local cache.
-}
readPixelsToPixmap ::
  (MonadIO m) =>
  SKImage ->
  -- | Destination pixmap.
  SKPixmap ->
  -- | Source (X, Y) position
  V2 Int ->
  SKImageCachingHint ->
  m Bool
readPixelsToPixmap image pixmap (V2 srcX srcY) cachingHint = evalManaged do
  liftIO $
    toBool
      <$> sk_image_read_pixels_into_pixmap
        (ptr image)
        (ptr pixmap)
        (fromIntegral srcX)
        (fromIntegral srcY)
        (marshalSKEnum cachingHint)

{- | Copies SkImage to dst, scaling pixels to fit dst.width() and dst.height(), and
converting pixels to match dst.colorType() and dst.alphaType(). Returns true if
pixels are copied. Returns false if dst.addr() is nullptr, or dst.rowBytes() is
less than dst SkImageInfo::minRowBytes.

Pixels are copied only if pixel conversion is possible. If SkImage SkColorType is
kGray_8_SkColorType, or kAlpha_8_SkColorType; dst.colorType() must match.
If SkImage SkColorType is kGray_8_SkColorType, dst.colorSpace() must match.
If SkImage SkAlphaType is kOpaque_SkAlphaType, dst.alphaType() must
match. If SkImage SkColorSpace is nullptr, dst.colorSpace() must match. Returns
false if pixel conversion is not possible.

If cachingHint is kAllow_CachingHint, pixels may be retained locally.
If cachingHint is kDisallow_CachingHint, pixels are not added to the local cache.
-}
scalePixelsToPixmap ::
  (MonadIO m) =>
  SKImage ->
  -- | Destination pixmap.
  SKPixmap ->
  SKSamplingOptions ->
  SKImageCachingHint ->
  m Bool
scalePixelsToPixmap image pixmap sampling cachingHint = evalManaged do
  sampling' <- storable $ marshalSKSamplingOptions sampling
  liftIO $ toBool <$> sk_image_scale_pixels (ptr image) (ptr pixmap) sampling' (marshalSKEnum cachingHint)

{- | Returns encoded SkImage pixels as SkData, if SkImage was created from
supported encoded stream format. Platform support for formats vary and may
require building with one or more of: SK_ENCODE_JPEG, SK_ENCODE_PNG,
SK_ENCODE_WEBP.

Returns 'Nothing' if SkImage contents are not encoded.
-}
getEncodedData :: (MonadResource m) => SKImage -> m (Maybe (ReleaseKey, SKData))
getEncodedData image =
  allocateSKObjectOrNothingIfNull'
    (sk_image_ref_encoded (ptr image))
    SKRefCnt.decrementNV

{- | Returns subset of this image.

Throws 'SkiaError' if any of the following are true:

  * Subset is empty

  * Subset is not contained inside the image's bounds

  * Pixels in the source image could not be read or copied

  * This image is texture-backed and the provided context is null or does not match
    the source image's context.

If the source image was texture-backed, the resulting image will be texture-backed also.
Otherwise, the returned image will be raster-backed.
-}
createSubsetRaster ::
  (MonadResource m) =>
  SKImage ->
  -- | Subset bounds
  Rect Int ->
  m (ReleaseKey, SKImage)
createSubsetRaster image subset =
  allocateSKObjectOrErrorIfNull'
    "Cannot create subset raster image"
    ( evalManaged do
        subset' <- storable $ toSKIRect subset
        liftIO $ sk_image_make_subset_raster (ptr image) subset'
    )
    SKRefCnt.decrement

{- | Like 'createSubsetRaster' but you may specify the the GrDirectContext of
the source image. The source image must not be texture-backed.
-}
createSubset ::
  (MonadResource m) =>
  SKImage ->
  GRDirectContext ->
  Rect Int ->
  m (ReleaseKey, SKImage)
createSubset image ctx subset =
  allocateSKObjectOrErrorIfNull'
    "Cannot create subset raster image with GPU context"
    ( evalManaged do
        subset' <- storable $ toSKIRect subset
        liftIO $ sk_image_make_subset (ptr image) (ptr ctx) subset'
    )
    SKRefCnt.decrement

{- | Returns raster image or lazy image. Copies SkImage backed by GPU texture
into CPU memory if needed. Returns original SkImage if decoded in raster bitmap,
or if encoded in a stream.

Throws 'SkiaError' if backed by GPU texture and copy fails.
-}
createNonTextureImage ::
  (MonadResource m) =>
  SKImage ->
  m (ReleaseKey, SKImage)
createNonTextureImage image =
  allocateSKObjectOrErrorIfNull'
    "Cannot create non-texture image"
    (sk_image_make_non_texture_image (ptr image))
    SKRefCnt.decrement

{- | Returns raster image. Copies SkImage backed by GPU texture into CPU memory,
or decodes SkImage from lazy image. Returns original SkImage if decoded in
raster bitmap.

Throws 'SkiaError' if copy, decode, or pixel read fails.
-}
createRasterImage :: (MonadResource m) => SKImage -> m (ReleaseKey, SKImage)
createRasterImage image =
  allocateSKObjectOrErrorIfNull'
    "Cannot create acquire image"
    (sk_image_make_raster_image (ptr image))
    SKRefCnt.decrement

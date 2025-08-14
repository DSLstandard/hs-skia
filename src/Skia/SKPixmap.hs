module Skia.SKPixmap where

import Linear
import Skia.Bindings.Sk_pixmap
import Skia.Color
import Skia.Internal.Prelude
import Skia.Rect
import Skia.SKImageInfo
import Skia.SKRefCnt qualified as SKRefCnt
import Skia.SKSamplingOptions

-- * Creating SKPixmap

{- | Creates an empty SkPixmap without pixels, with kUnknown_SkColorType, with
kUnknown_SkAlphaType, and with a width and height of zero. Use reset() to
associate pixels, SkColorType, SkAlphaType, width, and height after SkPixmap has
been created.

The returned SKPixmap is destructed when 'ReleaseKey' is released.
-}
createEmpty :: (MonadResource m) => m (ReleaseKey, SKPixmap)
createEmpty =
  allocateSKObjectNeverNull
    sk_pixmap_new
    sk_pixmap_destructor

{- | Creates SkPixmap from info width, height, SkAlphaType, and SkColorType.
addr points to pixels, or nullptr. rowBytes should be info.width() times
info.bytesPerPixel(), or larger.

No parameter checking is performed; it is up to the caller to ensure that addr
and rowBytes agree with info.

The memory lifetime of pixels is managed by the caller. When SkPixmap goes out
of scope, addr is unaffected.

SkPixmap may be later modified by 'reset' to change its size, pixel type, or
storage.

The returned SKPixmap is destructed when 'ReleaseKey' is released.
-}
createWithParams ::
  (MonadResource m) =>
  SKImageInfo ->
  -- | addr; pointer to pixels allocated by caller; may be nullptr
  Ptr Word8 ->
  -- | rowBytes; size of one row of addr; width times pixel size, or larger
  Int ->
  m (ReleaseKey, SKPixmap)
createWithParams iminfo buffer rowBytes =
  allocateSKObjectNeverNull
    ( evalManaged do
        iminfo' <- storable $ marshalSKImageInfo iminfo
        liftIO $ sk_pixmap_new_with_params iminfo' (castPtr buffer) (fromIntegral rowBytes)
    )
    sk_pixmap_destructor

-- * SKPixmap utils

{- | Sets width, height, row bytes to zero; pixel address to nullptr;
SkColorType to kUnknown_SkColorType; and SkAlphaType to kUnknown_SkAlphaType.

The prior pixels are unaffected; it is up to the caller to release pixels memory
if desired.
-}
reset :: (MonadIO m) => SKPixmap -> m ()
reset pixmap = evalManaged do
  liftIO $ sk_pixmap_reset (ptr pixmap)

-- Sets width, height, SkAlphaType, and SkColorType from info.
-- Sets pixel address from addr, which may be nullptr.
-- Sets row bytes from rowBytes, which should be info.width() times
-- info.bytesPerPixel(), or larger.
--
-- Does not check addr. Asserts if built with SK_DEBUG defined and if rowBytes is
-- too small to hold one row of pixels.
--
-- The memory lifetime pixels are managed by the caller. When SkPixmap goes
-- out of scope, addr is unaffected.
resetWithParams ::
  (MonadIO m) =>
  SKPixmap ->
  SKImageInfo ->
  -- | addr; pointer to pixels allocated by caller; may be nullptr
  Ptr Word8 ->
  -- | rowBytes; size of one row of addr; width times pixel size, or larger
  Int ->
  m ()
resetWithParams pixmap iminfo buffer rowBytes = evalManaged do
  iminfo' <- storable $ marshalSKImageInfo iminfo
  liftIO $ sk_pixmap_reset_with_params (ptr pixmap) iminfo' (castPtr buffer) (fromIntegral rowBytes)

{- | Changes SkColorSpace in SkImageInfo; preserves width, height, SkAlphaType,
and SkColorType in SkImage, and leaves pixel address and row bytes unchanged.
SkColorSpace reference count is incremented.
-}
setColorSpace :: (MonadIO m) => SKPixmap -> SKColorSpace -> m ()
setColorSpace pixmap colorspace =
  liftIO $ sk_pixmap_set_colorspace (ptr pixmap) (ptr colorspace)

{- | Sets subset width, height, pixel address to intersection of SkPixmap with
area, if intersection is not empty; and return true. Otherwise, leave subset
unchanged and return false.

Failing to read the return value generates a compile time warning.
-}
extractSubsetToPixmap ::
  (MonadIO m) =>
  SKPixmap ->
  -- | Destination pixmap
  SKPixmap ->
  -- | Subset
  Rect Int ->
  m Bool
extractSubsetToPixmap pixmap dstPixmap subset = evalManaged do
  subset' <- storable $ toSKIRect subset
  liftIO $ toBool <$> sk_pixmap_extract_subset (ptr pixmap) (ptr dstPixmap) subset'

-- | Returns width, height, SkAlphaType, SkColorType, and SkColorSpace.
getInfo :: (MonadIO m) => SKPixmap -> m SKImageInfo
getInfo pixmap = evalManaged do
  iminfo' <- managed alloca
  liftIO $ sk_pixmap_get_info (ptr pixmap) iminfo'
  liftIO $ unmarshalSKImageInfo =<< peek iminfo'

{- | Returns row bytes, the interval from one pixel row to the next. Row bytes
is at least as large as: width() * info().bytesPerPixel().

Returns zero if colorType() is kUnknown_SkColorType. It is up to the SkBitmap
creator to ensure that row bytes is a useful value.
-}
getRowBytes :: (MonadIO m) => SKPixmap -> m Int
getRowBytes pixmap = evalManaged do
  liftIO $ fromIntegral <$> sk_pixmap_get_row_bytes (ptr pixmap)

{- | Returns the SkColorSpace of the SKPixmap if set, the range of colors,
associated with SkImageInfo.

The returned SkColorSpace is immutable.

Note that this function increments the reference count of 'SKColorSpace', which
will be decremented once it is released by the returned 'ReleaseKey'.
-}
getColorSpace :: (MonadResource m) => SKPixmap -> m (Maybe (ReleaseKey, SKColorSpace))
getColorSpace pixmap =
  allocateSKObjectOrNothingIfNull'
    (sk_pixmap_get_colorspace (ptr pixmap))
    SKRefCnt.decrementNV

{- | Returns true if all pixels are opaque. SkColorType determines how pixels
are encoded, and whether pixel describes alpha. Returns true for SkColorType
without alpha in each pixel; for other SkColorType, returns true if all pixels
have alpha values equivalent to 1.0 or greater.

For SkColorType kRGB_565_SkColorType or kGray_8_SkColorType: always returns
true. For SkColorType kAlpha_8_SkColorType, kBGRA_8888_SkColorType,
kRGBA_8888_SkColorType: returns true if all pixel alpha values are 255. For
SkColorType kARGB_4444_SkColorType: returns true if all pixel alpha values are
15. For kRGBA_F16_SkColorType: returns true if all pixel alpha values are 1.0 or
greater.

Returns false for kUnknown_SkColorType.
-}
computeIsOpaque :: (MonadIO m) => SKPixmap -> m Bool
computeIsOpaque pixmap = evalManaged do
  liftIO $ toBool <$> sk_pixmap_compute_is_opaque (ptr pixmap)

{- | Returns pixel at (x, y) as unpremultiplied color.
Returns black with alpha if SkColorType is kAlpha_8_SkColorType.

Input is not validated: out of bounds values of x or y trigger an assert() if
built with SK_DEBUG defined; and returns undefined values or may crash if
SK_RELEASE is defined. Fails if SkColorType is kUnknown_SkColorType or
pixel address is nullptr.

SkColorSpace in SkImageInfo is ignored. Some color precision may be lost in the
conversion to unpremultiplied color; original pixel data may have additional
precision.
-}
getPixelColor ::
  (MonadIO m) =>
  SKPixmap ->
  -- | Pixel position
  V2 Int ->
  m SKColor
getPixelColor pixmap (V2 x y) = evalManaged do
  liftIO $ fmap coerce $ sk_pixmap_get_pixel_color (ptr pixmap) (fromIntegral x) (fromIntegral y)

{- | Returns pixel at (x, y) as unpremultiplied color as an SkColor4f. Returns
black with alpha if SkColorType is kAlpha_8_SkColorType.

Input is not validated: out of bounds values of x or y trigger an assert() if
built with SK_DEBUG defined; and returns undefined values or may crash if
SK_RELEASE is defined. Fails if SkColorType is kUnknown_SkColorType or pixel
address is nullptr.

SkColorSpace in SkImageInfo is ignored. Some color precision may be lost in the
conversion to unpremultiplied color; original pixel data may have additional
precision, though this is less likely than for getColor(). Rounding errors may
occur if the underlying type has lower precision.
-}
getPixelColorRGBA ::
  (MonadIO m) =>
  SKPixmap ->
  -- | Pixel position
  V2 Int ->
  m (RGBA Float)
getPixelColorRGBA pixmap (V2 x y) = evalManaged do
  color' <- managed alloca
  liftIO $ sk_pixmap_get_pixel_color4f (ptr pixmap) (fromIntegral x) (fromIntegral y) color'
  liftIO $ fmap fromSKColor4f $ peek color'

{- | Look up the pixel at (x,y) and return its alpha component, normalized to
[0..1]. This is roughly equivalent to SkGetColorA(getColor()), but can be
more efficent (and more precise if the pixels store more than 8 bits per
component).
-}
getPixelAlpha ::
  (MonadIO m) =>
  SKPixmap ->
  -- | Pixel position
  V2 Int ->
  m Float
getPixelAlpha pixmap (V2 x y) = evalManaged do
  liftIO $ coerce <$> sk_pixmap_get_pixel_alphaf (ptr pixmap) (fromIntegral x) (fromIntegral y)

-- | Exposes the **writable** address of a 'SKPixmap'.
withWritableAddress :: (MonadIO m) => SKPixmap -> (Ptr Word8 -> IO r) -> m r
withWritableAddress pixmap f = evalManaged do
  addr <- liftIO $ castPtr <$> sk_pixmap_get_writable_addr (ptr pixmap)
  liftIO $ f addr

{- | Exposes the **writable** address of a 'SKPixmap' of the pixel at the given
position.
-}
withWritableAddressOfXY ::
  (MonadIO m) =>
  SKPixmap ->
  -- | XY position
  V2 Int ->
  (Ptr Word8 -> IO r) ->
  m r
withWritableAddressOfXY pixmap (V2 x y) f = evalManaged do
  addr <- liftIO $ castPtr <$> sk_pixmap_get_writeable_addr_with_xy (ptr pixmap) (fromIntegral x) (fromIntegral y)
  liftIO $ f addr

{- | Copies a SkRect of pixels to dst. Copy starts at (srcX, srcY), and does not
exceed SkPixmap (width(), height()). dst specifies width, height, SkColorType,
SkAlphaType, and SkColorSpace of destination.  Returns true if pixels are
copied. Returns false if dst address equals nullptr, or dst.rowBytes() is less
than dst SkImageInfo::minRowBytes.

Pixels are copied only if pixel conversion is possible. If SkPixmap colorType()
is kGray_8_SkColorType, or kAlpha_8_SkColorType; dst.info().colorType must
match. If SkPixmap colorType() is kGray_8_SkColorType, dst.info().colorSpace
must match. If SkPixmap alphaType() is kOpaque_SkAlphaType, dst.info().alphaType
must match. If SkPixmap colorSpace() is nullptr, dst.info().colorSpace must
match. Returns false if pixel conversion is not possible.

srcX and srcY may be negative to copy only top or left of source. Returns false
SkPixmap width() or height() is zero or negative. Returns false if: abs(srcX) >=
Pixmap width(), or if abs(srcY) >= Pixmap height().

Returns 'Just' along with the output 'SKImageInfo' if the operation succeeds.
-}
readPixelsToBuffer ::
  (MonadIO m) =>
  SKPixmap ->
  -- | Destination pixel buffer
  Ptr Word8 ->
  -- | Destination pixel buffer row bytes
  Int ->
  -- | Source (X, Y) position
  V2 Int ->
  m (Maybe SKImageInfo)
readPixelsToBuffer pixmap dstPixels dstRowBytes (V2 srcX srcY) = evalManaged do
  iminfo' <- managed alloca
  success <-
    liftIO $
      toBool
        <$> sk_pixmap_read_pixels
          (ptr pixmap)
          iminfo'
          (castPtr dstPixels)
          (fromIntegral dstRowBytes)
          (fromIntegral srcX)
          (fromIntegral srcY)

  if success
    then do
      iminfo <- liftIO $ peek iminfo'
      iminfo <- liftIO $ unmarshalSKImageInfo iminfo
      pure $ Just iminfo
    else do
      pure Nothing

{- | Copies SkBitmap to dst, scaling pixels to fit dst.width() and dst.height(),
and converting pixels to match dst.colorType() and dst.alphaType(). Returns true
if pixels are copied. Returns false if dst address is nullptr, or dst.rowBytes()
is less than dst SkImageInfo::minRowBytes.

Pixels are copied only if pixel conversion is possible. If SkPixmap colorType()
is kGray_8_SkColorType, or kAlpha_8_SkColorType; dst SkColorType must match. If
SkPixmap colorType() is kGray_8_SkColorType, dst SkColorSpace must match. If
SkPixmap alphaType() is kOpaque_SkAlphaType, dst SkAlphaType must match. If
SkPixmap colorSpace() is nullptr, dst SkColorSpace must match. Returns false if
pixel conversion is not possible.

Returns false if SkBitmap width() or height() is zero or negative.
-}
scalePixelsToPixmap ::
  (MonadIO m) =>
  SKPixmap ->
  -- | Destination pixmap.
  SKPixmap ->
  SKSamplingOptions ->
  m Bool
scalePixelsToPixmap pixmap dstPixmap sampling = evalManaged do
  sampling' <- storable $ marshalSKSamplingOptions sampling
  liftIO $ toBool <$> sk_pixmap_scale_pixels (ptr pixmap) (ptr dstPixmap) sampling'

{- | Writes color to pixels bounded by subset; returns true on success.
Returns false if colorType() is kUnknown_SkColorType, or if subset does
not intersect bounds().
-}
erase ::
  (MonadIO m) =>
  SKPixmap ->
  SKColor ->
  -- | Subset
  Rect Int ->
  m Bool
erase pixmap color subset = evalManaged do
  subset' <- storable $ toSKIRect subset
  liftIO $ toBool <$> sk_pixmap_erase_color (ptr pixmap) (coerce color) subset'

{- | Writes color to pixels bounded by subset; returns true on success. if
subset is nullptr, writes colors pixels inside bounds(). Returns false if
colorType() is kUnknown_SkColorType, if subset is not nullptr and does not
intersect bounds(), or if subset is nullptr and bounds() is empty.
-}
eraseRGBA ::
  (MonadIO m) =>
  SKPixmap ->
  RGBA Float ->
  -- | Subset
  Rect Int ->
  m Bool
eraseRGBA pixmap color subset = evalManaged do
  color' <- storable $ toSKColor4f color
  subset' <- storable $ toSKIRect subset
  liftIO $ toBool <$> sk_pixmap_erase_color4f (ptr pixmap) color' subset'

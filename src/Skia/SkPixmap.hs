module Skia.SkPixmap where

import Language.C.Inline.Cpp qualified as C
import Linear
import Skia.Internal.Prelude
import Skia.SkColor
import Skia.SkImageInfo
import Skia.SkRect
import Skia.SkSamplingOptions

C.context $
  mconcat
    [ C.cppCtx
    , cppSkiaObjectTypes
    ]

C.include "core/SkPixmap.h"
C.include "core/SkColorSpace.h"

-- * Creating SkPixmap

{- | Creates an empty SkPixmap without pixels, with kUnknown_SkColorType, with
kUnknown_SkAlphaType, and with a width and height of zero. Use reset() to
associate pixels, SkColorType, SkAlphaType, width, and height after SkPixmap has
been created.

The returned SkPixmap is destructed when 'ReleaseKey' is released.
-}
createEmpty :: (MonadResource m) => m (ReleaseKey, SkPixmap)
createEmpty =
  allocateSkObjectNeverNull
    [C.block|SkPixmap* {
      return new SkPixmap();
    }|]
    (\p -> [C.block| void { delete $(SkPixmap* p); } |])

{- | Creates SkPixmap from info width, height, SkAlphaType, and SkColorType.
addr points to pixels, or nullptr. rowBytes should be info.width() times
info.bytesPerPixel(), or larger.

No parameter checking is performed; it is up to the caller to ensure that addr
and rowBytes agree with info.

The memory lifetime of pixels is managed by the caller. When SkPixmap goes out
of scope, addr is unaffected.

SkPixmap may be later modified by 'reset' to change its size, pixel type, or
storage.

The returned SkPixmap is destructed when 'ReleaseKey' is released.
-}
createWithParams ::
  (MonadResource m) =>
  ImageInfo ->
  -- | addr; pointer to pixels allocated by caller; may be nullptr
  Ptr Word8 ->
  -- | rowBytes; size of one row of addr; width times pixel size, or larger
  Int ->
  m (ReleaseKey, SkPixmap)
createWithParams iminfo (castPtr -> buffer) (fromIntegral -> rowBytes) =
  allocateSkObjectNeverNull
    ( evalManaged do
        ciminfo <- marshalSKImageInfo iminfo
        liftIO
          [C.block|SkPixmap* {
          return new SkPixmap(
            *$(SkImageInfo* ciminfo),
            $(void* buffer),
            $(int rowBytes)
          );
        }|]
    )
    (\p -> [C.block| void { delete $(SkPixmap* p); } |])

-- * SkPixmap utils

{- | Sets width, height, row bytes to zero; pixel address to nullptr;
SkColorType to kUnknown_SkColorType; and SkAlphaType to kUnknown_SkAlphaType.

The prior pixels are unaffected; it is up to the caller to release pixels memory
if desired.
-}
reset :: (MonadIO m) => SkPixmap -> m ()
reset (ptr -> pixmap) =
  liftIO
    [C.block|void {
    $(SkPixmap* pixmap)->reset();
  }|]

{- | Sets width, height, SkAlphaType, and SkColorType from info.
Sets pixel address from addr, which may be nullptr.
Sets row bytes from rowBytes, which should be info.width() times
info.bytesPerPixel(), or larger.

Does not check addr. Asserts if built with SK_DEBUG defined and if rowBytes is
too small to hold one row of pixels.

The memory lifetime pixels are managed by the caller. When SkPixmap goes
out of scope, addr is unaffected.
-}
resetWithParams ::
  (MonadIO m) =>
  SkPixmap ->
  ImageInfo ->
  -- | addr; pointer to pixels allocated by caller; may be nullptr
  Ptr Word8 ->
  -- | rowBytes; size of one row of addr; width times pixel size, or larger
  Int ->
  m ()
resetWithParams (ptr -> pixmap) iminfo (castPtr -> buffer) (fromIntegral -> rowBytes) = evalManaged do
  iminfo' <- marshalSKImageInfo iminfo
  liftIO
    [C.block|void {
    $(SkPixmap* pixmap)->reset(
      *$(SkImageInfo* iminfo'),
      $(void* buffer),
      $(int rowBytes)
    );
  }|]

{- | Changes SkColorSpace in SkImageInfo; preserves width, height, SkAlphaType,
and SkColorType in SkImage, and leaves pixel address and row bytes unchanged.
SkColorSpace reference count is incremented.
-}
setColorSpace :: (MonadIO m) => SkPixmap -> SkColorSpace -> m ()
setColorSpace (ptr -> pixmap) (ptr -> colorspace) =
  liftIO
    [C.block|void {
    $(SkPixmap* pixmap)->setColorSpace(sk_ref_sp($(SkColorSpace* colorspace)));
  }|]

{- | Sets subset width, height, pixel address to intersection of SkPixmap with
area, if intersection is not empty; and return true. Otherwise, leave subset
unchanged and return false.
-}
extractSubsetToPixmap ::
  (MonadIO m) =>
  SkPixmap ->
  -- | Destination pixmap
  SkPixmap ->
  -- | Subset
  Rect Int ->
  m Bool
extractSubsetToPixmap (ptr -> pixmap) (ptr -> dstPixmap) subset = evalManaged do
  subset' <- marshalSkIRect subset
  toBool
    <$> liftIO
      [C.block|bool {
    return $(SkPixmap* pixmap)->extractSubset($(SkPixmap* dstPixmap), *$(SkIRect* subset'));
  }|]

{- | Returns row bytes, the interval from one pixel row to the next. Row bytes
is at least as large as: width() * info().bytesPerPixel().

Returns zero if colorType() is kUnknown_SkColorType. It is up to the SkBitmap
creator to ensure that row bytes is a useful value.
-}
getRowBytes :: (MonadIO m) => SkPixmap -> m Int
getRowBytes (ptr -> pixmap) =
  liftIO $ fromIntegral <$> [C.exp|size_t { $(SkPixmap* pixmap)->rowBytes() }|]

{- | Returns width, height, SkAlphaType, SkColorType, and SkColorSpace.

This is in a 'MonadResource' because 'ImageInfo' holds a reference to a
'SkColorSpace'.
-}
getInfo :: (MonadResource m) => SkPixmap -> m (ReleaseKey, ImageInfo)
getInfo (ptr -> pixmap) = do
  iminfo' <- liftIO [C.exp|const SkImageInfo* { &$(SkPixmap* pixmap)->info() }|]
  peekSKImageInfo iminfo'

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
computeIsOpaque :: (MonadIO m) => SkPixmap -> m Bool
computeIsOpaque (ptr -> pixmap) =
  liftIO $ toBool <$> [C.exp|bool { $(SkPixmap* pixmap)->computeIsOpaque() }|]

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
getPixelColor :: (MonadIO m) => SkPixmap -> V2 Int -> m SkColor
getPixelColor (ptr -> pixmap) (V2 (fromIntegral -> x) (fromIntegral -> y)) =
  SkColor
    <$> liftIO
      [C.exp|uint32_t {
    $(SkPixmap* pixmap)->getColor($(int x), $(int y))
  }|]

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
  SkPixmap ->
  -- | Pixel position
  V2 Int ->
  m (RGBA Float)
getPixelColorRGBA (ptr -> pixmap) (V2 (fromIntegral -> x) (fromIntegral -> y)) = evalManaged do
  color4f' <- allocaSkColor4f
  liftIO
    [C.block|void {
    *$(SkColor4f* color4f') = $(SkPixmap* pixmap)->getColor4f($(int x), $(int y));
  }|]
  peekSkColor4f color4f'

{- | Look up the pixel at (x,y) and return its alpha component, normalized to
[0..1]. This is roughly equivalent to SkGetColorA(getColor()), but can be
more efficent (and more precise if the pixels store more than 8 bits per
component).
-}
getPixelAlpha ::
  (MonadIO m) =>
  SkPixmap ->
  -- | Pixel position
  V2 Int ->
  m Float
getPixelAlpha (ptr -> pixmap) (V2 (fromIntegral -> x) (fromIntegral -> y)) =
  coerce
    <$> liftIO
      [C.exp|float {
    $(SkPixmap* pixmap)->getAlphaf($(int x), $(int y))
  }|]

-- | Exposes the **writable** address of a 'SkPixmap'.
withWritableAddress :: (MonadIO m) => SkPixmap -> (Ptr Word8 -> IO r) -> m r
withWritableAddress (ptr -> pixmap) f =
  liftIO $ do
    addr <- castPtr <$> [C.exp|void* { $(SkPixmap* pixmap)->writable_addr() }|]
    f addr

{- | Exposes the **writable** address of a 'SkPixmap' of the pixel at the given
position.
-}
withWritableAddressOfXY ::
  (MonadIO m) =>
  SkPixmap ->
  -- | XY position
  V2 Int ->
  (Ptr Word8 -> IO r) ->
  m r
withWritableAddressOfXY (ptr -> pixmap) (V2 (fromIntegral -> x) (fromIntegral -> y)) f =
  liftIO $ do
    addr <- castPtr <$> [C.exp|void* { $(SkPixmap* pixmap)->writable_addr($(int x), $(int y)) }|]
    f addr

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
  SkPixmap ->
  -- | Destination pixmap.
  SkPixmap ->
  SamplingOptions ->
  m Bool
scalePixelsToPixmap (ptr -> pixmap) (ptr -> dstPixmap) sampling = evalManaged do
  sampling' <- marshalSkSamplingOptions sampling
  toBool
    <$> liftIO
      [C.block|bool {
    return $(SkPixmap* pixmap)->scalePixels(*$(SkPixmap* dstPixmap), *$(SkSamplingOptions* sampling'));
  }|]

{- | Writes color to pixels bounded by subset; returns true on success.
Returns false if colorType() is kUnknown_SkColorType, or if subset does
not intersect bounds().
-}
erase ::
  (MonadIO m) =>
  SkPixmap ->
  SkColor ->
  -- | Subset
  Maybe (Rect Int) ->
  m Bool
erase (ptr -> pixmap) (SkColor color) subset = evalManaged do
  subset' <- maybe (pure nullPtr) marshalSkIRect subset
  toBool
    <$> liftIO
      [C.block|bool {
    if ($(const SkIRect* subset') == nullptr) {
      return $(SkPixmap* pixmap)->erase((SkColor) $(uint32_t color));
    } else {
      return $(SkPixmap* pixmap)->erase((SkColor) $(uint32_t color), *$(const SkIRect* subset'));
    }
  }|]

{- | Writes color to pixels bounded by subset; returns true on success. if
subset is nullptr, writes colors pixels inside bounds(). Returns false if
colorType() is kUnknown_SkColorType, if subset is not nullptr and does not
intersect bounds(), or if subset is nullptr and bounds() is empty.
-}
eraseRGBA ::
  (MonadIO m) =>
  SkPixmap ->
  RGBA Float ->
  -- | Subset
  Maybe (Rect Int) ->
  m Bool
eraseRGBA (ptr -> pixmap) color subset = evalManaged do
  color4f' <- marshalSkColor4f color
  subset' <- maybe (pure nullPtr) marshalSkIRect subset
  toBool
    <$> liftIO
      [C.block|bool {
    return $(SkPixmap* pixmap)->erase(*$(const SkColor4f* color4f'), $(const SkIRect* subset'));
  }|]
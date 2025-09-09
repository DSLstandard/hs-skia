module Skia.SkColor where

import Data.Bits
import Skia.Internal.Prelude
import Language.C.Inline.Cpp qualified as C
import Data.Vector qualified as V

C.context $ C.cppCtx <> cppSkiaObjectTypes

C.include "core/SkColor.h"
C.include "core/SkColorPriv.h"
C.include "core/SkUnPreMultiply.h"

data RGBA a = RGBA
  { red :: a
  , green :: a
  , blue :: a
  , alpha :: a
  }
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

{- | 32-bit ARGB color value, unpremultiplied. Color components are always in a
known order. This is different from 'SkPMColor', which has its bytes in a
configuration dependent order, to match the format of kBGRA_8888_SkColorType
bitmaps. SkColor is the type used to specify colors in SkPaint and in gradients.

Color that is premultiplied has the same component values as color that is
unpremultiplied if alpha is 255, fully opaque, although may have the component
values in a different order.
-}
newtype SkColor = SkColor
  { unSkColor :: Word32
  }
  deriving (Show, Eq, Ord)
  deriving newtype (Storable, Bits, Num)

{- | 32-bit ARGB color value, premultiplied. The byte order for this value is
configuration dependent, matching the format of 'SkColorType'BGRA8888' bitmaps.
This is different from SkColor, which is unpremultiplied, and is always in the
same byte order.
-}
newtype SkPMColor = SkPMColor
  { unSkPMColor :: Word32
  }
  deriving (Show, Eq, Ord)
  deriving newtype (Storable, Bits, Num)

premultiply :: SkColor -> SkPMColor
premultiply (SkColor color) = SkPMColor
  [C.pure| uint32_t {
    SkPreMultiplyColor($(uint32_t color))
  }|]
{-# NOINLINE premultiply #-}

unpremultiply :: SkPMColor -> SkColor
unpremultiply (SkPMColor pmcolor) = SkColor
  [C.pure| uint32_t {
    SkUnPreMultiply::PMColorToColor($(uint32_t pmcolor))
  }|]

-- * Conversion utilties

allocaSkColor4f :: Managed (Ptr C'SkColor4f)
allocaSkColor4f =
  managed $ bracket
    [C.block| SkColor4f* {
      return new SkColor4f;
    }|]
    ( \p -> do
      [C.block|void {
        delete $(SkColor4f* p);
      }|]
    )

peekSkColor4f :: (MonadIO m) => Ptr C'SkColor4f -> m (RGBA Float)
peekSkColor4f c = liftIO do
  r <- [C.exp| float { $(SkColor4f* c)->fR } |]
  g <- [C.exp| float { $(SkColor4f* c)->fG } |]
  b <- [C.exp| float { $(SkColor4f* c)->fB } |]
  a <- [C.exp| float { $(SkColor4f* c)->fA } |]
  pure $ coerce $ RGBA r g b a

pokeSkColor4f :: MonadIO m => Ptr C'SkColor4f -> RGBA Float -> m ()
pokeSkColor4f c (coerce -> RGBA r g b a) = liftIO do
  [C.block| void {
    $(SkColor4f* c)->fR = $(float r);
    $(SkColor4f* c)->fG = $(float g);
    $(SkColor4f* c)->fB = $(float b);
    $(SkColor4f* c)->fA = $(float a);
  }|]

marshalSkColor4f :: RGBA Float -> Managed (Ptr C'SkColor4f)
marshalSkColor4f rgba = do
  c <- allocaSkColor4f
  pokeSkColor4f c rgba
  pure c

marshalSkColor4fArray :: V.Vector (RGBA Float) -> Managed (Ptr C'SkColor4f)
marshalSkColor4fArray rgbas = do
  let clen :: CInt = fromIntegral $ V.length rgbas

  carray <- managed $ bracket
    [C.block| SkColor4f* {
      return new SkColor4f[$(int clen)];
    }|]
    ( \p -> [C.block|void { delete[] $(SkColor4f* p); }|])

  V.iforM_ rgbas \(fromIntegral -> i) rgba -> do
    citem <- liftIO [C.exp| SkColor4f* { &($(SkColor4f* carray)[$(int i)]) } |]
    pokeSkColor4f citem rgba

  pure carray
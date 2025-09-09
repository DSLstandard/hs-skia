module Skia.SkImageInfo (
  module Skia.SkImageInfo,
  SkImageInfo (..), -- from Skia.Types.Extra,
)
where

import Language.C.Inline.Cpp qualified as C
import Skia.Internal.Prelude
import Skia.SkRefCnt qualified as SkRefCnt
import Skia.SkColorType
import Skia.SkAlphaType

C.context $ C.cppCtx <> cppSkiaObjectTypes

C.include "core/SkImageInfo.h"
C.include "core/SkColorSpace.h"

{- | Describes pixel dimensions and encoding. 'SkBitmap', 'SkImage', 'SkPixmap',
and 'SkSurface' can be created from 'SkImageInfo'.

'SkImageInfo' can be retrieved from 'SkBitmap' and 'SkPixmap', but not from
'SkImage' and 'SkSurface'. For example, 'SkImage' and 'SkSurface'
implementations may defer pixel depth, so may not completely specify
'SkImageInfo'.

'SkImageInfo' contains dimensions, the pixel integral width and height. It
encodes how pixel bits describe alpha, transparency; color components red, blue,
and green; and 'SkColorSpace', the range and linearity of colors.
-}
data ImageInfo = ImageInfo
  { colorspace :: Maybe SkColorSpace
  , width :: Int
  , height :: Int
  , colorType :: SkColorType
  , alphaType :: SkAlphaType
  }
  deriving (Show)

-- * Marshal utils

-- unmarshalImageInfo :: (MonadIO m) => Sk_imageinfo -> m SkImageInfo
-- unmarshalImageInfo iminfo = do
--   -- NOTE: This function is IO just for 'unmarshalSkEnumOrDie"
--   colorType <- unmarshalSkEnumOrDie iminfo.colorType
--   alphaType <- unmarshalSkEnumOrDie iminfo.alphaType
--   pure
--     SkImageInfo
--       { colorspace = fromPtr iminfo.colorspace
--       , width = fromIntegral iminfo.width
--       , height = fromIntegral iminfo.height
--       , colorType
--       , alphaType
--       }

marshalSKImageInfo :: ImageInfo -> Managed (Ptr C'SkImageInfo)
marshalSKImageInfo iminfo = do
  managed $ bracket
    [C.block|SkImageInfo* {
      SkImageInfo *iminfo = new SkImageInfo;
      *iminfo = SkImageInfo::Make(
        $(int cwidth),
        $(int cheight),
        (SkColorType) $(int ccolortype),
        (SkAlphaType) $(int calphatype),
        sk_ref_sp($(SkColorSpace* ccolorspace))
      );
      return iminfo;
    }|]
    (\iminfo -> [C.block|void{ delete $(SkImageInfo* iminfo); }|])
  where
    ccolorspace = ptrOrNull iminfo.colorspace
    cwidth = fromIntegral iminfo.width
    cheight = fromIntegral iminfo.height
    ccolortype = marshalSkEnum iminfo.colorType
    calphatype = marshalSkEnum iminfo.alphaType

-- | This is in a 'MonadResource' because 'ImageInfo' holds a reference to a
-- 'SkColorSpace'.
peekSKImageInfo :: MonadResource m => Ptr C'SkImageInfo -> m (ReleaseKey, ImageInfo)
peekSKImageInfo p = do
  iminfo <- evalManaged do
    colorspace'' <- managed $ alloca @(Ptr C'SkColorSpace)
    width' <- managed $ alloca @CInt
    height' <- managed $ alloca @CInt
    colorType' <- managed $ alloca @CInt
    alphaType' <- managed $ alloca @CInt

    liftIO [C.block|void {
      SkImageInfo *p = $(SkImageInfo* p);
      *$(int* width') = p->width();
      *$(int* height') = p->height();
      *$(SkColorSpace** colorspace'') = p->refColorSpace().release();
      *$(int* alphaType') = p->alphaType();
      *$(int* colorType') = p->colorType();
    }|]

    -- FIXME: Mask exception
    colorspace' <- liftIO $ peek colorspace''
    width <- liftIO $ peek width'
    height <- liftIO $ peek height'
    alphaType <- unmarshalSkEnumOrDie =<< liftIO (peek alphaType')
    colorType <- unmarshalSkEnumOrDie =<< liftIO (peek colorType')

    pure ImageInfo
      { colorspace =
          if colorspace' == nullPtr
            then Nothing
            else Just (SkColorSpace colorspace')
      , width = fromIntegral width
      , height = fromIntegral height
      , alphaType
      , colorType
      }

  key <- register do
    whenJust iminfo.colorspace SkRefCnt.decrementNV 

  pure (key, iminfo)
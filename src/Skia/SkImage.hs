module Skia.SkImage where

import Language.C.Inline.Cpp qualified as C
import Skia.Internal.Prelude
import Skia.SkRefCnt qualified as SkRefCnt

C.context $ C.cppCtx <> cppSkiaObjectTypes

C.include "core/SkImage.h"

{- | Copies SkImage pixel address, row bytes, and SkImageInfo to pixmap, if
address is available, and returns true. If pixel address is not available,
return false and leave pixmap unchanged.
-}
peekPixels ::
  (MonadIO m) =>
  SkImage ->
  -- | pixmap. Storage for pixel state if pixels are readable; otherwise,
  -- ignored.
  SkPixmap ->
  m Bool
peekPixels (ptr -> cimage) (ptr -> cpixmap) = liftIO do
  toBool <$> [C.block| bool {
    return $(SkImage* cimage)->peekPixels($(SkPixmap* cpixmap));
  }|]

{- | Returns raster image. Copies SkImage backed by GPU texture into CPU memory,
or decodes SkImage from lazy image. Returns original SkImage if decoded in
raster bitmap.

Throws 'SkiaError' if copy, decode, or pixel read fails.
-}
createRasterFromPixmapCopy :: (MonadResource m) => SkPixmap -> m (ReleaseKey, SkImage)
createRasterFromPixmapCopy (ptr -> cpixmap) =
  allocateSkObjectOrErrorIfNull'
    "Cannot create acquire image"
    [C.block| SkImage* {
      return SkImages::RasterFromPixmapCopy(*$(SkPixmap* cpixmap)).release();
    }|]
    SkRefCnt.decrement

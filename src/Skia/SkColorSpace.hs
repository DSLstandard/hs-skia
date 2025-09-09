module Skia.SkColorSpace where

import Skia.Internal.Prelude
import Skia.SkRefCnt qualified as SkRefCnt
import Language.C.Inline.Cpp qualified as C

C.context $ C.cppCtx <> cppSkiaObjectTypes

C.include "core/SkColorSpace.h"

-- | Create the sRGB color space.
createSRGB :: (MonadResource m) => m (ReleaseKey, SkColorSpace)
createSRGB =
  allocateSkObjectNeverNull'
    [C.block| SkColorSpace* {
      return SkColorSpace::MakeSRGB().release();
    }|]
    SkRefCnt.decrementNV

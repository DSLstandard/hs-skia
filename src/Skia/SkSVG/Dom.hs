module Skia.SkSVG.Dom where

import Language.C.Inline.Cpp qualified as C
import Skia.Linear
import Skia.SkSVG.Internal.Prelude
import qualified Skia.SkRefCnt as SkRefCnt

C.context $ C.cppCtx <> cppSkiaObjectTypes <> cppSkSVGObjectTypes

C.include "modules/svg/include/SkSVGDOM.h"

-- | Creates an SVG DOM from an input stream.
createFromStream :: (MonadResource m, IsSkStream stream) => stream -> m (ReleaseKey, Dom)
createFromStream (ptr . toA SkStream -> stream) =
  allocateSkObjectOrErrorIfNull'
    "Failed to create SKSVGDOM from input stream"
    [C.exp| SkSVGDOM* {
      SkSVGDOM::Builder()
        .make(*$(SkStream* stream))
        .release()
    }|]
    SkRefCnt.decrement

{-# DEPRECATED getContainerSize "Google Skia recommends \"us[ing] getRoot()->intrinsicSize() to query the root element intrinsic size.\"" #-}

-- | Queries the root element intrinsic size.
getContainerSize :: (MonadIO m) => Dom -> m (V2 Float)
getContainerSize (ptr -> dom)  = evalManaged do
  size' <- allocaSkSize
  liftIO [C.block| void {
    *$(SkSize* size') = $(SkSVGDOM* dom)->containerSize();
  }|]
  peekSkSize size'

-- | Renders the SVG DOM to a canvas.
render :: (MonadIO m, IsSkCanvas canvas) => Dom -> canvas -> m ()
render (ptr -> dom) (ptr . toA SkCanvas -> canvas) = liftIO do
  [C.block| void {
    $(SkSVGDOM* dom)->render($(SkCanvas* canvas));
  }|]

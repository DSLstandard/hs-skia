module Skia.SKSvg.Dom where

import Skia.Bindings.Sksvg
import Skia.Internal.Prelude
import Skia.SKSvg.Objects qualified as SKSvg
import qualified Skia.SKRefCnt

-- | Creates an SVG DOM from an input stream.
create :: (MonadResource m, IsSKStream stream) => stream -> m (ReleaseKey, SKSvg.Dom)
create (toA SKStream -> stream) =
  allocateSKObjectOrErrorIfNull' "Failed to create SKSVGDOM from input stream"
    (sksvg_dom_create (ptr stream))
    Skia.SKRefCnt.decrement

-- | Queries the root element intrinsic size.
getContainerSize :: (MonadIO m) => SKSvg.Dom ->
  -- | Viewport
  V2 Int ->
  -- | DPI (Tip: Google Skia's default argument here is 90)
  Float ->
  m (V2 Int)
getContainerSize dom (V2 vw vh) dpi = evalManaged do
  width <- managed alloca
  height <- managed alloca
  liftIO $ sksvg_dom_get_container_size (ptr dom) (fromIntegral vw) (fromIntegral vh) (coerce dpi) width height
  w <- peekWith fromIntegral width
  h <- peekWith fromIntegral height
  pure $ V2 w h

-- | Renders the SVG DOM to a canvas.
render :: (MonadIO m, IsSKCanvas canvas) => SKSvg.Dom -> canvas -> m ()
render dom (toA SKCanvas -> canvas) = liftIO do
  sksvg_dom_render (ptr dom) (ptr canvas)

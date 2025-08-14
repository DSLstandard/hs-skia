module Skia.GRBackendRenderTarget where

import Skia.Bindings.Gr_context
import Skia.Bindings.Types
import Skia.Internal.Prelude

-- * Creating a backend render target

-- NOTE: 'createGl' and friends have no documentation in Skia, so we are taking
-- documentations from
-- https://learn.microsoft.com/en-us/dotnet/api/skiasharp.grbackendrendertarget?view=skiasharp-2.88.
-- instead.

{- | Creates a new OpenGL 'GRBackendRenderTarget' with the specified properties
and framebuffer.

The 'GRBackendRenderTarget' is destroyed when 'Acquire' releases.
-}
createGl ::
  (MonadResource m) =>
  -- | Width
  Int ->
  -- | Height
  Int ->
  -- | Sample count
  Int ->
  -- | Stencil bits
  Int ->
  -- | Framebuffer info
  Gr_gl_framebufferinfo ->
  m (ReleaseKey, GRBackendRenderTarget)
createGl width height sampleCount stencilBits glInfo =
  allocateSKObjectNeverNull
    ( evalManaged do
        glInfo' <- storable glInfo
        liftIO $
          gr_backendrendertarget_new_gl
            (fromIntegral width)
            (fromIntegral height)
            (fromIntegral sampleCount)
            (fromIntegral stencilBits)
            glInfo'
    )
    gr_backendrendertarget_delete

-- * Getters

-- | Returns true if the 'GRBackendRenderTarget' has been initialized.
isValid :: (MonadIO m) => GRBackendRenderTarget -> m Bool
isValid tgt = do
  liftIO $ fmap toBool $ gr_backendrendertarget_is_valid (ptr tgt)

-- | Gets the width in pixels.
getWidth :: (MonadIO m) => GRBackendRenderTarget -> m Int
getWidth tgt = liftIO do
  fmap fromIntegral $ gr_backendrendertarget_get_width (ptr tgt)

-- | Gets the height in pixels.
getHeight :: (MonadIO m) => GRBackendRenderTarget -> m Int
getHeight tgt = liftIO do
  fmap fromIntegral $ gr_backendrendertarget_get_height (ptr tgt)

-- | Gets the number of samples per pixel.
getSampleCount :: (MonadIO m) => GRBackendRenderTarget -> m Int
getSampleCount tgt = liftIO do
  fmap fromIntegral $ gr_backendrendertarget_get_samples (ptr tgt)

-- | Gets the number of bits of stencil per-pixel.
getStencilBits :: (MonadIO m) => GRBackendRenderTarget -> m Int
getStencilBits tgt = liftIO do
  fmap fromIntegral $ gr_backendrendertarget_get_stencils (ptr tgt)

-- | Gets the backend type for this render target.
getBackend :: (MonadIO m) => GRBackendRenderTarget -> m GRBackend
getBackend tgt = liftIO do
  unmarshalSKEnumOrDie =<< gr_backendrendertarget_get_backend (ptr tgt)

-- | Returns 'Nothing' if the target does not have 'GlFramebufferInfo'.
getGlFramebufferInfo :: (MonadIO m) => GRBackendRenderTarget -> m (Maybe Gr_gl_framebufferinfo)
getGlFramebufferInfo tgt = evalManaged do
  glInfo' <- managed alloca
  success <- liftIO $ fmap toBool $ gr_backendrendertarget_get_gl_framebufferinfo (ptr tgt) glInfo'
  if success
    then do
      glInfo <- liftIO $ peek glInfo'
      pure $ Just glInfo
    else do
      pure Nothing

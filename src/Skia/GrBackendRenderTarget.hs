module Skia.GrBackendRenderTarget where

import Language.C.Inline.Cpp qualified as C
import Skia.GrGLTypes
import Skia.GrTypes
import Skia.Internal.Prelude

C.context $ mconcat
  [ C.cppCtx
  , cppSkiaObjectTypes
  ]

C.include "gpu/GrBackendSurface.h"
C.include "gpu/ganesh/gl/GrGLBackendSurface.h"

-- * Creating a backend render target

-- NOTE: 'createGl' and friends have no documentation in Skia, so we are taking
-- documentations from
-- https://learn.microsoft.com/en-us/dotnet/api/skiasharp.grbackendrendertarget?view=skiasharp-2.88.
-- instead.

{- | Creates a new OpenGL 'GrBackendRenderTarget' with the specified properties
and framebuffer.

The 'GrBackendRenderTarget' is destroyed when 'Acquire' releases.
-}
makeGL ::
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
  GrGLFramebufferInfo ->
  m (ReleaseKey, GrBackendRenderTarget)
makeGL (fromIntegral -> cwidth) (fromIntegral -> cheight) (fromIntegral -> csampleCount) (fromIntegral -> cstencilBits) fbInfo =
  allocateSkObjectNeverNull
    ( do
        let GrGLFramebufferInfo{fboId, format} = fbInfo
        [C.block|GrBackendRenderTarget* {
          GrGLFramebufferInfo fbinfo = {
            .fFBOID = $(uint32_t fboId),
            .fFormat = $(uint32_t format)
          };
          return new GrBackendRenderTarget(
            GrBackendRenderTargets::MakeGL(
              $(int cwidth), $(int cheight),
              $(int csampleCount), $(int cstencilBits),
              fbinfo
            )
          );
        }|]
    )
    (\p -> [C.block| void { delete $(GrBackendRenderTarget* p); } |])

-- * Getters

-- | Returns true if the 'GrBackendRenderTarget' has been initialized.
isValid :: (MonadIO m) => GrBackendRenderTarget -> m Bool
isValid (ptr -> tgt) = liftIO do
  toBool <$> [C.exp| bool { $(GrBackendRenderTarget* tgt)->isValid() } |]

-- | Gets the width in pixels.
getWidth :: (MonadIO m) => GrBackendRenderTarget -> m Int
getWidth (ptr -> tgt) = liftIO do
  fromIntegral <$> [C.exp| int { $(GrBackendRenderTarget* tgt)->width() } |]

-- | Gets the height in pixels.
getHeight :: (MonadIO m) => GrBackendRenderTarget -> m Int
getHeight (ptr -> tgt) = liftIO do
  fromIntegral <$> [C.exp| int { $(GrBackendRenderTarget* tgt)->height() } |]

-- | Gets the number of samples per pixel.
getSampleCount :: (MonadIO m) => GrBackendRenderTarget -> m Int
getSampleCount (ptr -> tgt) = liftIO do
  fromIntegral <$> [C.exp| int { $(GrBackendRenderTarget* tgt)->sampleCnt() } |]

-- | Gets the number of bits of stencil per-pixel.
getStencilBits :: (MonadIO m) => GrBackendRenderTarget -> m Int
getStencilBits (ptr -> tgt) = liftIO do
  fromIntegral <$> [C.exp| int { $(GrBackendRenderTarget* tgt)->stencilBits() } |]

-- | Gets the backend type for this render target.
getBackend :: (MonadIO m) => GrBackendRenderTarget -> m GrBackendApi
getBackend (ptr -> tgt) = liftIO do
  backend <- [C.exp| int { (int) $(GrBackendRenderTarget* tgt)->backend() } |]
  unmarshalSkEnumOrDie backend

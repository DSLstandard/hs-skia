module Skia.GrContextOptions where

import Skia.Internal.Prelude
import Language.C.Inline.Cpp qualified as C

C.context $ C.cppCtx <> cppSkiaObjectTypes

C.include "gpu/GrContextOptions.h"

-- | Additional options used in constructing 'GRDirectContext'.
data ContextOptions = ContextOptions
  { avoidStencilBuffers :: Bool
  -- ^ Bugs on certain drivers cause stencil buffers to leak. This flag causes
  -- Skia to avoid allocating stencil buffers and use alternate rasterization
  -- paths, avoiding the leak.
  , runtimeProgramCacheSize :: Int
  -- ^ Maximum number of GPU programs or pipelines to keep active in the
  -- runtime cache.
  , glyphCacheTextureMaximumBytes :: Int
  -- ^ The maximum size of cache textures used for Skia's Glyph cache.
  , allowPathMaskCaching :: Bool
  -- ^ If true this allows path mask textures to be cached. This is only
  -- really useful if paths are commonly rendered at the same scale and
  -- fractional translation.
  , doManualMipmapping :: Bool
  -- ^ Construct mipmaps manually, via repeated downsampling draw-calls. This
  -- is used when the driver's implementation (glGenerateMipmap) contains
  -- bugs. This requires mipmap level control (ie desktop or ES3).
  , bufferMapThreshold :: Maybe Int
  -- ^ the threshold in bytes above which we will use a buffer mapping API to
  -- map vertex and index buffers to CPU memory in order to update them.
  -- Putting 'Nothing' here means the GrContext should deduce the optimal
  -- value for this platform.
  }
  deriving (Show, Eq, Ord)

-- | A 'ContextOptions' with Google Skia's default values.
defaultOptions :: ContextOptions
defaultOptions =
  -- See include/gpu/GrContextOptions.h
  ContextOptions
    { avoidStencilBuffers = False
    , runtimeProgramCacheSize = 256
    , glyphCacheTextureMaximumBytes = 2048 * 1024 * 4
    , allowPathMaskCaching = True
    , doManualMipmapping = False
    , bufferMapThreshold = Nothing
    }

-- * Marshal utils

marshalGrContextOptions :: ContextOptions -> Managed (Ptr C'GrContextOptions)
marshalGrContextOptions opts =
  managed $ bracket
    ( do
        let
          fAvoidStencilBuffers :: CBool = fromBool $ avoidStencilBuffers opts
          fRuntimeProgramCacheSize :: CInt = fromIntegral $ runtimeProgramCacheSize opts
          fGlyphCacheTextureMaximumBytes :: CInt = fromIntegral $ glyphCacheTextureMaximumBytes opts
          fAllowPathMaskCaching :: CBool = fromBool $ allowPathMaskCaching opts
          fDoManualMipmapping :: CBool = fromBool $ doManualMipmapping opts
          fBufferMapThreshold :: CInt = maybe (-1) fromIntegral $ bufferMapThreshold opts
        [C.block| GrContextOptions* {
          GrContextOptions* opts = new GrContextOptions;
          opts->fAvoidStencilBuffers = $(bool fAvoidStencilBuffers);
          opts->fRuntimeProgramCacheSize = $(int fRuntimeProgramCacheSize);
          opts->fGlyphCacheTextureMaximumBytes = $(int fGlyphCacheTextureMaximumBytes);
          opts->fAllowPathMaskCaching = $(bool fAllowPathMaskCaching);
          opts->fDoManualMipmapping = $(bool fDoManualMipmapping);
          opts->fBufferMapThreshold = $(int fBufferMapThreshold);
          return opts;
        }|]
    )
    (\p -> [C.block| void { delete $(GrContextOptions* p); } |])

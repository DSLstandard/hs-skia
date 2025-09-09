module Skia.GrGLTypes where

import Data.Word

data GrGLFramebufferInfo = GrGLFramebufferInfo
  { fboId :: Word32
  -- ^ OpenGL framebuffer ID
  , format :: Word32
  -- ^ OpenGL format, e.g. GL_RGBA8
  }
  deriving (Show, Eq, Ord)


module Skia.GrGLInterface (
  -- -- * Creating 'GrGLInterface'
  createNativeInterface,
  GLInterfaceType(..),
  assembleInterface,

  -- * Miscellaneous utilities.
  validate,
  hasExtension,
)
where

import Control.Exception
import Skia.Internal.Prelude
import Skia.SkRefCnt qualified as SkRefCnt
import Language.C.Inline.Cpp qualified as C

C.context $ C.cppCtx <> C.funCtx <> cppSkiaObjectTypes

C.include "gpu/gl/GrGLInterface.h"
C.include "gpu/gl/GrGLAssembleInterface.h"

{- | Creates a native 'GrGLInterface'. Depending the execution platform, a GL
interface (e.g., GLX, WGL, EGL, etc) will be picked. If no 'GrGLInterface' is
constructed, an 'SkiaError' is thrown.
-}
createNativeInterface :: (MonadResource m) => m (ReleaseKey, GrGLInterface)
createNativeInterface =
  allocateSkObjectNeverNull'
    ( do
        iface' <- [C.block|const GrGLInterface* {
          return GrGLMakeNativeInterface().release();
        }|]
        when (iface' == nullPtr) do
          throwIO $ SkiaError "Cannot create native GL interface"
        pure iface'
    )
    SkRefCnt.decrement

data GLInterfaceType
  = OpenGL'Or'GLES
  -- ^ Creates a GrGLInterface for an either OpenGL or GLES
  | OpenGL
  -- ^ Creates a GrGLInterface for an OpenGL (but not GLES) context
  | GLES
  -- ^ Creates a GrGLInterface for an OpenGL ES (but not Open GL) context
  | WebGL
  -- ^ Creates a GrGLInterface for a WebGL (similar to OpenGL ES) context
  deriving (Show, Eq, Ord)

{- | Generic function for creating a GrGLInterface.
It calls the get proc function to get each function address.
-}
assembleInterface ::
  (MonadResource m) =>
  -- | Type of the requested 'GrGLInterface'
  GLInterfaceType ->
  -- | A function that resolves an OpenGL function name to a OpenGL function.
  (CString -> IO (Ptr ())) ->
  m (ReleaseKey, GrGLInterface)
assembleInterface ty resolver =
  allocateSkObjectNeverNull'
    ( do
        let
          typeId :: CInt
          typeId = case ty of
            OpenGL'Or'GLES -> 0
            OpenGL -> 1
            GLES -> 2
            WebGL -> 3
        iface' :: Ptr C'GrGLInterface <- [C.block|const GrGLInterface* {
          GrGLGetProc get_gl_proc = [](void* ctx, const char* name) -> GrGLFuncPtr {
            auto resolver = reinterpret_cast<void* (*)(const char*)>(ctx);
            return (GrGLFuncPtr) resolver(name);
          };

          void* ctx = (void *) $fun:(void* (*resolver)(const char*));

          int typeId = $(int typeId);
          if (typeId == 1) {
            // OpenGL
            return GrGLMakeAssembledGLInterface(ctx, get_gl_proc).release();
          } else if (typeId == 2) {
            // GLES
            return GrGLMakeAssembledGLESInterface(ctx, get_gl_proc).release();
          } else if (typeId == 3) {
            // WebGL
            return GrGLMakeAssembledWebGLInterface(ctx, get_gl_proc).release();
          } else {
            // OpenGL'Or'GLES
            return GrGLMakeAssembledInterface(ctx, get_gl_proc).release();
          }
        }|]
        when (iface' == nullPtr) do
          throwIO $ SkiaError "Cannot assemble GL interface"
        pure iface'
    )
    SkRefCnt.decrement

{- | Validates that the 'GrGLInterface' supports its advertised standard. This
means the necessary function pointers have been initialized for both the GL
version and any advertised extensions.
-}
validate :: (MonadIO m) => GrGLInterface -> m Bool
validate (ptr -> iface) = liftIO do
  toBool <$> [C.block| bool {
    return $(GrGLInterface* iface)->validate();
  }|]

{- | This helper function queries the current GL context for its extensions and
remembers them (internally done by Google Skia). It supports both @glGetString-@
and @glGetStringi-@style extension string APIs and will use the latter if it is
available. It also will query for EGL extensions if a @eglQueryString@
implementation is provided.
-}
hasExtension ::
  (MonadIO m) =>
  GrGLInterface ->
  -- | OpenGL extension name
  String ->
  m Bool
hasExtension (ptr -> iface) extName = evalManaged do
  -- N.B. Must be null-temrinated
  cextName <- managed $ withCString extName

  toBool <$> liftIO [C.block| bool {
    return $(GrGLInterface* iface)->hasExtension($(const char* cextName));
  }|]
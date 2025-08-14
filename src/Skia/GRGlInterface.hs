module Skia.GRGlInterface (
  -- * Creating 'GRGlInterface'
  createNativeInterface,
  GetProcUserData,
  GetProc,
  assembleInterface,
  assembleGlInterface,
  assembleGlesInterface,
  assembleWebGlInterface,

  -- * Miscellaneous utilities.
  validate,
  hasExtension,
)
where

import Control.Exception
import Skia.Bindings.Gr_context
import Skia.Bindings.Types
import Skia.Internal.Prelude
import Skia.SKRefCnt qualified as SKRefCnt

{- | Creates a native 'GRGlInterface'. Depending the execution platform, a GL
interface (e.g., GLX, WGL, EGL, etc) will be picked. If no 'GRGlInterface' is
constructed, an 'SkiaError' is thrown.
-}
createNativeInterface :: (MonadResource m) => m (ReleaseKey, GRGlInterface)
createNativeInterface =
  allocateSKObjectNeverNull'
    ( do
        iface' <- gr_glinterface_create_native_interface
        when (iface' == nullPtr) do
          throwIO $ SkiaError "Cannot create native GL interface"
        pure iface'
    )
    SKRefCnt.decrement

-- | See documentation of 'GetProc'
type GetProcUserData a = Ptr a

{- | A type alias for 'Gr_gl_get_proc'.

This is a type for functions that resolve OpenGL function names to real function
addresses.
-}
type GetProc a =
  -- | A pointer to some user data. You may use it however you like to help in
  -- resolving OpenGL functions.
  GetProcUserData a ->
  -- | Input OpenGL function name.
  CString ->
  -- | The return value should be the corresponding OpenGL function.
  IO (FunPtr Gr_gl_func_ptr)

-- | If no 'GRGlInterface' is constructed, an 'SkiaError' is thrown.
privAssemble ::
  (MonadResource m) =>
  (Ptr () -> FunPtr Gr_gl_get_proc -> IO (Ptr Gr_glinterface)) ->
  FunPtr (GetProc a) ->
  GetProcUserData a ->
  m (ReleaseKey, GRGlInterface)
privAssemble assembleFn getprocfunptr userData =
  allocateSKObjectNeverNull'
    ( do
        iface' <- assembleFn (castPtr userData) (castFunPtr getprocfunptr)
        when (iface' == nullPtr) do
          throwIO $ SkiaError "Cannot assemble GL interface"
        pure iface'
    )
    SKRefCnt.decrement

{- | Generic function for creating a GrGLInterface for an either OpenGL or GLES.
It calls the get proc function to get each function address.
-}
assembleInterface :: (MonadResource m) => FunPtr (GetProc a) -> GetProcUserData a -> m (ReleaseKey, GRGlInterface)
assembleInterface = privAssemble gr_glinterface_assemble_interface

{- | Generic function for creating a GrGLInterface for an OpenGL (but not GLES)
context. It calls the input get proc function to get each function address.
-}
assembleGlInterface :: (MonadResource m) => FunPtr (GetProc a) -> GetProcUserData a -> m (ReleaseKey, GRGlInterface)
assembleGlInterface = privAssemble gr_glinterface_assemble_gl_interface

{- | Generic function for creating a GrGLInterface for an OpenGL ES (but not
Open GL) context. It calls the input get proc function to get each function
address.
-}
assembleGlesInterface :: (MonadResource m) => FunPtr (GetProc a) -> GetProcUserData a -> m (ReleaseKey, GRGlInterface)
assembleGlesInterface = privAssemble gr_glinterface_assemble_gles_interface

{- | Generic function for creating a GrGLInterface for a WebGL (similar to
OpenGL ES) context. It calls the input get proc function to get each function
address.
-}
assembleWebGlInterface :: (MonadResource m) => FunPtr (GetProc a) -> GetProcUserData a -> m (ReleaseKey, GRGlInterface)
assembleWebGlInterface = privAssemble gr_glinterface_assemble_webgl_interface

{- | Validates that the 'GRGlInterface' supports its advertised standard. This
means the necessary function pointers have been initialized for both the GL
version and any advertised extensions.
-}
validate :: (MonadIO m) => GRGlInterface -> m Bool
validate iface = evalManaged do
  liftIO $ fmap toBool $ gr_glinterface_validate (ptr iface)

{- | This helper function queries the current GL context for its extensions and
remembers them (internally done by Google Skia). It supports both @glGetString-@
and @glGetStringi-@style extension string APIs and will use the latter if it is
available. It also will query for EGL extensions if a @eglQueryString@
implementation is provided.
-}
hasExtension ::
  (MonadIO m) =>
  GRGlInterface ->
  -- | OpenGL extension name
  CString ->
  m Bool
hasExtension iface extName = evalManaged do
  liftIO $ fmap toBool $ gr_glinterface_has_extension (ptr iface) extName

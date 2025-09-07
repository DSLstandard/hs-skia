module Skia.GrDirectContext where

import Language.C.Inline.Cpp qualified as C
import Skia.GrContextOptions
import Skia.Internal.Prelude
import Skia.SkRefCnt qualified as SkRefCnt

C.context $ C.cppCtx <> cppSkiaObjectTypes

C.include "gpu/GrDirectContext.h"
C.include "gpu/ganesh/gl/GrGLDirectContext.h"
C.include "gpu/gl/GrGLInterface.h"

{- | Creates a 'GrDirectContext' for an OpenGL context.

The reference counter of the returned 'GrDirectContext' is decremented when
'Acquire' releases.
-}
makeGL :: (MonadResource m) => GrGLInterface -> ContextOptions -> m (ReleaseKey, GrDirectContext)
makeGL (ptr -> iface') options =
  allocateSkObjectNeverNull'
    ( evalManaged do
        options' <- marshalGrContextOptions options
        liftIO [C.block|GrDirectContext* {
          return GrDirectContexts::MakeGL(
            sk_ref_sp($(GrGLInterface* iface')),
            *$(GrContextOptions* options')
          ).release();
        }|]
    )
    SkRefCnt.decrement
    
{- | Call to ensure all drawing to the context has been issued to the underlying
3D API.
-}
flush :: (MonadIO m) => GrDirectContext -> m ()
flush (ptr -> ctx') = liftIO do
  [C.block|void {
    $(GrDirectContext* ctx')->flush();
  }|]

{- | Submit outstanding work to the gpu from all previously un-submitted
flushes. The return value of the submit will indicate whether or not the
submission to the GPU was successful.

If the call returns true, all previously passed in semaphores in flush calls
will have been submitted to the GPU and they can safely be waited on. The caller
should wait on those semaphores or perform some other global synchronization
before deleting the semaphores.

If it returns false, then those same semaphores will not have been submitted and
we will not try to submit them again. The caller is free to delete the
semaphores at any time.

If \"sync\" is 'True', this function will return once the gpu has finished with
all submitted work.
-}
submit ::
  (MonadIO m) =>
  GrDirectContext ->
  -- | \"sync\"
  Bool ->
  m Bool
submit (ptr -> ctx') (fromBool -> sync) = liftIO do
  fmap toBool [C.block|bool {
    return $(GrDirectContext* ctx')->submit((GrSyncCpu) $(bool sync));
  }|]

{- | Call to ensure all drawing to the context has been flushed and submitted to
the underlying 3D API. This is equivalent to calling 'flush' with followed by
'submit(sync)'.
-}
flushAndSubmit ::
  (MonadIO m) =>
  GrDirectContext ->
  -- | \"sync\"
  Bool ->
  m ()
flushAndSubmit (ptr -> ctx') (fromBool -> sync) = liftIO do
  [C.block|void {
    $(GrDirectContext* ctx')->flushAndSubmit((GrSyncCpu) $(bool sync));
  }|]
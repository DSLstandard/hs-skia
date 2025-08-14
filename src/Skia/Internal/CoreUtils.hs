{-# LANGUAGE CPP #-}

-- | Internal utilities based on functions and datatypes from 'Skia.Core'.
module Skia.Internal.CoreUtils where

import Control.Monad.Trans.Resource
import Control.Exception
import Control.Monad.IO.Class
import Data.Text qualified as T
import Data.Typeable
import Foreign
import Prelude
import Skia.Core
import Skia.Errors

#ifdef HS_SKIA_SKIA_ASSERTIONS_ENABLED
import GHC.Stack
import Control.Monad
#endif

-- | Acquires an SKObject by acquiring its pointer.
--
-- The acquired pointer is assumed to never be null.
--
-- If the cabal package flag @enable-skia-assertions@ is enabled, an assertion
-- is made to check the pointer is not null. If the pointer is null, Haskell
-- 'error' is called.
--
-- You may throw arbitrary errors in the function that acquires the resource,
-- even when the acquired pointer is null.
allocateSKObjectNeverNull ::
#ifdef HS_SKIA_SKIA_ASSERTIONS_ENABLED
  (MonadResource m, SKObject s, PtrNewType s a, HasCallStack) =>
#else
  (MonadResource m, SKObject s, PtrNewType s a) =>
#endif
  -- | Acquire
  IO (Ptr a) ->
  -- | Release
  (Ptr a -> IO ()) ->
  m (ReleaseKey, s)
allocateSKObjectNeverNull acquire release = do
  allocate
    ( do
      p <- acquire
#ifdef HS_SKIA_SKIA_ASSERTIONS_ENABLED
      when (p == nullPtr) do
        Prelude.error "acquireSKObjectNeverNull got nullptr. This is a bug of this Haskell library."
#endif
      pure $ fromPtr p
    )
    ( \obj -> do
      release (ptr obj)
    )

-- | Like 'allocateSKObjectNeverNull' but release takes in the acquired
-- SKObject instead of its pointer.
--
-- This function is occasionally useful.
allocateSKObjectNeverNull' ::
#ifdef HS_SKIA_SKIA_ASSERTIONS_ENABLED
  (MonadResource m, SKObject s, PtrNewType s a, HasCallStack) =>
#else
  (MonadResource m, SKObject s, PtrNewType s a) =>
#endif
  -- | Acquire
  IO (Ptr a) ->
  -- | Release
  (s -> IO ()) ->
  m (ReleaseKey, s)
allocateSKObjectNeverNull' acquire release = do
  allocate
    ( do
      p <- acquire
#ifdef HS_SKIA_SKIA_ASSERTIONS_ENABLED
      when (p == nullPtr) do
        Prelude.error "acquireSKObjectNeverNull got nullptr. This is a bug of this Haskell library."
#endif
      pure $ fromPtr p
    )
    ( \obj -> do
      release obj
    )

-- | Like 'allocateSKObjectNeverNull', but when the acquired pointer is null, a
-- 'SkiaError' error is thrown with the specified error message.
allocateSKObjectOrErrorIfNull :: 
  (MonadResource m, SKObject s, PtrNewType s a) =>
  -- | Error message
  T.Text -> 
  -- | Acquire
  IO (Ptr a) ->
  -- | Release
  (Ptr a -> IO ()) ->
  m (ReleaseKey, s)
allocateSKObjectOrErrorIfNull ~errmsg acquire release =
  allocate
    ( do
      p <- acquire
      when (p == nullPtr) do
        Control.Exception.throwIO $ SkiaError (T.unpack errmsg)
      pure $ fromPtr p
    )
    ( \obj -> do
      release (ptr obj)
    )


-- | Like 'allocateSKObjectOrErrorIfNull' but release takes in the acquired
-- SKObject instead of its pointer.
--
-- This function is occasionally useful.
allocateSKObjectOrErrorIfNull' :: 
  (MonadResource m, SKObject s, PtrNewType s a) =>
  -- | Error message
  T.Text -> 
  -- | Acquire
  IO (Ptr a) ->
  -- | Release
  (s -> IO ()) ->
  m (ReleaseKey, s)
allocateSKObjectOrErrorIfNull' ~errmsg acquire release =
  allocate
    ( do
      p <- acquire
      when (p == nullPtr) do
        Control.Exception.throwIO $ SkiaError (T.unpack errmsg)
      pure $ fromPtr p
    )
    ( \obj -> do
      release obj
    )


-- | Like 'unmarshalSKEnum' but dies with an 'error' if the input enum cannot be
-- unmarshalled correctly.
unmarshalSKEnumOrDie :: forall a s m. (Show a, Typeable s, MonadIO m, SKEnum s a, HasCallStack
  ) => a -> m s
unmarshalSKEnumOrDie a = liftIO do
  case unmarshalSKEnum a of
    Just s -> do
      pure s
    Nothing -> do
      let enumName = tyConName $ typeRepTyCon $ typeRep (Proxy @s)
      error $ "Unrecognized '" <> enumName <> "' enum value: " <> show a

-- | TODO: Document 
allocateSKObjectOrNothingIfNull ::
  (SKObject s, PtrNewType s a, MonadResource m) =>
  -- | Acquire
  IO (Ptr a) ->
  -- | Release
  (Ptr a -> IO ()) ->
  m (Maybe (ReleaseKey, s))
allocateSKObjectOrNothingIfNull acquire release =
  resourceMask \_restore -> do
    p <- liftIO acquire
    if p == nullPtr
      then do
        pure Nothing
      else do
        releaseKey <- register (release p)
        pure $ Just (releaseKey, fromPtr p)


-- | TODO: Document 
allocateSKObjectOrNothingIfNull' ::
  (SKObject s, PtrNewType s a, MonadResource m) =>
  -- | Acquire
  IO (Ptr a) ->
  -- | Release
  (s -> IO ()) ->
  m (Maybe (ReleaseKey, s))
allocateSKObjectOrNothingIfNull' acquire release = do
  resourceMask \_restore -> do
    p <- liftIO acquire
    if p == nullPtr
      then do
        pure Nothing
      else do
        let obj = fromPtr p
        releaseKey <- register (release obj)
        pure $ Just (releaseKey, obj)

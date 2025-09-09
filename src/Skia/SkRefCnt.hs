{- | This module deals with 'SkRefCnt' objects.

The functions in this module are not meant to be used by typical users of this
library - the APIs of this library is designed in such a way that end-users do
not have to care about the boring low-level reference counting details. However,
if you wish, you may use the functions here to perform unsafe operations.
-}
module Skia.SkRefCnt where

import Skia.Internal.Prelude
import Language.C.Inline.Cpp qualified as C

C.context $ mconcat
  [ C.cppCtx
  , cppSkiaObjectTypes
  ]

C.include "core/SkRefCnt.h"

-- * Utils on @SkRefCnt@

{- | Increments the reference counter of the input 'SkObject'. Decrements when
'Acquire' is released.

This effectively keeps the input 'SkObject' alive at least unless this 'Acquire'
releases.
-}
holdRef :: (MonadResource m, SkObject s, IsSkRefCnt s) => s -> m ReleaseKey
holdRef refcnt = do
  (key, ()) <- allocate (increment refcnt) (\() -> decrement refcnt)
  pure key

-- | Increments the reference counter of the input 'SkObject'.
increment :: (MonadIO m, SkObject s, IsSkRefCnt s) => s -> m ()
increment (ptr . toA SkRefCnt -> refcnt') = liftIO do
  [C.block| void {
    SkSafeRef($(SkRefCnt* refcnt'));
  }|]

-- | Decrements the reference counter of the input 'SkObject'.
decrement :: (MonadIO m, SkObject s, IsSkRefCnt s) => s -> m ()
decrement (ptr . toA SkRefCnt -> refcnt') = liftIO do
  [C.block| void {
    SkSafeUnref($(SkRefCnt* refcnt'));
  }|]

-- * Utils on @SkNVRefCnt@

-- | 'SkNVRefCnt' version of 'holdRef'.
holdRefNV :: (MonadResource m, SkObject s, IsSkNVRefCnt s) => s -> m ReleaseKey
holdRefNV refcnt = do
  (key, ()) <- allocate (incrementNV refcnt) (\() -> decrementNV refcnt)
  pure key

-- | 'SkNVRefCnt' version of 'increment'.
incrementNV :: (MonadIO m, SkObject s, IsSkNVRefCnt s) => s -> m ()
incrementNV (castPtr @_ @() . ptr . toA SkNVRefCnt -> refcnt') = liftIO do
  [C.block| void {
    auto p = reinterpret_cast<const SkNVRefCnt<void*>*>($(void* refcnt'));
    SkSafeRef(p);
  }|]

-- | 'SkNVRefCnt' version of 'decrement'.
decrementNV :: (MonadIO m, SkObject s, IsSkNVRefCnt s) => s -> m ()
decrementNV (castPtr @_ @() . ptr . toA SkNVRefCnt -> refcnt') = liftIO do
  [C.block| void {
    auto p = reinterpret_cast<const SkNVRefCnt<void*>*>($(void* refcnt'));
    SkSafeUnref(p);
  }|]

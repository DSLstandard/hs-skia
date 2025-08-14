{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This is an auto-generated FFI binding module.
module Skia.Bindings.Sk_data where

import Foreign
import Foreign.C
import Foreign.Storable
import Foreign.Storable.Offset

import Skia.Bindings.Types


{- | C function signature:

@
sk_data_t *sk_data_new_empty(void)
@
-}
foreign import ccall "sk_data_new_empty" sk_data_new_empty ::
  IO (Ptr (Sk_data)) -- ^ C return type: @"sk_data_t *"@

-- | Function pointer to 'sk_data_new_empty'
foreign import ccall "&sk_data_new_empty" p'sk_data_new_empty ::
  FunPtr (IO (Ptr (Sk_data)))

{- | C function signature:

@
sk_data_t *sk_data_new_with_copy(const void *src, size_t length)
@
-}
foreign import ccall "sk_data_new_with_copy" sk_data_new_with_copy ::
  Ptr (()) -- ^ C argument @"const void * src"@
  -> CSize -- ^ C argument @"size_t length"@
  -> IO (Ptr (Sk_data)) -- ^ C return type: @"sk_data_t *"@

-- | Function pointer to 'sk_data_new_with_copy'
foreign import ccall "&sk_data_new_with_copy" p'sk_data_new_with_copy ::
  FunPtr (Ptr (()) -> CSize -> IO (Ptr (Sk_data)))

{- | C function signature:

@
sk_data_t *sk_data_new_subset(const sk_data_t *src, size_t offset, size_t length)
@
-}
foreign import ccall "sk_data_new_subset" sk_data_new_subset ::
  Ptr (Sk_data) -- ^ C argument @"const sk_data_t * src"@
  -> CSize -- ^ C argument @"size_t offset"@
  -> CSize -- ^ C argument @"size_t length"@
  -> IO (Ptr (Sk_data)) -- ^ C return type: @"sk_data_t *"@

-- | Function pointer to 'sk_data_new_subset'
foreign import ccall "&sk_data_new_subset" p'sk_data_new_subset ::
  FunPtr (Ptr (Sk_data) -> CSize -> CSize -> IO (Ptr (Sk_data)))

{- | C function signature:

@
void sk_data_ref(const sk_data_t *)
@
-}
foreign import ccall "sk_data_ref" sk_data_ref ::
  Ptr (Sk_data) -- ^ C argument type: @"const sk_data_t *"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_data_ref'
foreign import ccall "&sk_data_ref" p'sk_data_ref ::
  FunPtr (Ptr (Sk_data) -> IO (()))

{- | C function signature:

@
void sk_data_unref(const sk_data_t *)
@
-}
foreign import ccall "sk_data_unref" sk_data_unref ::
  Ptr (Sk_data) -- ^ C argument type: @"const sk_data_t *"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_data_unref'
foreign import ccall "&sk_data_unref" p'sk_data_unref ::
  FunPtr (Ptr (Sk_data) -> IO (()))

{- | C function signature:

@
size_t sk_data_get_size(const sk_data_t *)
@
-}
foreign import ccall "sk_data_get_size" sk_data_get_size ::
  Ptr (Sk_data) -- ^ C argument type: @"const sk_data_t *"@
  -> IO (CSize) -- ^ C return type: @"size_t"@

-- | Function pointer to 'sk_data_get_size'
foreign import ccall "&sk_data_get_size" p'sk_data_get_size ::
  FunPtr (Ptr (Sk_data) -> IO (CSize))

{- | C function signature:

@
const void *sk_data_get_data(const sk_data_t *)
@
-}
foreign import ccall "sk_data_get_data" sk_data_get_data ::
  Ptr (Sk_data) -- ^ C argument type: @"const sk_data_t *"@
  -> IO (Ptr (())) -- ^ C return type: @"const void *"@

-- | Function pointer to 'sk_data_get_data'
foreign import ccall "&sk_data_get_data" p'sk_data_get_data ::
  FunPtr (Ptr (Sk_data) -> IO (Ptr (())))

{- | C function signature:

@
sk_data_t *sk_data_new_from_file(const char *path)
@
-}
foreign import ccall "sk_data_new_from_file" sk_data_new_from_file ::
  Ptr (CChar) -- ^ C argument @"const char * path"@
  -> IO (Ptr (Sk_data)) -- ^ C return type: @"sk_data_t *"@

-- | Function pointer to 'sk_data_new_from_file'
foreign import ccall "&sk_data_new_from_file" p'sk_data_new_from_file ::
  FunPtr (Ptr (CChar) -> IO (Ptr (Sk_data)))

{- | C function signature:

@
sk_data_t *sk_data_new_from_stream(sk_stream_t *stream, size_t length)
@
-}
foreign import ccall "sk_data_new_from_stream" sk_data_new_from_stream ::
  Ptr (Sk_stream) -- ^ C argument @"sk_stream_t * stream"@
  -> CSize -- ^ C argument @"size_t length"@
  -> IO (Ptr (Sk_data)) -- ^ C return type: @"sk_data_t *"@

-- | Function pointer to 'sk_data_new_from_stream'
foreign import ccall "&sk_data_new_from_stream" p'sk_data_new_from_stream ::
  FunPtr (Ptr (Sk_stream) -> CSize -> IO (Ptr (Sk_data)))

{- | C function signature:

@
const uint8_t *sk_data_get_bytes(const sk_data_t *)
@
-}
foreign import ccall "sk_data_get_bytes" sk_data_get_bytes ::
  Ptr (Sk_data) -- ^ C argument type: @"const sk_data_t *"@
  -> IO (Ptr (Word8)) -- ^ C return type: @"const uint8_t *"@

-- | Function pointer to 'sk_data_get_bytes'
foreign import ccall "&sk_data_get_bytes" p'sk_data_get_bytes ::
  FunPtr (Ptr (Sk_data) -> IO (Ptr (Word8)))

{- | C function signature:

@
sk_data_t *sk_data_new_with_proc(const void *ptr, size_t length, sk_data_release_proc proc, void *ctx)
@
-}
foreign import ccall "sk_data_new_with_proc" sk_data_new_with_proc ::
  Ptr (()) -- ^ C argument @"const void * ptr"@
  -> CSize -- ^ C argument @"size_t length"@
  -> FunPtr Sk_data_release_proc -- ^ C argument @"sk_data_release_proc proc"@
  -> Ptr (()) -- ^ C argument @"void * ctx"@
  -> IO (Ptr (Sk_data)) -- ^ C return type: @"sk_data_t *"@

-- | Function pointer to 'sk_data_new_with_proc'
foreign import ccall "&sk_data_new_with_proc" p'sk_data_new_with_proc ::
  FunPtr (Ptr (()) -> CSize -> FunPtr Sk_data_release_proc -> Ptr (()) -> IO (Ptr (Sk_data)))

{- | C function signature:

@
sk_data_t *sk_data_new_uninitialized(size_t size)
@
-}
foreign import ccall "sk_data_new_uninitialized" sk_data_new_uninitialized ::
  CSize -- ^ C argument @"size_t size"@
  -> IO (Ptr (Sk_data)) -- ^ C return type: @"sk_data_t *"@

-- | Function pointer to 'sk_data_new_uninitialized'
foreign import ccall "&sk_data_new_uninitialized" p'sk_data_new_uninitialized ::
  FunPtr (CSize -> IO (Ptr (Sk_data)))

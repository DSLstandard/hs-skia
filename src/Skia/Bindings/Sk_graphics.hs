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
module Skia.Bindings.Sk_graphics where

import Foreign
import Foreign.C
import Foreign.Storable
import Foreign.Storable.Offset

import Skia.Bindings.Types


{- | C function signature:

@
void sk_graphics_init(void)
@
-}
foreign import ccall "sk_graphics_init" sk_graphics_init ::
  IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_graphics_init'
foreign import ccall "&sk_graphics_init" p'sk_graphics_init ::
  FunPtr (IO (()))

{- | C function signature:

@
void sk_graphics_purge_font_cache(void)
@
-}
foreign import ccall "sk_graphics_purge_font_cache" sk_graphics_purge_font_cache ::
  IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_graphics_purge_font_cache'
foreign import ccall "&sk_graphics_purge_font_cache" p'sk_graphics_purge_font_cache ::
  FunPtr (IO (()))

{- | C function signature:

@
void sk_graphics_purge_resource_cache(void)
@
-}
foreign import ccall "sk_graphics_purge_resource_cache" sk_graphics_purge_resource_cache ::
  IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_graphics_purge_resource_cache'
foreign import ccall "&sk_graphics_purge_resource_cache" p'sk_graphics_purge_resource_cache ::
  FunPtr (IO (()))

{- | C function signature:

@
void sk_graphics_purge_all_caches(void)
@
-}
foreign import ccall "sk_graphics_purge_all_caches" sk_graphics_purge_all_caches ::
  IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_graphics_purge_all_caches'
foreign import ccall "&sk_graphics_purge_all_caches" p'sk_graphics_purge_all_caches ::
  FunPtr (IO (()))

{- | C function signature:

@
size_t sk_graphics_get_font_cache_used(void)
@
-}
foreign import ccall "sk_graphics_get_font_cache_used" sk_graphics_get_font_cache_used ::
  IO (CSize) -- ^ C return type: @"size_t"@

-- | Function pointer to 'sk_graphics_get_font_cache_used'
foreign import ccall "&sk_graphics_get_font_cache_used" p'sk_graphics_get_font_cache_used ::
  FunPtr (IO (CSize))

{- | C function signature:

@
size_t sk_graphics_get_font_cache_limit(void)
@
-}
foreign import ccall "sk_graphics_get_font_cache_limit" sk_graphics_get_font_cache_limit ::
  IO (CSize) -- ^ C return type: @"size_t"@

-- | Function pointer to 'sk_graphics_get_font_cache_limit'
foreign import ccall "&sk_graphics_get_font_cache_limit" p'sk_graphics_get_font_cache_limit ::
  FunPtr (IO (CSize))

{- | C function signature:

@
size_t sk_graphics_set_font_cache_limit(size_t bytes)
@
-}
foreign import ccall "sk_graphics_set_font_cache_limit" sk_graphics_set_font_cache_limit ::
  CSize -- ^ C argument @"size_t bytes"@
  -> IO (CSize) -- ^ C return type: @"size_t"@

-- | Function pointer to 'sk_graphics_set_font_cache_limit'
foreign import ccall "&sk_graphics_set_font_cache_limit" p'sk_graphics_set_font_cache_limit ::
  FunPtr (CSize -> IO (CSize))

{- | C function signature:

@
int sk_graphics_get_font_cache_count_used(void)
@
-}
foreign import ccall "sk_graphics_get_font_cache_count_used" sk_graphics_get_font_cache_count_used ::
  IO (CInt) -- ^ C return type: @"int"@

-- | Function pointer to 'sk_graphics_get_font_cache_count_used'
foreign import ccall "&sk_graphics_get_font_cache_count_used" p'sk_graphics_get_font_cache_count_used ::
  FunPtr (IO (CInt))

{- | C function signature:

@
int sk_graphics_get_font_cache_count_limit(void)
@
-}
foreign import ccall "sk_graphics_get_font_cache_count_limit" sk_graphics_get_font_cache_count_limit ::
  IO (CInt) -- ^ C return type: @"int"@

-- | Function pointer to 'sk_graphics_get_font_cache_count_limit'
foreign import ccall "&sk_graphics_get_font_cache_count_limit" p'sk_graphics_get_font_cache_count_limit ::
  FunPtr (IO (CInt))

{- | C function signature:

@
int sk_graphics_set_font_cache_count_limit(int count)
@
-}
foreign import ccall "sk_graphics_set_font_cache_count_limit" sk_graphics_set_font_cache_count_limit ::
  CInt -- ^ C argument @"int count"@
  -> IO (CInt) -- ^ C return type: @"int"@

-- | Function pointer to 'sk_graphics_set_font_cache_count_limit'
foreign import ccall "&sk_graphics_set_font_cache_count_limit" p'sk_graphics_set_font_cache_count_limit ::
  FunPtr (CInt -> IO (CInt))

{- | C function signature:

@
size_t sk_graphics_get_resource_cache_total_bytes_used(void)
@
-}
foreign import ccall "sk_graphics_get_resource_cache_total_bytes_used" sk_graphics_get_resource_cache_total_bytes_used ::
  IO (CSize) -- ^ C return type: @"size_t"@

-- | Function pointer to 'sk_graphics_get_resource_cache_total_bytes_used'
foreign import ccall "&sk_graphics_get_resource_cache_total_bytes_used" p'sk_graphics_get_resource_cache_total_bytes_used ::
  FunPtr (IO (CSize))

{- | C function signature:

@
size_t sk_graphics_get_resource_cache_total_byte_limit(void)
@
-}
foreign import ccall "sk_graphics_get_resource_cache_total_byte_limit" sk_graphics_get_resource_cache_total_byte_limit ::
  IO (CSize) -- ^ C return type: @"size_t"@

-- | Function pointer to 'sk_graphics_get_resource_cache_total_byte_limit'
foreign import ccall "&sk_graphics_get_resource_cache_total_byte_limit" p'sk_graphics_get_resource_cache_total_byte_limit ::
  FunPtr (IO (CSize))

{- | C function signature:

@
size_t sk_graphics_set_resource_cache_total_byte_limit(size_t newLimit)
@
-}
foreign import ccall "sk_graphics_set_resource_cache_total_byte_limit" sk_graphics_set_resource_cache_total_byte_limit ::
  CSize -- ^ C argument @"size_t newLimit"@
  -> IO (CSize) -- ^ C return type: @"size_t"@

-- | Function pointer to 'sk_graphics_set_resource_cache_total_byte_limit'
foreign import ccall "&sk_graphics_set_resource_cache_total_byte_limit" p'sk_graphics_set_resource_cache_total_byte_limit ::
  FunPtr (CSize -> IO (CSize))

{- | C function signature:

@
size_t sk_graphics_get_resource_cache_single_allocation_byte_limit(void)
@
-}
foreign import ccall "sk_graphics_get_resource_cache_single_allocation_byte_limit" sk_graphics_get_resource_cache_single_allocation_byte_limit ::
  IO (CSize) -- ^ C return type: @"size_t"@

-- | Function pointer to 'sk_graphics_get_resource_cache_single_allocation_byte_limit'
foreign import ccall "&sk_graphics_get_resource_cache_single_allocation_byte_limit" p'sk_graphics_get_resource_cache_single_allocation_byte_limit ::
  FunPtr (IO (CSize))

{- | C function signature:

@
size_t sk_graphics_set_resource_cache_single_allocation_byte_limit(size_t newLimit)
@
-}
foreign import ccall "sk_graphics_set_resource_cache_single_allocation_byte_limit" sk_graphics_set_resource_cache_single_allocation_byte_limit ::
  CSize -- ^ C argument @"size_t newLimit"@
  -> IO (CSize) -- ^ C return type: @"size_t"@

-- | Function pointer to 'sk_graphics_set_resource_cache_single_allocation_byte_limit'
foreign import ccall "&sk_graphics_set_resource_cache_single_allocation_byte_limit" p'sk_graphics_set_resource_cache_single_allocation_byte_limit ::
  FunPtr (CSize -> IO (CSize))

{- | C function signature:

@
void sk_graphics_dump_memory_statistics(sk_tracememorydump_t *dump)
@
-}
foreign import ccall "sk_graphics_dump_memory_statistics" sk_graphics_dump_memory_statistics ::
  Ptr (Sk_tracememorydump) -- ^ C argument @"sk_tracememorydump_t * dump"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_graphics_dump_memory_statistics'
foreign import ccall "&sk_graphics_dump_memory_statistics" p'sk_graphics_dump_memory_statistics ::
  FunPtr (Ptr (Sk_tracememorydump) -> IO (()))

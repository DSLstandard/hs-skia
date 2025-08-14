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
module Skia.Bindings.Sksg_invalidation_controller where

import Foreign
import Foreign.C
import Foreign.Storable
import Foreign.Storable.Offset

import Skia.Bindings.Types


{- | C function signature:

@
sksg_invalidation_controller_t *sksg_invalidation_controller_new(void)
@
-}
foreign import ccall "sksg_invalidation_controller_new" sksg_invalidation_controller_new ::
  IO (Ptr (Sksg_invalidation_controller)) -- ^ C return type: @"sksg_invalidation_controller_t *"@

-- | Function pointer to 'sksg_invalidation_controller_new'
foreign import ccall "&sksg_invalidation_controller_new" p'sksg_invalidation_controller_new ::
  FunPtr (IO (Ptr (Sksg_invalidation_controller)))

{- | C function signature:

@
void sksg_invalidation_controller_delete(sksg_invalidation_controller_t *instance)
@
-}
foreign import ccall "sksg_invalidation_controller_delete" sksg_invalidation_controller_delete ::
  Ptr (Sksg_invalidation_controller) -- ^ C argument @"sksg_invalidation_controller_t * instance"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sksg_invalidation_controller_delete'
foreign import ccall "&sksg_invalidation_controller_delete" p'sksg_invalidation_controller_delete ::
  FunPtr (Ptr (Sksg_invalidation_controller) -> IO (()))

{- | C function signature:

@
void sksg_invalidation_controller_inval(sksg_invalidation_controller_t *instance, sk_rect_t *rect, sk_matrix_t *matrix)
@
-}
foreign import ccall "sksg_invalidation_controller_inval" sksg_invalidation_controller_inval ::
  Ptr (Sksg_invalidation_controller) -- ^ C argument @"sksg_invalidation_controller_t * instance"@
  -> Ptr (Sk_rect) -- ^ C argument @"sk_rect_t * rect"@
  -> Ptr (Sk_matrix) -- ^ C argument @"sk_matrix_t * matrix"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sksg_invalidation_controller_inval'
foreign import ccall "&sksg_invalidation_controller_inval" p'sksg_invalidation_controller_inval ::
  FunPtr (Ptr (Sksg_invalidation_controller) -> Ptr (Sk_rect) -> Ptr (Sk_matrix) -> IO (()))

{- | C function signature:

@
void sksg_invalidation_controller_get_bounds(sksg_invalidation_controller_t *instance, sk_rect_t *bounds)
@
-}
foreign import ccall "sksg_invalidation_controller_get_bounds" sksg_invalidation_controller_get_bounds ::
  Ptr (Sksg_invalidation_controller) -- ^ C argument @"sksg_invalidation_controller_t * instance"@
  -> Ptr (Sk_rect) -- ^ C argument @"sk_rect_t * bounds"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sksg_invalidation_controller_get_bounds'
foreign import ccall "&sksg_invalidation_controller_get_bounds" p'sksg_invalidation_controller_get_bounds ::
  FunPtr (Ptr (Sksg_invalidation_controller) -> Ptr (Sk_rect) -> IO (()))

{- | C function signature:

@
void sksg_invalidation_controller_begin(sksg_invalidation_controller_t *instance)
@
-}
foreign import ccall "sksg_invalidation_controller_begin" sksg_invalidation_controller_begin ::
  Ptr (Sksg_invalidation_controller) -- ^ C argument @"sksg_invalidation_controller_t * instance"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sksg_invalidation_controller_begin'
foreign import ccall "&sksg_invalidation_controller_begin" p'sksg_invalidation_controller_begin ::
  FunPtr (Ptr (Sksg_invalidation_controller) -> IO (()))

{- | C function signature:

@
void sksg_invalidation_controller_end(sksg_invalidation_controller_t *instance)
@
-}
foreign import ccall "sksg_invalidation_controller_end" sksg_invalidation_controller_end ::
  Ptr (Sksg_invalidation_controller) -- ^ C argument @"sksg_invalidation_controller_t * instance"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sksg_invalidation_controller_end'
foreign import ccall "&sksg_invalidation_controller_end" p'sksg_invalidation_controller_end ::
  FunPtr (Ptr (Sksg_invalidation_controller) -> IO (()))

{- | C function signature:

@
void sksg_invalidation_controller_reset(sksg_invalidation_controller_t *instance)
@
-}
foreign import ccall "sksg_invalidation_controller_reset" sksg_invalidation_controller_reset ::
  Ptr (Sksg_invalidation_controller) -- ^ C argument @"sksg_invalidation_controller_t * instance"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sksg_invalidation_controller_reset'
foreign import ccall "&sksg_invalidation_controller_reset" p'sksg_invalidation_controller_reset ::
  FunPtr (Ptr (Sksg_invalidation_controller) -> IO (()))

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
module Skia.Bindings.Sk_matrix where

import Foreign
import Foreign.C
import Foreign.Storable
import Foreign.Storable.Offset

import Skia.Bindings.Types


{- | C function signature:

@
_Bool sk_matrix_try_invert(sk_matrix_t *matrix, sk_matrix_t *result)
@
-}
foreign import ccall "sk_matrix_try_invert" sk_matrix_try_invert ::
  Ptr (Sk_matrix) -- ^ C argument @"sk_matrix_t * matrix"@
  -> Ptr (Sk_matrix) -- ^ C argument @"sk_matrix_t * result"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_matrix_try_invert'
foreign import ccall "&sk_matrix_try_invert" p'sk_matrix_try_invert ::
  FunPtr (Ptr (Sk_matrix) -> Ptr (Sk_matrix) -> IO (CBool))

{- | C function signature:

@
void sk_matrix_concat(sk_matrix_t *result, sk_matrix_t *first, sk_matrix_t *second)
@
-}
foreign import ccall "sk_matrix_concat" sk_matrix_concat ::
  Ptr (Sk_matrix) -- ^ C argument @"sk_matrix_t * result"@
  -> Ptr (Sk_matrix) -- ^ C argument @"sk_matrix_t * first"@
  -> Ptr (Sk_matrix) -- ^ C argument @"sk_matrix_t * second"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_matrix_concat'
foreign import ccall "&sk_matrix_concat" p'sk_matrix_concat ::
  FunPtr (Ptr (Sk_matrix) -> Ptr (Sk_matrix) -> Ptr (Sk_matrix) -> IO (()))

{- | C function signature:

@
void sk_matrix_pre_concat(sk_matrix_t *result, sk_matrix_t *matrix)
@
-}
foreign import ccall "sk_matrix_pre_concat" sk_matrix_pre_concat ::
  Ptr (Sk_matrix) -- ^ C argument @"sk_matrix_t * result"@
  -> Ptr (Sk_matrix) -- ^ C argument @"sk_matrix_t * matrix"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_matrix_pre_concat'
foreign import ccall "&sk_matrix_pre_concat" p'sk_matrix_pre_concat ::
  FunPtr (Ptr (Sk_matrix) -> Ptr (Sk_matrix) -> IO (()))

{- | C function signature:

@
void sk_matrix_post_concat(sk_matrix_t *result, sk_matrix_t *matrix)
@
-}
foreign import ccall "sk_matrix_post_concat" sk_matrix_post_concat ::
  Ptr (Sk_matrix) -- ^ C argument @"sk_matrix_t * result"@
  -> Ptr (Sk_matrix) -- ^ C argument @"sk_matrix_t * matrix"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_matrix_post_concat'
foreign import ccall "&sk_matrix_post_concat" p'sk_matrix_post_concat ::
  FunPtr (Ptr (Sk_matrix) -> Ptr (Sk_matrix) -> IO (()))

{- | C function signature:

@
void sk_matrix_map_rect(sk_matrix_t *matrix, sk_rect_t *dest, sk_rect_t *source)
@
-}
foreign import ccall "sk_matrix_map_rect" sk_matrix_map_rect ::
  Ptr (Sk_matrix) -- ^ C argument @"sk_matrix_t * matrix"@
  -> Ptr (Sk_rect) -- ^ C argument @"sk_rect_t * dest"@
  -> Ptr (Sk_rect) -- ^ C argument @"sk_rect_t * source"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_matrix_map_rect'
foreign import ccall "&sk_matrix_map_rect" p'sk_matrix_map_rect ::
  FunPtr (Ptr (Sk_matrix) -> Ptr (Sk_rect) -> Ptr (Sk_rect) -> IO (()))

{- | C function signature:

@
void sk_matrix_map_points(sk_matrix_t *matrix, sk_point_t *dst, sk_point_t *src, int count)
@
-}
foreign import ccall "sk_matrix_map_points" sk_matrix_map_points ::
  Ptr (Sk_matrix) -- ^ C argument @"sk_matrix_t * matrix"@
  -> Ptr (Sk_point) -- ^ C argument @"sk_point_t * dst"@
  -> Ptr (Sk_point) -- ^ C argument @"sk_point_t * src"@
  -> CInt -- ^ C argument @"int count"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_matrix_map_points'
foreign import ccall "&sk_matrix_map_points" p'sk_matrix_map_points ::
  FunPtr (Ptr (Sk_matrix) -> Ptr (Sk_point) -> Ptr (Sk_point) -> CInt -> IO (()))

{- | C function signature:

@
void sk_matrix_map_vectors(sk_matrix_t *matrix, sk_point_t *dst, sk_point_t *src, int count)
@
-}
foreign import ccall "sk_matrix_map_vectors" sk_matrix_map_vectors ::
  Ptr (Sk_matrix) -- ^ C argument @"sk_matrix_t * matrix"@
  -> Ptr (Sk_point) -- ^ C argument @"sk_point_t * dst"@
  -> Ptr (Sk_point) -- ^ C argument @"sk_point_t * src"@
  -> CInt -- ^ C argument @"int count"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_matrix_map_vectors'
foreign import ccall "&sk_matrix_map_vectors" p'sk_matrix_map_vectors ::
  FunPtr (Ptr (Sk_matrix) -> Ptr (Sk_point) -> Ptr (Sk_point) -> CInt -> IO (()))

{- | C function signature:

@
void sk_matrix_map_xy(sk_matrix_t *matrix, float x, float y, sk_point_t *result)
@
-}
foreign import ccall "sk_matrix_map_xy" sk_matrix_map_xy ::
  Ptr (Sk_matrix) -- ^ C argument @"sk_matrix_t * matrix"@
  -> CFloat -- ^ C argument @"float x"@
  -> CFloat -- ^ C argument @"float y"@
  -> Ptr (Sk_point) -- ^ C argument @"sk_point_t * result"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_matrix_map_xy'
foreign import ccall "&sk_matrix_map_xy" p'sk_matrix_map_xy ::
  FunPtr (Ptr (Sk_matrix) -> CFloat -> CFloat -> Ptr (Sk_point) -> IO (()))

{- | C function signature:

@
void sk_matrix_map_vector(sk_matrix_t *matrix, float x, float y, sk_point_t *result)
@
-}
foreign import ccall "sk_matrix_map_vector" sk_matrix_map_vector ::
  Ptr (Sk_matrix) -- ^ C argument @"sk_matrix_t * matrix"@
  -> CFloat -- ^ C argument @"float x"@
  -> CFloat -- ^ C argument @"float y"@
  -> Ptr (Sk_point) -- ^ C argument @"sk_point_t * result"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_matrix_map_vector'
foreign import ccall "&sk_matrix_map_vector" p'sk_matrix_map_vector ::
  FunPtr (Ptr (Sk_matrix) -> CFloat -> CFloat -> Ptr (Sk_point) -> IO (()))

{- | C function signature:

@
float sk_matrix_map_radius(sk_matrix_t *matrix, float radius)
@
-}
foreign import ccall "sk_matrix_map_radius" sk_matrix_map_radius ::
  Ptr (Sk_matrix) -- ^ C argument @"sk_matrix_t * matrix"@
  -> CFloat -- ^ C argument @"float radius"@
  -> IO (CFloat) -- ^ C return type: @"float"@

-- | Function pointer to 'sk_matrix_map_radius'
foreign import ccall "&sk_matrix_map_radius" p'sk_matrix_map_radius ::
  FunPtr (Ptr (Sk_matrix) -> CFloat -> IO (CFloat))

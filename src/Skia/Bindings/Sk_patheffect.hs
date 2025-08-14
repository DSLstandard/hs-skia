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
module Skia.Bindings.Sk_patheffect where

import Foreign
import Foreign.C
import Foreign.Storable
import Foreign.Storable.Offset

import Skia.Bindings.Types


{- | C function signature:

@
void sk_path_effect_unref(sk_path_effect_t *t)
@
-}
foreign import ccall "sk_path_effect_unref" sk_path_effect_unref ::
  Ptr (Sk_path_effect) -- ^ C argument @"sk_path_effect_t * t"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_path_effect_unref'
foreign import ccall "&sk_path_effect_unref" p'sk_path_effect_unref ::
  FunPtr (Ptr (Sk_path_effect) -> IO (()))

{- | C function signature:

@
sk_path_effect_t *sk_path_effect_create_compose(sk_path_effect_t *outer, sk_path_effect_t *inner)
@
-}
foreign import ccall "sk_path_effect_create_compose" sk_path_effect_create_compose ::
  Ptr (Sk_path_effect) -- ^ C argument @"sk_path_effect_t * outer"@
  -> Ptr (Sk_path_effect) -- ^ C argument @"sk_path_effect_t * inner"@
  -> IO (Ptr (Sk_path_effect)) -- ^ C return type: @"sk_path_effect_t *"@

-- | Function pointer to 'sk_path_effect_create_compose'
foreign import ccall "&sk_path_effect_create_compose" p'sk_path_effect_create_compose ::
  FunPtr (Ptr (Sk_path_effect) -> Ptr (Sk_path_effect) -> IO (Ptr (Sk_path_effect)))

{- | C function signature:

@
sk_path_effect_t *sk_path_effect_create_sum(sk_path_effect_t *first, sk_path_effect_t *second)
@
-}
foreign import ccall "sk_path_effect_create_sum" sk_path_effect_create_sum ::
  Ptr (Sk_path_effect) -- ^ C argument @"sk_path_effect_t * first"@
  -> Ptr (Sk_path_effect) -- ^ C argument @"sk_path_effect_t * second"@
  -> IO (Ptr (Sk_path_effect)) -- ^ C return type: @"sk_path_effect_t *"@

-- | Function pointer to 'sk_path_effect_create_sum'
foreign import ccall "&sk_path_effect_create_sum" p'sk_path_effect_create_sum ::
  FunPtr (Ptr (Sk_path_effect) -> Ptr (Sk_path_effect) -> IO (Ptr (Sk_path_effect)))

{- | C function signature:

@
sk_path_effect_t *sk_path_effect_create_discrete(float segLength, float deviation, uint32_t seedAssist)
@
-}
foreign import ccall "sk_path_effect_create_discrete" sk_path_effect_create_discrete ::
  CFloat -- ^ C argument @"float segLength"@
  -> CFloat -- ^ C argument @"float deviation"@
  -> Word32 -- ^ C argument @"uint32_t seedAssist"@
  -> IO (Ptr (Sk_path_effect)) -- ^ C return type: @"sk_path_effect_t *"@

-- | Function pointer to 'sk_path_effect_create_discrete'
foreign import ccall "&sk_path_effect_create_discrete" p'sk_path_effect_create_discrete ::
  FunPtr (CFloat -> CFloat -> Word32 -> IO (Ptr (Sk_path_effect)))

{- | C function signature:

@
sk_path_effect_t *sk_path_effect_create_corner(float radius)
@
-}
foreign import ccall "sk_path_effect_create_corner" sk_path_effect_create_corner ::
  CFloat -- ^ C argument @"float radius"@
  -> IO (Ptr (Sk_path_effect)) -- ^ C return type: @"sk_path_effect_t *"@

-- | Function pointer to 'sk_path_effect_create_corner'
foreign import ccall "&sk_path_effect_create_corner" p'sk_path_effect_create_corner ::
  FunPtr (CFloat -> IO (Ptr (Sk_path_effect)))

{- | C function signature:

@
sk_path_effect_t *sk_path_effect_create_1d_path(const sk_path_t *path, float advance, float phase, sk_path_effect_1d_style_t style)
@
-}
foreign import ccall "sk_path_effect_create_1d_path" sk_path_effect_create_1d_path ::
  Ptr (Sk_path) -- ^ C argument @"const sk_path_t * path"@
  -> CFloat -- ^ C argument @"float advance"@
  -> CFloat -- ^ C argument @"float phase"@
  -> Sk_path_effect_1d_style -- ^ C argument @"sk_path_effect_1d_style_t style"@
  -> IO (Ptr (Sk_path_effect)) -- ^ C return type: @"sk_path_effect_t *"@

-- | Function pointer to 'sk_path_effect_create_1d_path'
foreign import ccall "&sk_path_effect_create_1d_path" p'sk_path_effect_create_1d_path ::
  FunPtr (Ptr (Sk_path) -> CFloat -> CFloat -> Sk_path_effect_1d_style -> IO (Ptr (Sk_path_effect)))

{- | C function signature:

@
sk_path_effect_t *sk_path_effect_create_2d_line(float width, const sk_matrix_t *matrix)
@
-}
foreign import ccall "sk_path_effect_create_2d_line" sk_path_effect_create_2d_line ::
  CFloat -- ^ C argument @"float width"@
  -> Ptr (Sk_matrix) -- ^ C argument @"const sk_matrix_t * matrix"@
  -> IO (Ptr (Sk_path_effect)) -- ^ C return type: @"sk_path_effect_t *"@

-- | Function pointer to 'sk_path_effect_create_2d_line'
foreign import ccall "&sk_path_effect_create_2d_line" p'sk_path_effect_create_2d_line ::
  FunPtr (CFloat -> Ptr (Sk_matrix) -> IO (Ptr (Sk_path_effect)))

{- | C function signature:

@
sk_path_effect_t *sk_path_effect_create_2d_path(const sk_matrix_t *matrix, const sk_path_t *path)
@
-}
foreign import ccall "sk_path_effect_create_2d_path" sk_path_effect_create_2d_path ::
  Ptr (Sk_matrix) -- ^ C argument @"const sk_matrix_t * matrix"@
  -> Ptr (Sk_path) -- ^ C argument @"const sk_path_t * path"@
  -> IO (Ptr (Sk_path_effect)) -- ^ C return type: @"sk_path_effect_t *"@

-- | Function pointer to 'sk_path_effect_create_2d_path'
foreign import ccall "&sk_path_effect_create_2d_path" p'sk_path_effect_create_2d_path ::
  FunPtr (Ptr (Sk_matrix) -> Ptr (Sk_path) -> IO (Ptr (Sk_path_effect)))

{- | C function signature:

@
sk_path_effect_t *sk_path_effect_create_dash(const float intervals[], int count, float phase)
@
-}
foreign import ccall "sk_path_effect_create_dash" sk_path_effect_create_dash ::
  Ptr (CFloat) -- ^ C argument @"const float [] intervals"@
  -> CInt -- ^ C argument @"int count"@
  -> CFloat -- ^ C argument @"float phase"@
  -> IO (Ptr (Sk_path_effect)) -- ^ C return type: @"sk_path_effect_t *"@

-- | Function pointer to 'sk_path_effect_create_dash'
foreign import ccall "&sk_path_effect_create_dash" p'sk_path_effect_create_dash ::
  FunPtr (Ptr (CFloat) -> CInt -> CFloat -> IO (Ptr (Sk_path_effect)))

{- | C function signature:

@
sk_path_effect_t *sk_path_effect_create_trim(float start, float stop, sk_path_effect_trim_mode_t mode)
@
-}
foreign import ccall "sk_path_effect_create_trim" sk_path_effect_create_trim ::
  CFloat -- ^ C argument @"float start"@
  -> CFloat -- ^ C argument @"float stop"@
  -> Sk_path_effect_trim_mode -- ^ C argument @"sk_path_effect_trim_mode_t mode"@
  -> IO (Ptr (Sk_path_effect)) -- ^ C return type: @"sk_path_effect_t *"@

-- | Function pointer to 'sk_path_effect_create_trim'
foreign import ccall "&sk_path_effect_create_trim" p'sk_path_effect_create_trim ::
  FunPtr (CFloat -> CFloat -> Sk_path_effect_trim_mode -> IO (Ptr (Sk_path_effect)))

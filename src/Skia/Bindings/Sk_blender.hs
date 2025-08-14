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
module Skia.Bindings.Sk_blender where

import Foreign
import Foreign.C
import Foreign.Storable
import Foreign.Storable.Offset

import Skia.Bindings.Types


{- | C function signature:

@
void sk_blender_ref(sk_blender_t *blender)
@
-}
foreign import ccall "sk_blender_ref" sk_blender_ref ::
  Ptr (Sk_blender) -- ^ C argument @"sk_blender_t * blender"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_blender_ref'
foreign import ccall "&sk_blender_ref" p'sk_blender_ref ::
  FunPtr (Ptr (Sk_blender) -> IO (()))

{- | C function signature:

@
void sk_blender_unref(sk_blender_t *blender)
@
-}
foreign import ccall "sk_blender_unref" sk_blender_unref ::
  Ptr (Sk_blender) -- ^ C argument @"sk_blender_t * blender"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_blender_unref'
foreign import ccall "&sk_blender_unref" p'sk_blender_unref ::
  FunPtr (Ptr (Sk_blender) -> IO (()))

{- | C function signature:

@
sk_blender_t *sk_blender_new_mode(sk_blendmode_t mode)
@
-}
foreign import ccall "sk_blender_new_mode" sk_blender_new_mode ::
  Sk_blendmode -- ^ C argument @"sk_blendmode_t mode"@
  -> IO (Ptr (Sk_blender)) -- ^ C return type: @"sk_blender_t *"@

-- | Function pointer to 'sk_blender_new_mode'
foreign import ccall "&sk_blender_new_mode" p'sk_blender_new_mode ::
  FunPtr (Sk_blendmode -> IO (Ptr (Sk_blender)))

{- | C function signature:

@
sk_blender_t *sk_blender_new_arithmetic(float k1, float k2, float k3, float k4, _Bool enforcePremul)
@
-}
foreign import ccall "sk_blender_new_arithmetic" sk_blender_new_arithmetic ::
  CFloat -- ^ C argument @"float k1"@
  -> CFloat -- ^ C argument @"float k2"@
  -> CFloat -- ^ C argument @"float k3"@
  -> CFloat -- ^ C argument @"float k4"@
  -> CBool -- ^ C argument @"_Bool enforcePremul"@
  -> IO (Ptr (Sk_blender)) -- ^ C return type: @"sk_blender_t *"@

-- | Function pointer to 'sk_blender_new_arithmetic'
foreign import ccall "&sk_blender_new_arithmetic" p'sk_blender_new_arithmetic ::
  FunPtr (CFloat -> CFloat -> CFloat -> CFloat -> CBool -> IO (Ptr (Sk_blender)))

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
module Skia.Bindings.Sk_maskfilter where

import Foreign
import Foreign.C
import Foreign.Storable
import Foreign.Storable.Offset

import Skia.Bindings.Types


{- | C function signature:

@
void sk_maskfilter_ref(sk_maskfilter_t *)
@
-}
foreign import ccall "sk_maskfilter_ref" sk_maskfilter_ref ::
  Ptr (Sk_maskfilter) -- ^ C argument type: @"sk_maskfilter_t *"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_maskfilter_ref'
foreign import ccall "&sk_maskfilter_ref" p'sk_maskfilter_ref ::
  FunPtr (Ptr (Sk_maskfilter) -> IO (()))

{- | C function signature:

@
void sk_maskfilter_unref(sk_maskfilter_t *)
@
-}
foreign import ccall "sk_maskfilter_unref" sk_maskfilter_unref ::
  Ptr (Sk_maskfilter) -- ^ C argument type: @"sk_maskfilter_t *"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_maskfilter_unref'
foreign import ccall "&sk_maskfilter_unref" p'sk_maskfilter_unref ::
  FunPtr (Ptr (Sk_maskfilter) -> IO (()))

{- | C function signature:

@
sk_maskfilter_t *sk_maskfilter_new_blur(sk_blurstyle_t, float sigma)
@
-}
foreign import ccall "sk_maskfilter_new_blur" sk_maskfilter_new_blur ::
  Sk_blurstyle -- ^ C argument type: @"sk_blurstyle_t"@
  -> CFloat -- ^ C argument @"float sigma"@
  -> IO (Ptr (Sk_maskfilter)) -- ^ C return type: @"sk_maskfilter_t *"@

-- | Function pointer to 'sk_maskfilter_new_blur'
foreign import ccall "&sk_maskfilter_new_blur" p'sk_maskfilter_new_blur ::
  FunPtr (Sk_blurstyle -> CFloat -> IO (Ptr (Sk_maskfilter)))

{- | C function signature:

@
sk_maskfilter_t *sk_maskfilter_new_blur_with_flags(sk_blurstyle_t, float sigma, _Bool respectCTM)
@
-}
foreign import ccall "sk_maskfilter_new_blur_with_flags" sk_maskfilter_new_blur_with_flags ::
  Sk_blurstyle -- ^ C argument type: @"sk_blurstyle_t"@
  -> CFloat -- ^ C argument @"float sigma"@
  -> CBool -- ^ C argument @"_Bool respectCTM"@
  -> IO (Ptr (Sk_maskfilter)) -- ^ C return type: @"sk_maskfilter_t *"@

-- | Function pointer to 'sk_maskfilter_new_blur_with_flags'
foreign import ccall "&sk_maskfilter_new_blur_with_flags" p'sk_maskfilter_new_blur_with_flags ::
  FunPtr (Sk_blurstyle -> CFloat -> CBool -> IO (Ptr (Sk_maskfilter)))

{- | C function signature:

@
sk_maskfilter_t *sk_maskfilter_new_table(const uint8_t table[256])
@
-}
foreign import ccall "sk_maskfilter_new_table" sk_maskfilter_new_table ::
  Ptr (Word8) -- ^ C argument @"const uint8_t [256] table"@
  -> IO (Ptr (Sk_maskfilter)) -- ^ C return type: @"sk_maskfilter_t *"@

-- | Function pointer to 'sk_maskfilter_new_table'
foreign import ccall "&sk_maskfilter_new_table" p'sk_maskfilter_new_table ::
  FunPtr (Ptr (Word8) -> IO (Ptr (Sk_maskfilter)))

{- | C function signature:

@
sk_maskfilter_t *sk_maskfilter_new_gamma(float gamma)
@
-}
foreign import ccall "sk_maskfilter_new_gamma" sk_maskfilter_new_gamma ::
  CFloat -- ^ C argument @"float gamma"@
  -> IO (Ptr (Sk_maskfilter)) -- ^ C return type: @"sk_maskfilter_t *"@

-- | Function pointer to 'sk_maskfilter_new_gamma'
foreign import ccall "&sk_maskfilter_new_gamma" p'sk_maskfilter_new_gamma ::
  FunPtr (CFloat -> IO (Ptr (Sk_maskfilter)))

{- | C function signature:

@
sk_maskfilter_t *sk_maskfilter_new_clip(uint8_t min, uint8_t max)
@
-}
foreign import ccall "sk_maskfilter_new_clip" sk_maskfilter_new_clip ::
  Word8 -- ^ C argument @"uint8_t min"@
  -> Word8 -- ^ C argument @"uint8_t max"@
  -> IO (Ptr (Sk_maskfilter)) -- ^ C return type: @"sk_maskfilter_t *"@

-- | Function pointer to 'sk_maskfilter_new_clip'
foreign import ccall "&sk_maskfilter_new_clip" p'sk_maskfilter_new_clip ::
  FunPtr (Word8 -> Word8 -> IO (Ptr (Sk_maskfilter)))

{- | C function signature:

@
sk_maskfilter_t *sk_maskfilter_new_shader(sk_shader_t *cshader)
@
-}
foreign import ccall "sk_maskfilter_new_shader" sk_maskfilter_new_shader ::
  Ptr (Sk_shader) -- ^ C argument @"sk_shader_t * cshader"@
  -> IO (Ptr (Sk_maskfilter)) -- ^ C return type: @"sk_maskfilter_t *"@

-- | Function pointer to 'sk_maskfilter_new_shader'
foreign import ccall "&sk_maskfilter_new_shader" p'sk_maskfilter_new_shader ::
  FunPtr (Ptr (Sk_shader) -> IO (Ptr (Sk_maskfilter)))

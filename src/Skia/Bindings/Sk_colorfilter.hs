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
module Skia.Bindings.Sk_colorfilter where

import Foreign
import Foreign.C
import Foreign.Storable
import Foreign.Storable.Offset

import Skia.Bindings.Types


{- | C function signature:

@
void sk_colorfilter_unref(sk_colorfilter_t *filter)
@
-}
foreign import ccall "sk_colorfilter_unref" sk_colorfilter_unref ::
  Ptr (Sk_colorfilter) -- ^ C argument @"sk_colorfilter_t * filter"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_colorfilter_unref'
foreign import ccall "&sk_colorfilter_unref" p'sk_colorfilter_unref ::
  FunPtr (Ptr (Sk_colorfilter) -> IO (()))

{- | C function signature:

@
sk_colorfilter_t *sk_colorfilter_new_mode(sk_color_t c, sk_blendmode_t mode)
@
-}
foreign import ccall "sk_colorfilter_new_mode" sk_colorfilter_new_mode ::
  Sk_color -- ^ C argument @"sk_color_t c"@
  -> Sk_blendmode -- ^ C argument @"sk_blendmode_t mode"@
  -> IO (Ptr (Sk_colorfilter)) -- ^ C return type: @"sk_colorfilter_t *"@

-- | Function pointer to 'sk_colorfilter_new_mode'
foreign import ccall "&sk_colorfilter_new_mode" p'sk_colorfilter_new_mode ::
  FunPtr (Sk_color -> Sk_blendmode -> IO (Ptr (Sk_colorfilter)))

{- | C function signature:

@
sk_colorfilter_t *sk_colorfilter_new_lighting(sk_color_t mul, sk_color_t add)
@
-}
foreign import ccall "sk_colorfilter_new_lighting" sk_colorfilter_new_lighting ::
  Sk_color -- ^ C argument @"sk_color_t mul"@
  -> Sk_color -- ^ C argument @"sk_color_t add"@
  -> IO (Ptr (Sk_colorfilter)) -- ^ C return type: @"sk_colorfilter_t *"@

-- | Function pointer to 'sk_colorfilter_new_lighting'
foreign import ccall "&sk_colorfilter_new_lighting" p'sk_colorfilter_new_lighting ::
  FunPtr (Sk_color -> Sk_color -> IO (Ptr (Sk_colorfilter)))

{- | C function signature:

@
sk_colorfilter_t *sk_colorfilter_new_compose(sk_colorfilter_t *outer, sk_colorfilter_t *inner)
@
-}
foreign import ccall "sk_colorfilter_new_compose" sk_colorfilter_new_compose ::
  Ptr (Sk_colorfilter) -- ^ C argument @"sk_colorfilter_t * outer"@
  -> Ptr (Sk_colorfilter) -- ^ C argument @"sk_colorfilter_t * inner"@
  -> IO (Ptr (Sk_colorfilter)) -- ^ C return type: @"sk_colorfilter_t *"@

-- | Function pointer to 'sk_colorfilter_new_compose'
foreign import ccall "&sk_colorfilter_new_compose" p'sk_colorfilter_new_compose ::
  FunPtr (Ptr (Sk_colorfilter) -> Ptr (Sk_colorfilter) -> IO (Ptr (Sk_colorfilter)))

{- | C function signature:

@
sk_colorfilter_t *sk_colorfilter_new_color_matrix(const float array[20])
@
-}
foreign import ccall "sk_colorfilter_new_color_matrix" sk_colorfilter_new_color_matrix ::
  Ptr (CFloat) -- ^ C argument @"const float [20] array"@
  -> IO (Ptr (Sk_colorfilter)) -- ^ C return type: @"sk_colorfilter_t *"@

-- | Function pointer to 'sk_colorfilter_new_color_matrix'
foreign import ccall "&sk_colorfilter_new_color_matrix" p'sk_colorfilter_new_color_matrix ::
  FunPtr (Ptr (CFloat) -> IO (Ptr (Sk_colorfilter)))

{- | C function signature:

@
sk_colorfilter_t *sk_colorfilter_new_hsla_matrix(const float array[20])
@
-}
foreign import ccall "sk_colorfilter_new_hsla_matrix" sk_colorfilter_new_hsla_matrix ::
  Ptr (CFloat) -- ^ C argument @"const float [20] array"@
  -> IO (Ptr (Sk_colorfilter)) -- ^ C return type: @"sk_colorfilter_t *"@

-- | Function pointer to 'sk_colorfilter_new_hsla_matrix'
foreign import ccall "&sk_colorfilter_new_hsla_matrix" p'sk_colorfilter_new_hsla_matrix ::
  FunPtr (Ptr (CFloat) -> IO (Ptr (Sk_colorfilter)))

{- | C function signature:

@
sk_colorfilter_t *sk_colorfilter_new_linear_to_srgb_gamma(void)
@
-}
foreign import ccall "sk_colorfilter_new_linear_to_srgb_gamma" sk_colorfilter_new_linear_to_srgb_gamma ::
  IO (Ptr (Sk_colorfilter)) -- ^ C return type: @"sk_colorfilter_t *"@

-- | Function pointer to 'sk_colorfilter_new_linear_to_srgb_gamma'
foreign import ccall "&sk_colorfilter_new_linear_to_srgb_gamma" p'sk_colorfilter_new_linear_to_srgb_gamma ::
  FunPtr (IO (Ptr (Sk_colorfilter)))

{- | C function signature:

@
sk_colorfilter_t *sk_colorfilter_new_srgb_to_linear_gamma(void)
@
-}
foreign import ccall "sk_colorfilter_new_srgb_to_linear_gamma" sk_colorfilter_new_srgb_to_linear_gamma ::
  IO (Ptr (Sk_colorfilter)) -- ^ C return type: @"sk_colorfilter_t *"@

-- | Function pointer to 'sk_colorfilter_new_srgb_to_linear_gamma'
foreign import ccall "&sk_colorfilter_new_srgb_to_linear_gamma" p'sk_colorfilter_new_srgb_to_linear_gamma ::
  FunPtr (IO (Ptr (Sk_colorfilter)))

{- | C function signature:

@
sk_colorfilter_t *sk_colorfilter_new_lerp(float weight, sk_colorfilter_t *filter0, sk_colorfilter_t *filter1)
@
-}
foreign import ccall "sk_colorfilter_new_lerp" sk_colorfilter_new_lerp ::
  CFloat -- ^ C argument @"float weight"@
  -> Ptr (Sk_colorfilter) -- ^ C argument @"sk_colorfilter_t * filter0"@
  -> Ptr (Sk_colorfilter) -- ^ C argument @"sk_colorfilter_t * filter1"@
  -> IO (Ptr (Sk_colorfilter)) -- ^ C return type: @"sk_colorfilter_t *"@

-- | Function pointer to 'sk_colorfilter_new_lerp'
foreign import ccall "&sk_colorfilter_new_lerp" p'sk_colorfilter_new_lerp ::
  FunPtr (CFloat -> Ptr (Sk_colorfilter) -> Ptr (Sk_colorfilter) -> IO (Ptr (Sk_colorfilter)))

{- | C function signature:

@
sk_colorfilter_t *sk_colorfilter_new_luma_color(void)
@
-}
foreign import ccall "sk_colorfilter_new_luma_color" sk_colorfilter_new_luma_color ::
  IO (Ptr (Sk_colorfilter)) -- ^ C return type: @"sk_colorfilter_t *"@

-- | Function pointer to 'sk_colorfilter_new_luma_color'
foreign import ccall "&sk_colorfilter_new_luma_color" p'sk_colorfilter_new_luma_color ::
  FunPtr (IO (Ptr (Sk_colorfilter)))

{- | C function signature:

@
sk_colorfilter_t *sk_colorfilter_new_high_contrast(const sk_highcontrastconfig_t *config)
@
-}
foreign import ccall "sk_colorfilter_new_high_contrast" sk_colorfilter_new_high_contrast ::
  Ptr (Sk_highcontrastconfig) -- ^ C argument @"const sk_highcontrastconfig_t * config"@
  -> IO (Ptr (Sk_colorfilter)) -- ^ C return type: @"sk_colorfilter_t *"@

-- | Function pointer to 'sk_colorfilter_new_high_contrast'
foreign import ccall "&sk_colorfilter_new_high_contrast" p'sk_colorfilter_new_high_contrast ::
  FunPtr (Ptr (Sk_highcontrastconfig) -> IO (Ptr (Sk_colorfilter)))

{- | C function signature:

@
sk_colorfilter_t *sk_colorfilter_new_table(const uint8_t table[256])
@
-}
foreign import ccall "sk_colorfilter_new_table" sk_colorfilter_new_table ::
  Ptr (Word8) -- ^ C argument @"const uint8_t [256] table"@
  -> IO (Ptr (Sk_colorfilter)) -- ^ C return type: @"sk_colorfilter_t *"@

-- | Function pointer to 'sk_colorfilter_new_table'
foreign import ccall "&sk_colorfilter_new_table" p'sk_colorfilter_new_table ::
  FunPtr (Ptr (Word8) -> IO (Ptr (Sk_colorfilter)))

{- | C function signature:

@
sk_colorfilter_t *sk_colorfilter_new_table_argb(const uint8_t tableA[256], const uint8_t tableR[256], const uint8_t tableG[256], const uint8_t tableB[256])
@
-}
foreign import ccall "sk_colorfilter_new_table_argb" sk_colorfilter_new_table_argb ::
  Ptr (Word8) -- ^ C argument @"const uint8_t [256] tableA"@
  -> Ptr (Word8) -- ^ C argument @"const uint8_t [256] tableR"@
  -> Ptr (Word8) -- ^ C argument @"const uint8_t [256] tableG"@
  -> Ptr (Word8) -- ^ C argument @"const uint8_t [256] tableB"@
  -> IO (Ptr (Sk_colorfilter)) -- ^ C return type: @"sk_colorfilter_t *"@

-- | Function pointer to 'sk_colorfilter_new_table_argb'
foreign import ccall "&sk_colorfilter_new_table_argb" p'sk_colorfilter_new_table_argb ::
  FunPtr (Ptr (Word8) -> Ptr (Word8) -> Ptr (Word8) -> Ptr (Word8) -> IO (Ptr (Sk_colorfilter)))

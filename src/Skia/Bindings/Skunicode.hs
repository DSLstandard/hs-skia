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
module Skia.Bindings.Skunicode where

import Foreign
import Foreign.C
import Foreign.Storable
import Foreign.Storable.Offset

import Skia.Bindings.Types


{- | C function signature:

@
_Bool skunicode_unicode_is_supported_icu(void)
@
-}
foreign import ccall "skunicode_unicode_is_supported_icu" skunicode_unicode_is_supported_icu ::
  IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'skunicode_unicode_is_supported_icu'
foreign import ccall "&skunicode_unicode_is_supported_icu" p'skunicode_unicode_is_supported_icu ::
  FunPtr (IO (CBool))

{- | C function signature:

@
_Bool skunicode_unicode_is_supported_icu4x(void)
@
-}
foreign import ccall "skunicode_unicode_is_supported_icu4x" skunicode_unicode_is_supported_icu4x ::
  IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'skunicode_unicode_is_supported_icu4x'
foreign import ccall "&skunicode_unicode_is_supported_icu4x" p'skunicode_unicode_is_supported_icu4x ::
  FunPtr (IO (CBool))

{- | C function signature:

@
_Bool skunicode_unicode_is_supported_libgrapheme(void)
@
-}
foreign import ccall "skunicode_unicode_is_supported_libgrapheme" skunicode_unicode_is_supported_libgrapheme ::
  IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'skunicode_unicode_is_supported_libgrapheme'
foreign import ccall "&skunicode_unicode_is_supported_libgrapheme" p'skunicode_unicode_is_supported_libgrapheme ::
  FunPtr (IO (CBool))

{- | C function signature:

@
const skunicode_skunicode_t *skunicode_unicode_new_icu(void)
@
-}
foreign import ccall "skunicode_unicode_new_icu" skunicode_unicode_new_icu ::
  IO (Ptr (Skunicode_skunicode)) -- ^ C return type: @"const skunicode_skunicode_t *"@

-- | Function pointer to 'skunicode_unicode_new_icu'
foreign import ccall "&skunicode_unicode_new_icu" p'skunicode_unicode_new_icu ::
  FunPtr (IO (Ptr (Skunicode_skunicode)))

{- | C function signature:

@
const skunicode_skunicode_t *skunicode_unicode_new_icu4x(void)
@
-}
foreign import ccall "skunicode_unicode_new_icu4x" skunicode_unicode_new_icu4x ::
  IO (Ptr (Skunicode_skunicode)) -- ^ C return type: @"const skunicode_skunicode_t *"@

-- | Function pointer to 'skunicode_unicode_new_icu4x'
foreign import ccall "&skunicode_unicode_new_icu4x" p'skunicode_unicode_new_icu4x ::
  FunPtr (IO (Ptr (Skunicode_skunicode)))

{- | C function signature:

@
const skunicode_skunicode_t *skunicode_unicode_new_libgrapheme(void)
@
-}
foreign import ccall "skunicode_unicode_new_libgrapheme" skunicode_unicode_new_libgrapheme ::
  IO (Ptr (Skunicode_skunicode)) -- ^ C return type: @"const skunicode_skunicode_t *"@

-- | Function pointer to 'skunicode_unicode_new_libgrapheme'
foreign import ccall "&skunicode_unicode_new_libgrapheme" p'skunicode_unicode_new_libgrapheme ::
  FunPtr (IO (Ptr (Skunicode_skunicode)))

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
module Skia.Bindings.Sk_fontmgr where

import Foreign
import Foreign.C
import Foreign.Storable
import Foreign.Storable.Offset

import Skia.Bindings.Types


{- | C function signature:

@
_Bool skfontmgr_is_supported_fontconfig(void)
@
-}
foreign import ccall "skfontmgr_is_supported_fontconfig" skfontmgr_is_supported_fontconfig ::
  IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'skfontmgr_is_supported_fontconfig'
foreign import ccall "&skfontmgr_is_supported_fontconfig" p'skfontmgr_is_supported_fontconfig ::
  FunPtr (IO (CBool))

{- | C function signature:

@
sk_fontmgr_t *skfontmgr_new_fontconfig(void)
@
-}
foreign import ccall "skfontmgr_new_fontconfig" skfontmgr_new_fontconfig ::
  IO (Ptr (Sk_fontmgr)) -- ^ C return type: @"sk_fontmgr_t *"@

-- | Function pointer to 'skfontmgr_new_fontconfig'
foreign import ccall "&skfontmgr_new_fontconfig" p'skfontmgr_new_fontconfig ::
  FunPtr (IO (Ptr (Sk_fontmgr)))

{- | C function signature:

@
_Bool skfontmgr_is_supported_gdi(void)
@
-}
foreign import ccall "skfontmgr_is_supported_gdi" skfontmgr_is_supported_gdi ::
  IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'skfontmgr_is_supported_gdi'
foreign import ccall "&skfontmgr_is_supported_gdi" p'skfontmgr_is_supported_gdi ::
  FunPtr (IO (CBool))

{- | C function signature:

@
sk_fontmgr_t *skfontmgr_new_gdi(void)
@
-}
foreign import ccall "skfontmgr_new_gdi" skfontmgr_new_gdi ::
  IO (Ptr (Sk_fontmgr)) -- ^ C return type: @"sk_fontmgr_t *"@

-- | Function pointer to 'skfontmgr_new_gdi'
foreign import ccall "&skfontmgr_new_gdi" p'skfontmgr_new_gdi ::
  FunPtr (IO (Ptr (Sk_fontmgr)))

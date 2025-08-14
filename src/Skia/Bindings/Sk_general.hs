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
module Skia.Bindings.Sk_general where

import Foreign
import Foreign.C
import Foreign.Storable
import Foreign.Storable.Offset

import Skia.Bindings.Types


{- | C function signature:

@
_Bool sk_refcnt_unique(const sk_refcnt_t *refcnt)
@
-}
foreign import ccall "sk_refcnt_unique" sk_refcnt_unique ::
  Ptr (Sk_refcnt) -- ^ C argument @"const sk_refcnt_t * refcnt"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_refcnt_unique'
foreign import ccall "&sk_refcnt_unique" p'sk_refcnt_unique ::
  FunPtr (Ptr (Sk_refcnt) -> IO (CBool))

{- | C function signature:

@
void sk_refcnt_safe_ref(sk_refcnt_t *refcnt)
@
-}
foreign import ccall "sk_refcnt_safe_ref" sk_refcnt_safe_ref ::
  Ptr (Sk_refcnt) -- ^ C argument @"sk_refcnt_t * refcnt"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_refcnt_safe_ref'
foreign import ccall "&sk_refcnt_safe_ref" p'sk_refcnt_safe_ref ::
  FunPtr (Ptr (Sk_refcnt) -> IO (()))

{- | C function signature:

@
void sk_refcnt_safe_unref(sk_refcnt_t *refcnt)
@
-}
foreign import ccall "sk_refcnt_safe_unref" sk_refcnt_safe_unref ::
  Ptr (Sk_refcnt) -- ^ C argument @"sk_refcnt_t * refcnt"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_refcnt_safe_unref'
foreign import ccall "&sk_refcnt_safe_unref" p'sk_refcnt_safe_unref ::
  FunPtr (Ptr (Sk_refcnt) -> IO (()))

{- | C function signature:

@
_Bool sk_nvrefcnt_unique(const sk_nvrefcnt_t *refcnt)
@
-}
foreign import ccall "sk_nvrefcnt_unique" sk_nvrefcnt_unique ::
  Ptr (Sk_nvrefcnt) -- ^ C argument @"const sk_nvrefcnt_t * refcnt"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_nvrefcnt_unique'
foreign import ccall "&sk_nvrefcnt_unique" p'sk_nvrefcnt_unique ::
  FunPtr (Ptr (Sk_nvrefcnt) -> IO (CBool))

{- | C function signature:

@
void sk_nvrefcnt_safe_ref(sk_nvrefcnt_t *refcnt)
@
-}
foreign import ccall "sk_nvrefcnt_safe_ref" sk_nvrefcnt_safe_ref ::
  Ptr (Sk_nvrefcnt) -- ^ C argument @"sk_nvrefcnt_t * refcnt"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_nvrefcnt_safe_ref'
foreign import ccall "&sk_nvrefcnt_safe_ref" p'sk_nvrefcnt_safe_ref ::
  FunPtr (Ptr (Sk_nvrefcnt) -> IO (()))

{- | C function signature:

@
void sk_nvrefcnt_safe_unref(sk_nvrefcnt_t *refcnt)
@
-}
foreign import ccall "sk_nvrefcnt_safe_unref" sk_nvrefcnt_safe_unref ::
  Ptr (Sk_nvrefcnt) -- ^ C argument @"sk_nvrefcnt_t * refcnt"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_nvrefcnt_safe_unref'
foreign import ccall "&sk_nvrefcnt_safe_unref" p'sk_nvrefcnt_safe_unref ::
  FunPtr (Ptr (Sk_nvrefcnt) -> IO (()))

{- | C function signature:

@
sk_colortype_t sk_colortype_get_default_8888(void)
@
-}
foreign import ccall "sk_colortype_get_default_8888" sk_colortype_get_default_8888 ::
  IO (Sk_colortype) -- ^ C return type: @"sk_colortype_t"@

-- | Function pointer to 'sk_colortype_get_default_8888'
foreign import ccall "&sk_colortype_get_default_8888" p'sk_colortype_get_default_8888 ::
  FunPtr (IO (Sk_colortype))

{- | C function signature:

@
int sk_version_get_milestone(void)
@
-}
foreign import ccall "sk_version_get_milestone" sk_version_get_milestone ::
  IO (CInt) -- ^ C return type: @"int"@

-- | Function pointer to 'sk_version_get_milestone'
foreign import ccall "&sk_version_get_milestone" p'sk_version_get_milestone ::
  FunPtr (IO (CInt))

{- | C function signature:

@
int sk_version_get_increment(void)
@
-}
foreign import ccall "sk_version_get_increment" sk_version_get_increment ::
  IO (CInt) -- ^ C return type: @"int"@

-- | Function pointer to 'sk_version_get_increment'
foreign import ccall "&sk_version_get_increment" p'sk_version_get_increment ::
  FunPtr (IO (CInt))

{- | C function signature:

@
const char *sk_version_get_string(void)
@
-}
foreign import ccall "sk_version_get_string" sk_version_get_string ::
  IO (Ptr (CChar)) -- ^ C return type: @"const char *"@

-- | Function pointer to 'sk_version_get_string'
foreign import ccall "&sk_version_get_string" p'sk_version_get_string ::
  FunPtr (IO (Ptr (CChar)))

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
module Skia.Bindings.Sk_string where

import Foreign
import Foreign.C
import Foreign.Storable
import Foreign.Storable.Offset

import Skia.Bindings.Types


{- | C function signature:

@
sk_string_t *sk_string_new_empty(void)
@
-}
foreign import ccall "sk_string_new_empty" sk_string_new_empty ::
  IO (Ptr (Sk_string)) -- ^ C return type: @"sk_string_t *"@

-- | Function pointer to 'sk_string_new_empty'
foreign import ccall "&sk_string_new_empty" p'sk_string_new_empty ::
  FunPtr (IO (Ptr (Sk_string)))

{- | C function signature:

@
sk_string_t *sk_string_new_with_copy(const char *src, size_t length)
@
-}
foreign import ccall "sk_string_new_with_copy" sk_string_new_with_copy ::
  Ptr (CChar) -- ^ C argument @"const char * src"@
  -> CSize -- ^ C argument @"size_t length"@
  -> IO (Ptr (Sk_string)) -- ^ C return type: @"sk_string_t *"@

-- | Function pointer to 'sk_string_new_with_copy'
foreign import ccall "&sk_string_new_with_copy" p'sk_string_new_with_copy ::
  FunPtr (Ptr (CChar) -> CSize -> IO (Ptr (Sk_string)))

{- | C function signature:

@
void sk_string_destructor(const sk_string_t *)
@
-}
foreign import ccall "sk_string_destructor" sk_string_destructor ::
  Ptr (Sk_string) -- ^ C argument type: @"const sk_string_t *"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_string_destructor'
foreign import ccall "&sk_string_destructor" p'sk_string_destructor ::
  FunPtr (Ptr (Sk_string) -> IO (()))

{- | C function signature:

@
size_t sk_string_get_size(const sk_string_t *)
@
-}
foreign import ccall "sk_string_get_size" sk_string_get_size ::
  Ptr (Sk_string) -- ^ C argument type: @"const sk_string_t *"@
  -> IO (CSize) -- ^ C return type: @"size_t"@

-- | Function pointer to 'sk_string_get_size'
foreign import ccall "&sk_string_get_size" p'sk_string_get_size ::
  FunPtr (Ptr (Sk_string) -> IO (CSize))

{- | C function signature:

@
const char *sk_string_get_c_str(const sk_string_t *)
@
-}
foreign import ccall "sk_string_get_c_str" sk_string_get_c_str ::
  Ptr (Sk_string) -- ^ C argument type: @"const sk_string_t *"@
  -> IO (Ptr (CChar)) -- ^ C return type: @"const char *"@

-- | Function pointer to 'sk_string_get_c_str'
foreign import ccall "&sk_string_get_c_str" p'sk_string_get_c_str ::
  FunPtr (Ptr (Sk_string) -> IO (Ptr (CChar)))

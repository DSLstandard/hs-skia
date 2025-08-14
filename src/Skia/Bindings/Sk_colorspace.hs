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
module Skia.Bindings.Sk_colorspace where

import Foreign
import Foreign.C
import Foreign.Storable
import Foreign.Storable.Offset

import Skia.Bindings.Types


{- | C function signature:

@
void sk_colorspace_ref(sk_colorspace_t *colorspace)
@
-}
foreign import ccall "sk_colorspace_ref" sk_colorspace_ref ::
  Ptr (Sk_colorspace) -- ^ C argument @"sk_colorspace_t * colorspace"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_colorspace_ref'
foreign import ccall "&sk_colorspace_ref" p'sk_colorspace_ref ::
  FunPtr (Ptr (Sk_colorspace) -> IO (()))

{- | C function signature:

@
void sk_colorspace_unref(sk_colorspace_t *colorspace)
@
-}
foreign import ccall "sk_colorspace_unref" sk_colorspace_unref ::
  Ptr (Sk_colorspace) -- ^ C argument @"sk_colorspace_t * colorspace"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_colorspace_unref'
foreign import ccall "&sk_colorspace_unref" p'sk_colorspace_unref ::
  FunPtr (Ptr (Sk_colorspace) -> IO (()))

{- | C function signature:

@
sk_colorspace_t *sk_colorspace_new_srgb(void)
@
-}
foreign import ccall "sk_colorspace_new_srgb" sk_colorspace_new_srgb ::
  IO (Ptr (Sk_colorspace)) -- ^ C return type: @"sk_colorspace_t *"@

-- | Function pointer to 'sk_colorspace_new_srgb'
foreign import ccall "&sk_colorspace_new_srgb" p'sk_colorspace_new_srgb ::
  FunPtr (IO (Ptr (Sk_colorspace)))

{- | C function signature:

@
sk_colorspace_t *sk_colorspace_new_srgb_linear(void)
@
-}
foreign import ccall "sk_colorspace_new_srgb_linear" sk_colorspace_new_srgb_linear ::
  IO (Ptr (Sk_colorspace)) -- ^ C return type: @"sk_colorspace_t *"@

-- | Function pointer to 'sk_colorspace_new_srgb_linear'
foreign import ccall "&sk_colorspace_new_srgb_linear" p'sk_colorspace_new_srgb_linear ::
  FunPtr (IO (Ptr (Sk_colorspace)))

{- | C function signature:

@
sk_colorspace_t *sk_colorspace_new_rgb(const sk_colorspace_transfer_fn_t *transferFn, const sk_colorspace_xyz_t *toXYZD50)
@
-}
foreign import ccall "sk_colorspace_new_rgb" sk_colorspace_new_rgb ::
  Ptr (Sk_colorspace_transfer_fn) -- ^ C argument @"const sk_colorspace_transfer_fn_t * transferFn"@
  -> Ptr (Sk_colorspace_xyz) -- ^ C argument @"const sk_colorspace_xyz_t * toXYZD50"@
  -> IO (Ptr (Sk_colorspace)) -- ^ C return type: @"sk_colorspace_t *"@

-- | Function pointer to 'sk_colorspace_new_rgb'
foreign import ccall "&sk_colorspace_new_rgb" p'sk_colorspace_new_rgb ::
  FunPtr (Ptr (Sk_colorspace_transfer_fn) -> Ptr (Sk_colorspace_xyz) -> IO (Ptr (Sk_colorspace)))

{- | C function signature:

@
sk_colorspace_t *sk_colorspace_new_icc(const sk_colorspace_icc_profile_t *profile)
@
-}
foreign import ccall "sk_colorspace_new_icc" sk_colorspace_new_icc ::
  Ptr (Sk_colorspace_icc_profile) -- ^ C argument @"const sk_colorspace_icc_profile_t * profile"@
  -> IO (Ptr (Sk_colorspace)) -- ^ C return type: @"sk_colorspace_t *"@

-- | Function pointer to 'sk_colorspace_new_icc'
foreign import ccall "&sk_colorspace_new_icc" p'sk_colorspace_new_icc ::
  FunPtr (Ptr (Sk_colorspace_icc_profile) -> IO (Ptr (Sk_colorspace)))

{- | C function signature:

@
void sk_colorspace_to_profile(const sk_colorspace_t *colorspace, sk_colorspace_icc_profile_t *profile)
@
-}
foreign import ccall "sk_colorspace_to_profile" sk_colorspace_to_profile ::
  Ptr (Sk_colorspace) -- ^ C argument @"const sk_colorspace_t * colorspace"@
  -> Ptr (Sk_colorspace_icc_profile) -- ^ C argument @"sk_colorspace_icc_profile_t * profile"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_colorspace_to_profile'
foreign import ccall "&sk_colorspace_to_profile" p'sk_colorspace_to_profile ::
  FunPtr (Ptr (Sk_colorspace) -> Ptr (Sk_colorspace_icc_profile) -> IO (()))

{- | C function signature:

@
_Bool sk_colorspace_gamma_close_to_srgb(const sk_colorspace_t *colorspace)
@
-}
foreign import ccall "sk_colorspace_gamma_close_to_srgb" sk_colorspace_gamma_close_to_srgb ::
  Ptr (Sk_colorspace) -- ^ C argument @"const sk_colorspace_t * colorspace"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_colorspace_gamma_close_to_srgb'
foreign import ccall "&sk_colorspace_gamma_close_to_srgb" p'sk_colorspace_gamma_close_to_srgb ::
  FunPtr (Ptr (Sk_colorspace) -> IO (CBool))

{- | C function signature:

@
_Bool sk_colorspace_gamma_is_linear(const sk_colorspace_t *colorspace)
@
-}
foreign import ccall "sk_colorspace_gamma_is_linear" sk_colorspace_gamma_is_linear ::
  Ptr (Sk_colorspace) -- ^ C argument @"const sk_colorspace_t * colorspace"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_colorspace_gamma_is_linear'
foreign import ccall "&sk_colorspace_gamma_is_linear" p'sk_colorspace_gamma_is_linear ::
  FunPtr (Ptr (Sk_colorspace) -> IO (CBool))

{- | C function signature:

@
_Bool sk_colorspace_is_numerical_transfer_fn(const sk_colorspace_t *colorspace, sk_colorspace_transfer_fn_t *transferFn)
@
-}
foreign import ccall "sk_colorspace_is_numerical_transfer_fn" sk_colorspace_is_numerical_transfer_fn ::
  Ptr (Sk_colorspace) -- ^ C argument @"const sk_colorspace_t * colorspace"@
  -> Ptr (Sk_colorspace_transfer_fn) -- ^ C argument @"sk_colorspace_transfer_fn_t * transferFn"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_colorspace_is_numerical_transfer_fn'
foreign import ccall "&sk_colorspace_is_numerical_transfer_fn" p'sk_colorspace_is_numerical_transfer_fn ::
  FunPtr (Ptr (Sk_colorspace) -> Ptr (Sk_colorspace_transfer_fn) -> IO (CBool))

{- | C function signature:

@
_Bool sk_colorspace_to_xyzd50(const sk_colorspace_t *colorspace, sk_colorspace_xyz_t *toXYZD50)
@
-}
foreign import ccall "sk_colorspace_to_xyzd50" sk_colorspace_to_xyzd50 ::
  Ptr (Sk_colorspace) -- ^ C argument @"const sk_colorspace_t * colorspace"@
  -> Ptr (Sk_colorspace_xyz) -- ^ C argument @"sk_colorspace_xyz_t * toXYZD50"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_colorspace_to_xyzd50'
foreign import ccall "&sk_colorspace_to_xyzd50" p'sk_colorspace_to_xyzd50 ::
  FunPtr (Ptr (Sk_colorspace) -> Ptr (Sk_colorspace_xyz) -> IO (CBool))

{- | C function signature:

@
sk_colorspace_t *sk_colorspace_make_linear_gamma(const sk_colorspace_t *colorspace)
@
-}
foreign import ccall "sk_colorspace_make_linear_gamma" sk_colorspace_make_linear_gamma ::
  Ptr (Sk_colorspace) -- ^ C argument @"const sk_colorspace_t * colorspace"@
  -> IO (Ptr (Sk_colorspace)) -- ^ C return type: @"sk_colorspace_t *"@

-- | Function pointer to 'sk_colorspace_make_linear_gamma'
foreign import ccall "&sk_colorspace_make_linear_gamma" p'sk_colorspace_make_linear_gamma ::
  FunPtr (Ptr (Sk_colorspace) -> IO (Ptr (Sk_colorspace)))

{- | C function signature:

@
sk_colorspace_t *sk_colorspace_make_srgb_gamma(const sk_colorspace_t *colorspace)
@
-}
foreign import ccall "sk_colorspace_make_srgb_gamma" sk_colorspace_make_srgb_gamma ::
  Ptr (Sk_colorspace) -- ^ C argument @"const sk_colorspace_t * colorspace"@
  -> IO (Ptr (Sk_colorspace)) -- ^ C return type: @"sk_colorspace_t *"@

-- | Function pointer to 'sk_colorspace_make_srgb_gamma'
foreign import ccall "&sk_colorspace_make_srgb_gamma" p'sk_colorspace_make_srgb_gamma ::
  FunPtr (Ptr (Sk_colorspace) -> IO (Ptr (Sk_colorspace)))

{- | C function signature:

@
_Bool sk_colorspace_is_srgb(const sk_colorspace_t *colorspace)
@
-}
foreign import ccall "sk_colorspace_is_srgb" sk_colorspace_is_srgb ::
  Ptr (Sk_colorspace) -- ^ C argument @"const sk_colorspace_t * colorspace"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_colorspace_is_srgb'
foreign import ccall "&sk_colorspace_is_srgb" p'sk_colorspace_is_srgb ::
  FunPtr (Ptr (Sk_colorspace) -> IO (CBool))

{- | C function signature:

@
_Bool sk_colorspace_equals(const sk_colorspace_t *src, const sk_colorspace_t *dst)
@
-}
foreign import ccall "sk_colorspace_equals" sk_colorspace_equals ::
  Ptr (Sk_colorspace) -- ^ C argument @"const sk_colorspace_t * src"@
  -> Ptr (Sk_colorspace) -- ^ C argument @"const sk_colorspace_t * dst"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_colorspace_equals'
foreign import ccall "&sk_colorspace_equals" p'sk_colorspace_equals ::
  FunPtr (Ptr (Sk_colorspace) -> Ptr (Sk_colorspace) -> IO (CBool))

{- | C function signature:

@
void sk_colorspace_transfer_fn_named_srgb(sk_colorspace_transfer_fn_t *transferFn)
@
-}
foreign import ccall "sk_colorspace_transfer_fn_named_srgb" sk_colorspace_transfer_fn_named_srgb ::
  Ptr (Sk_colorspace_transfer_fn) -- ^ C argument @"sk_colorspace_transfer_fn_t * transferFn"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_colorspace_transfer_fn_named_srgb'
foreign import ccall "&sk_colorspace_transfer_fn_named_srgb" p'sk_colorspace_transfer_fn_named_srgb ::
  FunPtr (Ptr (Sk_colorspace_transfer_fn) -> IO (()))

{- | C function signature:

@
void sk_colorspace_transfer_fn_named_2dot2(sk_colorspace_transfer_fn_t *transferFn)
@
-}
foreign import ccall "sk_colorspace_transfer_fn_named_2dot2" sk_colorspace_transfer_fn_named_2dot2 ::
  Ptr (Sk_colorspace_transfer_fn) -- ^ C argument @"sk_colorspace_transfer_fn_t * transferFn"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_colorspace_transfer_fn_named_2dot2'
foreign import ccall "&sk_colorspace_transfer_fn_named_2dot2" p'sk_colorspace_transfer_fn_named_2dot2 ::
  FunPtr (Ptr (Sk_colorspace_transfer_fn) -> IO (()))

{- | C function signature:

@
void sk_colorspace_transfer_fn_named_linear(sk_colorspace_transfer_fn_t *transferFn)
@
-}
foreign import ccall "sk_colorspace_transfer_fn_named_linear" sk_colorspace_transfer_fn_named_linear ::
  Ptr (Sk_colorspace_transfer_fn) -- ^ C argument @"sk_colorspace_transfer_fn_t * transferFn"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_colorspace_transfer_fn_named_linear'
foreign import ccall "&sk_colorspace_transfer_fn_named_linear" p'sk_colorspace_transfer_fn_named_linear ::
  FunPtr (Ptr (Sk_colorspace_transfer_fn) -> IO (()))

{- | C function signature:

@
void sk_colorspace_transfer_fn_named_rec2020(sk_colorspace_transfer_fn_t *transferFn)
@
-}
foreign import ccall "sk_colorspace_transfer_fn_named_rec2020" sk_colorspace_transfer_fn_named_rec2020 ::
  Ptr (Sk_colorspace_transfer_fn) -- ^ C argument @"sk_colorspace_transfer_fn_t * transferFn"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_colorspace_transfer_fn_named_rec2020'
foreign import ccall "&sk_colorspace_transfer_fn_named_rec2020" p'sk_colorspace_transfer_fn_named_rec2020 ::
  FunPtr (Ptr (Sk_colorspace_transfer_fn) -> IO (()))

{- | C function signature:

@
void sk_colorspace_transfer_fn_named_pq(sk_colorspace_transfer_fn_t *transferFn)
@
-}
foreign import ccall "sk_colorspace_transfer_fn_named_pq" sk_colorspace_transfer_fn_named_pq ::
  Ptr (Sk_colorspace_transfer_fn) -- ^ C argument @"sk_colorspace_transfer_fn_t * transferFn"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_colorspace_transfer_fn_named_pq'
foreign import ccall "&sk_colorspace_transfer_fn_named_pq" p'sk_colorspace_transfer_fn_named_pq ::
  FunPtr (Ptr (Sk_colorspace_transfer_fn) -> IO (()))

{- | C function signature:

@
void sk_colorspace_transfer_fn_named_hlg(sk_colorspace_transfer_fn_t *transferFn)
@
-}
foreign import ccall "sk_colorspace_transfer_fn_named_hlg" sk_colorspace_transfer_fn_named_hlg ::
  Ptr (Sk_colorspace_transfer_fn) -- ^ C argument @"sk_colorspace_transfer_fn_t * transferFn"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_colorspace_transfer_fn_named_hlg'
foreign import ccall "&sk_colorspace_transfer_fn_named_hlg" p'sk_colorspace_transfer_fn_named_hlg ::
  FunPtr (Ptr (Sk_colorspace_transfer_fn) -> IO (()))

{- | C function signature:

@
float sk_colorspace_transfer_fn_eval(const sk_colorspace_transfer_fn_t *transferFn, float x)
@
-}
foreign import ccall "sk_colorspace_transfer_fn_eval" sk_colorspace_transfer_fn_eval ::
  Ptr (Sk_colorspace_transfer_fn) -- ^ C argument @"const sk_colorspace_transfer_fn_t * transferFn"@
  -> CFloat -- ^ C argument @"float x"@
  -> IO (CFloat) -- ^ C return type: @"float"@

-- | Function pointer to 'sk_colorspace_transfer_fn_eval'
foreign import ccall "&sk_colorspace_transfer_fn_eval" p'sk_colorspace_transfer_fn_eval ::
  FunPtr (Ptr (Sk_colorspace_transfer_fn) -> CFloat -> IO (CFloat))

{- | C function signature:

@
_Bool sk_colorspace_transfer_fn_invert(const sk_colorspace_transfer_fn_t *src, sk_colorspace_transfer_fn_t *dst)
@
-}
foreign import ccall "sk_colorspace_transfer_fn_invert" sk_colorspace_transfer_fn_invert ::
  Ptr (Sk_colorspace_transfer_fn) -- ^ C argument @"const sk_colorspace_transfer_fn_t * src"@
  -> Ptr (Sk_colorspace_transfer_fn) -- ^ C argument @"sk_colorspace_transfer_fn_t * dst"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_colorspace_transfer_fn_invert'
foreign import ccall "&sk_colorspace_transfer_fn_invert" p'sk_colorspace_transfer_fn_invert ::
  FunPtr (Ptr (Sk_colorspace_transfer_fn) -> Ptr (Sk_colorspace_transfer_fn) -> IO (CBool))

{- | C function signature:

@
_Bool sk_colorspace_primaries_to_xyzd50(const sk_colorspace_primaries_t *primaries, sk_colorspace_xyz_t *toXYZD50)
@
-}
foreign import ccall "sk_colorspace_primaries_to_xyzd50" sk_colorspace_primaries_to_xyzd50 ::
  Ptr (Sk_colorspace_primaries) -- ^ C argument @"const sk_colorspace_primaries_t * primaries"@
  -> Ptr (Sk_colorspace_xyz) -- ^ C argument @"sk_colorspace_xyz_t * toXYZD50"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_colorspace_primaries_to_xyzd50'
foreign import ccall "&sk_colorspace_primaries_to_xyzd50" p'sk_colorspace_primaries_to_xyzd50 ::
  FunPtr (Ptr (Sk_colorspace_primaries) -> Ptr (Sk_colorspace_xyz) -> IO (CBool))

{- | C function signature:

@
void sk_colorspace_xyz_named_srgb(sk_colorspace_xyz_t *xyz)
@
-}
foreign import ccall "sk_colorspace_xyz_named_srgb" sk_colorspace_xyz_named_srgb ::
  Ptr (Sk_colorspace_xyz) -- ^ C argument @"sk_colorspace_xyz_t * xyz"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_colorspace_xyz_named_srgb'
foreign import ccall "&sk_colorspace_xyz_named_srgb" p'sk_colorspace_xyz_named_srgb ::
  FunPtr (Ptr (Sk_colorspace_xyz) -> IO (()))

{- | C function signature:

@
void sk_colorspace_xyz_named_adobe_rgb(sk_colorspace_xyz_t *xyz)
@
-}
foreign import ccall "sk_colorspace_xyz_named_adobe_rgb" sk_colorspace_xyz_named_adobe_rgb ::
  Ptr (Sk_colorspace_xyz) -- ^ C argument @"sk_colorspace_xyz_t * xyz"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_colorspace_xyz_named_adobe_rgb'
foreign import ccall "&sk_colorspace_xyz_named_adobe_rgb" p'sk_colorspace_xyz_named_adobe_rgb ::
  FunPtr (Ptr (Sk_colorspace_xyz) -> IO (()))

{- | C function signature:

@
void sk_colorspace_xyz_named_display_p3(sk_colorspace_xyz_t *xyz)
@
-}
foreign import ccall "sk_colorspace_xyz_named_display_p3" sk_colorspace_xyz_named_display_p3 ::
  Ptr (Sk_colorspace_xyz) -- ^ C argument @"sk_colorspace_xyz_t * xyz"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_colorspace_xyz_named_display_p3'
foreign import ccall "&sk_colorspace_xyz_named_display_p3" p'sk_colorspace_xyz_named_display_p3 ::
  FunPtr (Ptr (Sk_colorspace_xyz) -> IO (()))

{- | C function signature:

@
void sk_colorspace_xyz_named_rec2020(sk_colorspace_xyz_t *xyz)
@
-}
foreign import ccall "sk_colorspace_xyz_named_rec2020" sk_colorspace_xyz_named_rec2020 ::
  Ptr (Sk_colorspace_xyz) -- ^ C argument @"sk_colorspace_xyz_t * xyz"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_colorspace_xyz_named_rec2020'
foreign import ccall "&sk_colorspace_xyz_named_rec2020" p'sk_colorspace_xyz_named_rec2020 ::
  FunPtr (Ptr (Sk_colorspace_xyz) -> IO (()))

{- | C function signature:

@
void sk_colorspace_xyz_named_xyz(sk_colorspace_xyz_t *xyz)
@
-}
foreign import ccall "sk_colorspace_xyz_named_xyz" sk_colorspace_xyz_named_xyz ::
  Ptr (Sk_colorspace_xyz) -- ^ C argument @"sk_colorspace_xyz_t * xyz"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_colorspace_xyz_named_xyz'
foreign import ccall "&sk_colorspace_xyz_named_xyz" p'sk_colorspace_xyz_named_xyz ::
  FunPtr (Ptr (Sk_colorspace_xyz) -> IO (()))

{- | C function signature:

@
_Bool sk_colorspace_xyz_invert(const sk_colorspace_xyz_t *src, sk_colorspace_xyz_t *dst)
@
-}
foreign import ccall "sk_colorspace_xyz_invert" sk_colorspace_xyz_invert ::
  Ptr (Sk_colorspace_xyz) -- ^ C argument @"const sk_colorspace_xyz_t * src"@
  -> Ptr (Sk_colorspace_xyz) -- ^ C argument @"sk_colorspace_xyz_t * dst"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_colorspace_xyz_invert'
foreign import ccall "&sk_colorspace_xyz_invert" p'sk_colorspace_xyz_invert ::
  FunPtr (Ptr (Sk_colorspace_xyz) -> Ptr (Sk_colorspace_xyz) -> IO (CBool))

{- | C function signature:

@
void sk_colorspace_xyz_concat(const sk_colorspace_xyz_t *a, const sk_colorspace_xyz_t *b, sk_colorspace_xyz_t *result)
@
-}
foreign import ccall "sk_colorspace_xyz_concat" sk_colorspace_xyz_concat ::
  Ptr (Sk_colorspace_xyz) -- ^ C argument @"const sk_colorspace_xyz_t * a"@
  -> Ptr (Sk_colorspace_xyz) -- ^ C argument @"const sk_colorspace_xyz_t * b"@
  -> Ptr (Sk_colorspace_xyz) -- ^ C argument @"sk_colorspace_xyz_t * result"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_colorspace_xyz_concat'
foreign import ccall "&sk_colorspace_xyz_concat" p'sk_colorspace_xyz_concat ::
  FunPtr (Ptr (Sk_colorspace_xyz) -> Ptr (Sk_colorspace_xyz) -> Ptr (Sk_colorspace_xyz) -> IO (()))

{- | C function signature:

@
void sk_colorspace_icc_profile_delete(sk_colorspace_icc_profile_t *profile)
@
-}
foreign import ccall "sk_colorspace_icc_profile_delete" sk_colorspace_icc_profile_delete ::
  Ptr (Sk_colorspace_icc_profile) -- ^ C argument @"sk_colorspace_icc_profile_t * profile"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_colorspace_icc_profile_delete'
foreign import ccall "&sk_colorspace_icc_profile_delete" p'sk_colorspace_icc_profile_delete ::
  FunPtr (Ptr (Sk_colorspace_icc_profile) -> IO (()))

{- | C function signature:

@
sk_colorspace_icc_profile_t *sk_colorspace_icc_profile_new(void)
@
-}
foreign import ccall "sk_colorspace_icc_profile_new" sk_colorspace_icc_profile_new ::
  IO (Ptr (Sk_colorspace_icc_profile)) -- ^ C return type: @"sk_colorspace_icc_profile_t *"@

-- | Function pointer to 'sk_colorspace_icc_profile_new'
foreign import ccall "&sk_colorspace_icc_profile_new" p'sk_colorspace_icc_profile_new ::
  FunPtr (IO (Ptr (Sk_colorspace_icc_profile)))

{- | C function signature:

@
_Bool sk_colorspace_icc_profile_parse(const void *buffer, size_t length, sk_colorspace_icc_profile_t *profile)
@
-}
foreign import ccall "sk_colorspace_icc_profile_parse" sk_colorspace_icc_profile_parse ::
  Ptr (()) -- ^ C argument @"const void * buffer"@
  -> CSize -- ^ C argument @"size_t length"@
  -> Ptr (Sk_colorspace_icc_profile) -- ^ C argument @"sk_colorspace_icc_profile_t * profile"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_colorspace_icc_profile_parse'
foreign import ccall "&sk_colorspace_icc_profile_parse" p'sk_colorspace_icc_profile_parse ::
  FunPtr (Ptr (()) -> CSize -> Ptr (Sk_colorspace_icc_profile) -> IO (CBool))

{- | C function signature:

@
const uint8_t *sk_colorspace_icc_profile_get_buffer(const sk_colorspace_icc_profile_t *profile, uint32_t *size)
@
-}
foreign import ccall "sk_colorspace_icc_profile_get_buffer" sk_colorspace_icc_profile_get_buffer ::
  Ptr (Sk_colorspace_icc_profile) -- ^ C argument @"const sk_colorspace_icc_profile_t * profile"@
  -> Ptr (Word32) -- ^ C argument @"uint32_t * size"@
  -> IO (Ptr (Word8)) -- ^ C return type: @"const uint8_t *"@

-- | Function pointer to 'sk_colorspace_icc_profile_get_buffer'
foreign import ccall "&sk_colorspace_icc_profile_get_buffer" p'sk_colorspace_icc_profile_get_buffer ::
  FunPtr (Ptr (Sk_colorspace_icc_profile) -> Ptr (Word32) -> IO (Ptr (Word8)))

{- | C function signature:

@
_Bool sk_colorspace_icc_profile_get_to_xyzd50(const sk_colorspace_icc_profile_t *profile, sk_colorspace_xyz_t *toXYZD50)
@
-}
foreign import ccall "sk_colorspace_icc_profile_get_to_xyzd50" sk_colorspace_icc_profile_get_to_xyzd50 ::
  Ptr (Sk_colorspace_icc_profile) -- ^ C argument @"const sk_colorspace_icc_profile_t * profile"@
  -> Ptr (Sk_colorspace_xyz) -- ^ C argument @"sk_colorspace_xyz_t * toXYZD50"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_colorspace_icc_profile_get_to_xyzd50'
foreign import ccall "&sk_colorspace_icc_profile_get_to_xyzd50" p'sk_colorspace_icc_profile_get_to_xyzd50 ::
  FunPtr (Ptr (Sk_colorspace_icc_profile) -> Ptr (Sk_colorspace_xyz) -> IO (CBool))

{- | C function signature:

@
sk_color_t sk_color4f_to_color(const sk_color4f_t *color4f)
@
-}
foreign import ccall "sk_color4f_to_color" sk_color4f_to_color ::
  Ptr (Sk_color4f) -- ^ C argument @"const sk_color4f_t * color4f"@
  -> IO (Sk_color) -- ^ C return type: @"sk_color_t"@

-- | Function pointer to 'sk_color4f_to_color'
foreign import ccall "&sk_color4f_to_color" p'sk_color4f_to_color ::
  FunPtr (Ptr (Sk_color4f) -> IO (Sk_color))

{- | C function signature:

@
void sk_color4f_from_color(sk_color_t color, sk_color4f_t *color4f)
@
-}
foreign import ccall "sk_color4f_from_color" sk_color4f_from_color ::
  Sk_color -- ^ C argument @"sk_color_t color"@
  -> Ptr (Sk_color4f) -- ^ C argument @"sk_color4f_t * color4f"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_color4f_from_color'
foreign import ccall "&sk_color4f_from_color" p'sk_color4f_from_color ::
  FunPtr (Sk_color -> Ptr (Sk_color4f) -> IO (()))

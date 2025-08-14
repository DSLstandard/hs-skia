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
module Skia.Bindings.Sk_runtimeeffect where

import Foreign
import Foreign.C
import Foreign.Storable
import Foreign.Storable.Offset

import Skia.Bindings.Types


{- | C function signature:

@
sk_runtimeeffect_t *sk_runtimeeffect_make_for_color_filter(sk_string_t *sksl, sk_string_t *error)
@
-}
foreign import ccall "sk_runtimeeffect_make_for_color_filter" sk_runtimeeffect_make_for_color_filter ::
  Ptr (Sk_string) -- ^ C argument @"sk_string_t * sksl"@
  -> Ptr (Sk_string) -- ^ C argument @"sk_string_t * error"@
  -> IO (Ptr (Sk_runtimeeffect)) -- ^ C return type: @"sk_runtimeeffect_t *"@

-- | Function pointer to 'sk_runtimeeffect_make_for_color_filter'
foreign import ccall "&sk_runtimeeffect_make_for_color_filter" p'sk_runtimeeffect_make_for_color_filter ::
  FunPtr (Ptr (Sk_string) -> Ptr (Sk_string) -> IO (Ptr (Sk_runtimeeffect)))

{- | C function signature:

@
sk_runtimeeffect_t *sk_runtimeeffect_make_for_shader(sk_string_t *sksl, sk_string_t *error)
@
-}
foreign import ccall "sk_runtimeeffect_make_for_shader" sk_runtimeeffect_make_for_shader ::
  Ptr (Sk_string) -- ^ C argument @"sk_string_t * sksl"@
  -> Ptr (Sk_string) -- ^ C argument @"sk_string_t * error"@
  -> IO (Ptr (Sk_runtimeeffect)) -- ^ C return type: @"sk_runtimeeffect_t *"@

-- | Function pointer to 'sk_runtimeeffect_make_for_shader'
foreign import ccall "&sk_runtimeeffect_make_for_shader" p'sk_runtimeeffect_make_for_shader ::
  FunPtr (Ptr (Sk_string) -> Ptr (Sk_string) -> IO (Ptr (Sk_runtimeeffect)))

{- | C function signature:

@
sk_runtimeeffect_t *sk_runtimeeffect_make_for_blender(sk_string_t *sksl, sk_string_t *error)
@
-}
foreign import ccall "sk_runtimeeffect_make_for_blender" sk_runtimeeffect_make_for_blender ::
  Ptr (Sk_string) -- ^ C argument @"sk_string_t * sksl"@
  -> Ptr (Sk_string) -- ^ C argument @"sk_string_t * error"@
  -> IO (Ptr (Sk_runtimeeffect)) -- ^ C return type: @"sk_runtimeeffect_t *"@

-- | Function pointer to 'sk_runtimeeffect_make_for_blender'
foreign import ccall "&sk_runtimeeffect_make_for_blender" p'sk_runtimeeffect_make_for_blender ::
  FunPtr (Ptr (Sk_string) -> Ptr (Sk_string) -> IO (Ptr (Sk_runtimeeffect)))

{- | C function signature:

@
void sk_runtimeeffect_unref(sk_runtimeeffect_t *effect)
@
-}
foreign import ccall "sk_runtimeeffect_unref" sk_runtimeeffect_unref ::
  Ptr (Sk_runtimeeffect) -- ^ C argument @"sk_runtimeeffect_t * effect"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_runtimeeffect_unref'
foreign import ccall "&sk_runtimeeffect_unref" p'sk_runtimeeffect_unref ::
  FunPtr (Ptr (Sk_runtimeeffect) -> IO (()))

{- | C function signature:

@
sk_shader_t *sk_runtimeeffect_make_shader(sk_runtimeeffect_t *effect, sk_data_t *uniforms, sk_flattenable_t **children, size_t childCount, const sk_matrix_t *localMatrix)
@
-}
foreign import ccall "sk_runtimeeffect_make_shader" sk_runtimeeffect_make_shader ::
  Ptr (Sk_runtimeeffect) -- ^ C argument @"sk_runtimeeffect_t * effect"@
  -> Ptr (Sk_data) -- ^ C argument @"sk_data_t * uniforms"@
  -> Ptr (Ptr (Sk_flattenable)) -- ^ C argument @"sk_flattenable_t ** children"@
  -> CSize -- ^ C argument @"size_t childCount"@
  -> Ptr (Sk_matrix) -- ^ C argument @"const sk_matrix_t * localMatrix"@
  -> IO (Ptr (Sk_shader)) -- ^ C return type: @"sk_shader_t *"@

-- | Function pointer to 'sk_runtimeeffect_make_shader'
foreign import ccall "&sk_runtimeeffect_make_shader" p'sk_runtimeeffect_make_shader ::
  FunPtr (Ptr (Sk_runtimeeffect) -> Ptr (Sk_data) -> Ptr (Ptr (Sk_flattenable)) -> CSize -> Ptr (Sk_matrix) -> IO (Ptr (Sk_shader)))

{- | C function signature:

@
sk_colorfilter_t *sk_runtimeeffect_make_color_filter(sk_runtimeeffect_t *effect, sk_data_t *uniforms, sk_flattenable_t **children, size_t childCount)
@
-}
foreign import ccall "sk_runtimeeffect_make_color_filter" sk_runtimeeffect_make_color_filter ::
  Ptr (Sk_runtimeeffect) -- ^ C argument @"sk_runtimeeffect_t * effect"@
  -> Ptr (Sk_data) -- ^ C argument @"sk_data_t * uniforms"@
  -> Ptr (Ptr (Sk_flattenable)) -- ^ C argument @"sk_flattenable_t ** children"@
  -> CSize -- ^ C argument @"size_t childCount"@
  -> IO (Ptr (Sk_colorfilter)) -- ^ C return type: @"sk_colorfilter_t *"@

-- | Function pointer to 'sk_runtimeeffect_make_color_filter'
foreign import ccall "&sk_runtimeeffect_make_color_filter" p'sk_runtimeeffect_make_color_filter ::
  FunPtr (Ptr (Sk_runtimeeffect) -> Ptr (Sk_data) -> Ptr (Ptr (Sk_flattenable)) -> CSize -> IO (Ptr (Sk_colorfilter)))

{- | C function signature:

@
sk_blender_t *sk_runtimeeffect_make_blender(sk_runtimeeffect_t *effect, sk_data_t *uniforms, sk_flattenable_t **children, size_t childCount)
@
-}
foreign import ccall "sk_runtimeeffect_make_blender" sk_runtimeeffect_make_blender ::
  Ptr (Sk_runtimeeffect) -- ^ C argument @"sk_runtimeeffect_t * effect"@
  -> Ptr (Sk_data) -- ^ C argument @"sk_data_t * uniforms"@
  -> Ptr (Ptr (Sk_flattenable)) -- ^ C argument @"sk_flattenable_t ** children"@
  -> CSize -- ^ C argument @"size_t childCount"@
  -> IO (Ptr (Sk_blender)) -- ^ C return type: @"sk_blender_t *"@

-- | Function pointer to 'sk_runtimeeffect_make_blender'
foreign import ccall "&sk_runtimeeffect_make_blender" p'sk_runtimeeffect_make_blender ::
  FunPtr (Ptr (Sk_runtimeeffect) -> Ptr (Sk_data) -> Ptr (Ptr (Sk_flattenable)) -> CSize -> IO (Ptr (Sk_blender)))

{- | C function signature:

@
size_t sk_runtimeeffect_get_uniform_byte_size(const sk_runtimeeffect_t *effect)
@
-}
foreign import ccall "sk_runtimeeffect_get_uniform_byte_size" sk_runtimeeffect_get_uniform_byte_size ::
  Ptr (Sk_runtimeeffect) -- ^ C argument @"const sk_runtimeeffect_t * effect"@
  -> IO (CSize) -- ^ C return type: @"size_t"@

-- | Function pointer to 'sk_runtimeeffect_get_uniform_byte_size'
foreign import ccall "&sk_runtimeeffect_get_uniform_byte_size" p'sk_runtimeeffect_get_uniform_byte_size ::
  FunPtr (Ptr (Sk_runtimeeffect) -> IO (CSize))

{- | C function signature:

@
size_t sk_runtimeeffect_get_uniforms_size(const sk_runtimeeffect_t *effect)
@
-}
foreign import ccall "sk_runtimeeffect_get_uniforms_size" sk_runtimeeffect_get_uniforms_size ::
  Ptr (Sk_runtimeeffect) -- ^ C argument @"const sk_runtimeeffect_t * effect"@
  -> IO (CSize) -- ^ C return type: @"size_t"@

-- | Function pointer to 'sk_runtimeeffect_get_uniforms_size'
foreign import ccall "&sk_runtimeeffect_get_uniforms_size" p'sk_runtimeeffect_get_uniforms_size ::
  FunPtr (Ptr (Sk_runtimeeffect) -> IO (CSize))

{- | C function signature:

@
void sk_runtimeeffect_get_uniform_name(const sk_runtimeeffect_t *effect, int index, sk_string_t *name)
@
-}
foreign import ccall "sk_runtimeeffect_get_uniform_name" sk_runtimeeffect_get_uniform_name ::
  Ptr (Sk_runtimeeffect) -- ^ C argument @"const sk_runtimeeffect_t * effect"@
  -> CInt -- ^ C argument @"int index"@
  -> Ptr (Sk_string) -- ^ C argument @"sk_string_t * name"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_runtimeeffect_get_uniform_name'
foreign import ccall "&sk_runtimeeffect_get_uniform_name" p'sk_runtimeeffect_get_uniform_name ::
  FunPtr (Ptr (Sk_runtimeeffect) -> CInt -> Ptr (Sk_string) -> IO (()))

{- | C function signature:

@
void sk_runtimeeffect_get_uniform_from_index(const sk_runtimeeffect_t *effect, int index, sk_runtimeeffect_uniform_t *cuniform)
@
-}
foreign import ccall "sk_runtimeeffect_get_uniform_from_index" sk_runtimeeffect_get_uniform_from_index ::
  Ptr (Sk_runtimeeffect) -- ^ C argument @"const sk_runtimeeffect_t * effect"@
  -> CInt -- ^ C argument @"int index"@
  -> Ptr (Sk_runtimeeffect_uniform) -- ^ C argument @"sk_runtimeeffect_uniform_t * cuniform"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_runtimeeffect_get_uniform_from_index'
foreign import ccall "&sk_runtimeeffect_get_uniform_from_index" p'sk_runtimeeffect_get_uniform_from_index ::
  FunPtr (Ptr (Sk_runtimeeffect) -> CInt -> Ptr (Sk_runtimeeffect_uniform) -> IO (()))

{- | C function signature:

@
void sk_runtimeeffect_get_uniform_from_name(const sk_runtimeeffect_t *effect, const char *name, size_t len, sk_runtimeeffect_uniform_t *cuniform)
@
-}
foreign import ccall "sk_runtimeeffect_get_uniform_from_name" sk_runtimeeffect_get_uniform_from_name ::
  Ptr (Sk_runtimeeffect) -- ^ C argument @"const sk_runtimeeffect_t * effect"@
  -> Ptr (CChar) -- ^ C argument @"const char * name"@
  -> CSize -- ^ C argument @"size_t len"@
  -> Ptr (Sk_runtimeeffect_uniform) -- ^ C argument @"sk_runtimeeffect_uniform_t * cuniform"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_runtimeeffect_get_uniform_from_name'
foreign import ccall "&sk_runtimeeffect_get_uniform_from_name" p'sk_runtimeeffect_get_uniform_from_name ::
  FunPtr (Ptr (Sk_runtimeeffect) -> Ptr (CChar) -> CSize -> Ptr (Sk_runtimeeffect_uniform) -> IO (()))

{- | C function signature:

@
size_t sk_runtimeeffect_get_children_size(const sk_runtimeeffect_t *effect)
@
-}
foreign import ccall "sk_runtimeeffect_get_children_size" sk_runtimeeffect_get_children_size ::
  Ptr (Sk_runtimeeffect) -- ^ C argument @"const sk_runtimeeffect_t * effect"@
  -> IO (CSize) -- ^ C return type: @"size_t"@

-- | Function pointer to 'sk_runtimeeffect_get_children_size'
foreign import ccall "&sk_runtimeeffect_get_children_size" p'sk_runtimeeffect_get_children_size ::
  FunPtr (Ptr (Sk_runtimeeffect) -> IO (CSize))

{- | C function signature:

@
void sk_runtimeeffect_get_child_name(const sk_runtimeeffect_t *effect, int index, sk_string_t *name)
@
-}
foreign import ccall "sk_runtimeeffect_get_child_name" sk_runtimeeffect_get_child_name ::
  Ptr (Sk_runtimeeffect) -- ^ C argument @"const sk_runtimeeffect_t * effect"@
  -> CInt -- ^ C argument @"int index"@
  -> Ptr (Sk_string) -- ^ C argument @"sk_string_t * name"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_runtimeeffect_get_child_name'
foreign import ccall "&sk_runtimeeffect_get_child_name" p'sk_runtimeeffect_get_child_name ::
  FunPtr (Ptr (Sk_runtimeeffect) -> CInt -> Ptr (Sk_string) -> IO (()))

{- | C function signature:

@
void sk_runtimeeffect_get_child_from_index(const sk_runtimeeffect_t *effect, int index, sk_runtimeeffect_child_t *cchild)
@
-}
foreign import ccall "sk_runtimeeffect_get_child_from_index" sk_runtimeeffect_get_child_from_index ::
  Ptr (Sk_runtimeeffect) -- ^ C argument @"const sk_runtimeeffect_t * effect"@
  -> CInt -- ^ C argument @"int index"@
  -> Ptr (Sk_runtimeeffect_child) -- ^ C argument @"sk_runtimeeffect_child_t * cchild"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_runtimeeffect_get_child_from_index'
foreign import ccall "&sk_runtimeeffect_get_child_from_index" p'sk_runtimeeffect_get_child_from_index ::
  FunPtr (Ptr (Sk_runtimeeffect) -> CInt -> Ptr (Sk_runtimeeffect_child) -> IO (()))

{- | C function signature:

@
void sk_runtimeeffect_get_child_from_name(const sk_runtimeeffect_t *effect, const char *name, size_t len, sk_runtimeeffect_child_t *cchild)
@
-}
foreign import ccall "sk_runtimeeffect_get_child_from_name" sk_runtimeeffect_get_child_from_name ::
  Ptr (Sk_runtimeeffect) -- ^ C argument @"const sk_runtimeeffect_t * effect"@
  -> Ptr (CChar) -- ^ C argument @"const char * name"@
  -> CSize -- ^ C argument @"size_t len"@
  -> Ptr (Sk_runtimeeffect_child) -- ^ C argument @"sk_runtimeeffect_child_t * cchild"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_runtimeeffect_get_child_from_name'
foreign import ccall "&sk_runtimeeffect_get_child_from_name" p'sk_runtimeeffect_get_child_from_name ::
  FunPtr (Ptr (Sk_runtimeeffect) -> Ptr (CChar) -> CSize -> Ptr (Sk_runtimeeffect_child) -> IO (()))

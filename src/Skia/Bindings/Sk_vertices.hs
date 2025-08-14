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
module Skia.Bindings.Sk_vertices where

import Foreign
import Foreign.C
import Foreign.Storable
import Foreign.Storable.Offset

import Skia.Bindings.Types


{- | C function signature:

@
void sk_vertices_unref(sk_vertices_t *cvertices)
@
-}
foreign import ccall "sk_vertices_unref" sk_vertices_unref ::
  Ptr (Sk_vertices) -- ^ C argument @"sk_vertices_t * cvertices"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_vertices_unref'
foreign import ccall "&sk_vertices_unref" p'sk_vertices_unref ::
  FunPtr (Ptr (Sk_vertices) -> IO (()))

{- | C function signature:

@
void sk_vertices_ref(sk_vertices_t *cvertices)
@
-}
foreign import ccall "sk_vertices_ref" sk_vertices_ref ::
  Ptr (Sk_vertices) -- ^ C argument @"sk_vertices_t * cvertices"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_vertices_ref'
foreign import ccall "&sk_vertices_ref" p'sk_vertices_ref ::
  FunPtr (Ptr (Sk_vertices) -> IO (()))

{- | C function signature:

@
sk_vertices_t *sk_vertices_make_copy(sk_vertices_vertex_mode_t vmode, int vertexCount, const sk_point_t *positions, const sk_point_t *texs, const sk_color_t *colors, int indexCount, const uint16_t *indices)
@
-}
foreign import ccall "sk_vertices_make_copy" sk_vertices_make_copy ::
  Sk_vertices_vertex_mode -- ^ C argument @"sk_vertices_vertex_mode_t vmode"@
  -> CInt -- ^ C argument @"int vertexCount"@
  -> Ptr (Sk_point) -- ^ C argument @"const sk_point_t * positions"@
  -> Ptr (Sk_point) -- ^ C argument @"const sk_point_t * texs"@
  -> Ptr (Sk_color) -- ^ C argument @"const sk_color_t * colors"@
  -> CInt -- ^ C argument @"int indexCount"@
  -> Ptr (Word16) -- ^ C argument @"const uint16_t * indices"@
  -> IO (Ptr (Sk_vertices)) -- ^ C return type: @"sk_vertices_t *"@

-- | Function pointer to 'sk_vertices_make_copy'
foreign import ccall "&sk_vertices_make_copy" p'sk_vertices_make_copy ::
  FunPtr (Sk_vertices_vertex_mode -> CInt -> Ptr (Sk_point) -> Ptr (Sk_point) -> Ptr (Sk_color) -> CInt -> Ptr (Word16) -> IO (Ptr (Sk_vertices)))

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
module Skia.Bindings.Sksvg where

import Foreign
import Foreign.C
import Foreign.Storable
import Foreign.Storable.Offset

import Skia.Bindings.Types


{- | C function signature:

@
const sksvg_dom_t *sksvg_dom_create(sk_stream_t *stream)
@
-}
foreign import ccall "sksvg_dom_create" sksvg_dom_create ::
  Ptr (Sk_stream) -- ^ C argument @"sk_stream_t * stream"@
  -> IO (Ptr (Sksvg_dom)) -- ^ C return type: @"const sksvg_dom_t *"@

-- | Function pointer to 'sksvg_dom_create'
foreign import ccall "&sksvg_dom_create" p'sksvg_dom_create ::
  FunPtr (Ptr (Sk_stream) -> IO (Ptr (Sksvg_dom)))

{- | C function signature:

@
void sksvg_dom_get_container_size(sksvg_dom_t *dom, int32_t viewportWidth, int32_t viewportHeight, sk_scalar_t dpi, int32_t *dstWidth, int32_t *dstHeight)
@
-}
foreign import ccall "sksvg_dom_get_container_size" sksvg_dom_get_container_size ::
  Ptr (Sksvg_dom) -- ^ C argument @"sksvg_dom_t * dom"@
  -> Int32 -- ^ C argument @"int32_t viewportWidth"@
  -> Int32 -- ^ C argument @"int32_t viewportHeight"@
  -> Sk_scalar -- ^ C argument @"sk_scalar_t dpi"@
  -> Ptr (Int32) -- ^ C argument @"int32_t * dstWidth"@
  -> Ptr (Int32) -- ^ C argument @"int32_t * dstHeight"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sksvg_dom_get_container_size'
foreign import ccall "&sksvg_dom_get_container_size" p'sksvg_dom_get_container_size ::
  FunPtr (Ptr (Sksvg_dom) -> Int32 -> Int32 -> Sk_scalar -> Ptr (Int32) -> Ptr (Int32) -> IO (()))

{- | C function signature:

@
void sksvg_dom_render(sksvg_dom_t *dom, sk_canvas_t *canvas)
@
-}
foreign import ccall "sksvg_dom_render" sksvg_dom_render ::
  Ptr (Sksvg_dom) -- ^ C argument @"sksvg_dom_t * dom"@
  -> Ptr (Sk_canvas) -- ^ C argument @"sk_canvas_t * canvas"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sksvg_dom_render'
foreign import ccall "&sksvg_dom_render" p'sksvg_dom_render ::
  FunPtr (Ptr (Sksvg_dom) -> Ptr (Sk_canvas) -> IO (()))

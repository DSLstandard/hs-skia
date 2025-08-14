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
module Skia.Bindings.Sk_svg where

import Foreign
import Foreign.C
import Foreign.Storable
import Foreign.Storable.Offset

import Skia.Bindings.Types


{- | C function signature:

@
sk_canvas_t *sk_svgcanvas_create_with_stream(const sk_rect_t *bounds, sk_wstream_t *stream)
@
-}
foreign import ccall "sk_svgcanvas_create_with_stream" sk_svgcanvas_create_with_stream ::
  Ptr (Sk_rect) -- ^ C argument @"const sk_rect_t * bounds"@
  -> Ptr (Sk_wstream) -- ^ C argument @"sk_wstream_t * stream"@
  -> IO (Ptr (Sk_canvas)) -- ^ C return type: @"sk_canvas_t *"@

-- | Function pointer to 'sk_svgcanvas_create_with_stream'
foreign import ccall "&sk_svgcanvas_create_with_stream" p'sk_svgcanvas_create_with_stream ::
  FunPtr (Ptr (Sk_rect) -> Ptr (Sk_wstream) -> IO (Ptr (Sk_canvas)))

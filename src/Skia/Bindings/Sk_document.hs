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
module Skia.Bindings.Sk_document where

import Foreign
import Foreign.C
import Foreign.Storable
import Foreign.Storable.Offset

import Skia.Bindings.Types


{- | C function signature:

@
void sk_document_unref(sk_document_t *document)
@
-}
foreign import ccall "sk_document_unref" sk_document_unref ::
  Ptr (Sk_document) -- ^ C argument @"sk_document_t * document"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_document_unref'
foreign import ccall "&sk_document_unref" p'sk_document_unref ::
  FunPtr (Ptr (Sk_document) -> IO (()))

{- | C function signature:

@
sk_document_t *sk_document_create_pdf_from_stream(sk_wstream_t *stream)
@
-}
foreign import ccall "sk_document_create_pdf_from_stream" sk_document_create_pdf_from_stream ::
  Ptr (Sk_wstream) -- ^ C argument @"sk_wstream_t * stream"@
  -> IO (Ptr (Sk_document)) -- ^ C return type: @"sk_document_t *"@

-- | Function pointer to 'sk_document_create_pdf_from_stream'
foreign import ccall "&sk_document_create_pdf_from_stream" p'sk_document_create_pdf_from_stream ::
  FunPtr (Ptr (Sk_wstream) -> IO (Ptr (Sk_document)))

{- | C function signature:

@
sk_document_t *sk_document_create_pdf_from_stream_with_metadata(sk_wstream_t *stream, const sk_document_pdf_metadata_t *metadata)
@
-}
foreign import ccall "sk_document_create_pdf_from_stream_with_metadata" sk_document_create_pdf_from_stream_with_metadata ::
  Ptr (Sk_wstream) -- ^ C argument @"sk_wstream_t * stream"@
  -> Ptr (Sk_document_pdf_metadata) -- ^ C argument @"const sk_document_pdf_metadata_t * metadata"@
  -> IO (Ptr (Sk_document)) -- ^ C return type: @"sk_document_t *"@

-- | Function pointer to 'sk_document_create_pdf_from_stream_with_metadata'
foreign import ccall "&sk_document_create_pdf_from_stream_with_metadata" p'sk_document_create_pdf_from_stream_with_metadata ::
  FunPtr (Ptr (Sk_wstream) -> Ptr (Sk_document_pdf_metadata) -> IO (Ptr (Sk_document)))

{- | C function signature:

@
sk_canvas_t *sk_document_begin_page(sk_document_t *document, float width, float height, const sk_rect_t *content)
@
-}
foreign import ccall "sk_document_begin_page" sk_document_begin_page ::
  Ptr (Sk_document) -- ^ C argument @"sk_document_t * document"@
  -> CFloat -- ^ C argument @"float width"@
  -> CFloat -- ^ C argument @"float height"@
  -> Ptr (Sk_rect) -- ^ C argument @"const sk_rect_t * content"@
  -> IO (Ptr (Sk_canvas)) -- ^ C return type: @"sk_canvas_t *"@

-- | Function pointer to 'sk_document_begin_page'
foreign import ccall "&sk_document_begin_page" p'sk_document_begin_page ::
  FunPtr (Ptr (Sk_document) -> CFloat -> CFloat -> Ptr (Sk_rect) -> IO (Ptr (Sk_canvas)))

{- | C function signature:

@
void sk_document_end_page(sk_document_t *document)
@
-}
foreign import ccall "sk_document_end_page" sk_document_end_page ::
  Ptr (Sk_document) -- ^ C argument @"sk_document_t * document"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_document_end_page'
foreign import ccall "&sk_document_end_page" p'sk_document_end_page ::
  FunPtr (Ptr (Sk_document) -> IO (()))

{- | C function signature:

@
void sk_document_close(sk_document_t *document)
@
-}
foreign import ccall "sk_document_close" sk_document_close ::
  Ptr (Sk_document) -- ^ C argument @"sk_document_t * document"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_document_close'
foreign import ccall "&sk_document_close" p'sk_document_close ::
  FunPtr (Ptr (Sk_document) -> IO (()))

{- | C function signature:

@
void sk_document_abort(sk_document_t *document)
@
-}
foreign import ccall "sk_document_abort" sk_document_abort ::
  Ptr (Sk_document) -- ^ C argument @"sk_document_t * document"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_document_abort'
foreign import ccall "&sk_document_abort" p'sk_document_abort ::
  FunPtr (Ptr (Sk_document) -> IO (()))

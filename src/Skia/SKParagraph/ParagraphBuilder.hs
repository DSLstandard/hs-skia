module Skia.SKParagraph.ParagraphBuilder where

import Data.ByteString qualified as BS
import Data.Text qualified as T
import Skia.Bindings.Skparagraph
import Skia.SKParagraph.Internal.Prelude

create ::
  (MonadResource m) =>
  ParagraphStyle ->
  FontCollection ->
  SKUnicode ->
  m (ReleaseKey, ParagraphBuilderImpl)
create paraStyle fontCollection unicode =
  allocateSKObjectNeverNull
    ( skparagraph_paragraph_builder_impl_new
        (ptr paraStyle)
        (ptr fontCollection)
        (ptr unicode)
    )
    skparagraph_paragraph_builder_impl_delete

addByteStringUTF8 :: (MonadIO m, IsParagraphBuilder builder) => builder -> BS.ByteString -> m ()
addByteStringUTF8 (toA ParagraphBuilder -> builder) utf8 = evalManaged do
  (utf8', len) <- storableByteStringLen utf8
  liftIO $ skparagraph_paragraph_builder_add_text_utf8_len (ptr builder) utf8' (fromIntegral len)

addText :: (MonadIO m, IsParagraphBuilder builder) => builder -> T.Text -> m ()
addText (toA ParagraphBuilder -> builder) text = evalManaged do
  (utf8', len) <- storableTextUTF8Len text
  liftIO $ skparagraph_paragraph_builder_add_text_utf8_len (ptr builder) utf8' (fromIntegral len)

build :: (MonadResource m, IsParagraphBuilder builder) => builder -> m (ReleaseKey, Paragraph)
build (toA ParagraphBuilder -> builder) =
  allocateSKObjectNeverNull
    (skparagraph_paragraph_builder_build (ptr builder))
    skparagraph_paragraph_delete

pushStyle :: (MonadIO m, IsParagraphBuilder builder) => builder -> TextStyle -> m ()
pushStyle (toA ParagraphBuilder -> builder) style =
  liftIO $ skparagraph_paragraph_builder_push_style (ptr builder) (ptr style)

pop :: (MonadIO m, IsParagraphBuilder builder) => builder -> m ()
pop (toA ParagraphBuilder -> builder) =
  liftIO $ skparagraph_paragraph_builder_pop (ptr builder)

-- | Resets this builder to its initial state, discarding any text, styles,
-- placeholders that have been added, but keeping the initial ParagraphStyle.
reset :: (MonadIO m, IsParagraphBuilder builder) => builder -> m ()
reset  (toA ParagraphBuilder -> builder) =
  liftIO $ skparagraph_paragraph_builder_reset (ptr builder)

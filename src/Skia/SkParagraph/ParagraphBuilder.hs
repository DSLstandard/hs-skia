module Skia.SkParagraph.ParagraphBuilder where

import Data.ByteString qualified as BS
import Data.Text qualified as T
import Skia.SkParagraph.Internal.Prelude
import Skia.SkParagraph.TextStyle
import Language.C.Inline.Cpp qualified as C
import Skia.SkUnicode.Objects

C.context $ C.cppCtx <> cppSkiaObjectTypes <> cppSkUnicodeObjectTypes <> cppSkParagraphObjectTypes

C.include "modules/skparagraph/include/ParagraphBuilder.h"
C.include "modules/skparagraph/src/ParagraphBuilderImpl.h"

create ::
  (MonadResource m) =>
  ParagraphStyle ->
  FontCollection ->
  SkUnicode ->
  m (ReleaseKey, ParagraphBuilderImpl)
create (ptr -> paraStyle) (ptr -> fontCollection) (ptr -> unicode) =
  allocateSkObjectNeverNull
    [C.exp| skia::textlayout::ParagraphBuilderImpl* {
      new skia::textlayout::ParagraphBuilderImpl(
        *$(skia::textlayout::ParagraphStyle* paraStyle),
        sk_ref_sp($(skia::textlayout::FontCollection* fontCollection)),
        sk_ref_sp($(SkUnicode* unicode))
      )
    }|]
    \ptr ->
      [C.block| void {
        delete $(skia::textlayout::ParagraphBuilderImpl* ptr);
      }|]

addByteStringUTF8 :: (MonadIO m, IsParagraphBuilder builder) => builder -> BS.ByteString -> m ()
addByteStringUTF8 (ptr . toA ParagraphBuilder -> builder) utf8 = evalManaged do
  (utf8', fromIntegral -> len) <- storableByteStringLen utf8
  liftIO [C.block| void {
    $(skia::textlayout::ParagraphBuilder* builder)->addText($(const char* utf8'), $(size_t len));
  }|]

addText :: (MonadIO m, IsParagraphBuilder builder) => builder -> T.Text -> m ()
addText (ptr . toA ParagraphBuilder -> builder) text = evalManaged do
  (utf8', fromIntegral -> len) <- storableTextUTF8Len text
  liftIO [C.block| void {
    $(skia::textlayout::ParagraphBuilder* builder)->addText($(const char* utf8'), $(size_t len));
  }|]

addPlaceholder :: (MonadIO m, IsParagraphBuilder builder) => builder -> PlaceholderStyle -> m ()
addPlaceholder (ptr . toA ParagraphBuilder -> builder) style = evalManaged do
  style' <- marshalPlaceholderStyle style
  liftIO [C.block| void {
    $(skia::textlayout::ParagraphBuilder* builder)->addPlaceholder(*$(const skia::textlayout::PlaceholderStyle* style'));
  }|]

build :: (MonadResource m, IsParagraphBuilder builder) => builder -> m (ReleaseKey, Paragraph)
build (ptr . toA ParagraphBuilder -> builder) =
  allocateSkObjectNeverNull
    [C.exp| skia::textlayout::Paragraph* {
      $(skia::textlayout::ParagraphBuilder* builder)->Build().release()
    }|]
    \ptr ->
      [C.block| void {
        delete $(skia::textlayout::Paragraph* ptr);
      }|]

pushStyle :: (MonadIO m, IsParagraphBuilder builder) => builder -> TextStyle -> m ()
pushStyle (ptr . toA ParagraphBuilder -> builder) (ptr -> style) = liftIO do
  [C.block| void {
    $(skia::textlayout::ParagraphBuilder* builder)->pushStyle(*$(skia::textlayout::TextStyle* style));
  }|]

pop :: (MonadIO m, IsParagraphBuilder builder) => builder -> m ()
pop (ptr . toA ParagraphBuilder -> builder) = liftIO do
  [C.block| void {
    $(skia::textlayout::ParagraphBuilder* builder)->pop();
  }|]

-- | Resets this builder to its initial state, discarding any text, styles,
-- placeholders that have been added, but keeping the initial ParagraphStyle.
reset :: (MonadIO m, IsParagraphBuilder builder) => builder -> m ()
reset (ptr . toA ParagraphBuilder -> builder) = liftIO do
  [C.block| void {
    $(skia::textlayout::ParagraphBuilder* builder)->Reset();
  }|]
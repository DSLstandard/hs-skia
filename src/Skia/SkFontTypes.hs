module Skia.SkFontTypes where

import Skia.Internal.Prelude
import Skia.Internal.THUtils
import Language.C.Inline.Cpp qualified as C

C.context $ C.cppCtx <> cppSkiaObjectTypes

C.include "core/SkFontTypes.h"

$( qGenerateSkEnum
  "SkTextEncoding"
  ""
  [ ("UTF8", "SkTextEncoding::kUTF8", "uses bytes to represent UTF-8 or ASCII")
  , ("UTF16", "SkTextEncoding::kUTF16", "uses two byte words to represent most of Unicode")
  , ("UTF32", "SkTextEncoding::kUTF32", "uses four byte words to represent all of Unicode")
  , ("GlyphID", "SkTextEncoding::kGlyphID", "uses two byte words to represent glyph indices")
  ]
 )

$( qGenerateSkEnum
  "SkFontHinting"
  ""
  [ ("None", "SkFontHinting::kNone", "glyph outlines unchanged")
  , ("Slight", "SkFontHinting::kSlight", "minimal modification to improve contrast")
  , ("Normal", "SkFontHinting::kNormal", "glyph outlines modified to improve contrast")
  , ("Full", "SkFontHinting::kFull", "modifies glyph outlines for maximum contrast")
  ]
 )

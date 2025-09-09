module Skia.SkUnicode.Objects where

import Skia.Objects
import Skia.Internal.Prelude
import Skia.Internal.THUtils
import Language.C.Inline.Cpp qualified as C

$( qGenerateSkObject
    "SkUnicode"
    "SkUnicode"
    [''SkRefCnt]
    ""
 )

cppSkUnicodeObjectTypes :: C.Context
cppSkUnicodeObjectTypes = formInlineCppTypePairs $(qGatherInlineSkObjectEntries)
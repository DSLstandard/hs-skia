module Skia.SkSVG.Objects where

import Foreign
import Skia.Core
import Skia.Internal.THUtils
import Skia.Objects
import Language.C.Inline.Cpp qualified as C

$( qGenerateSkObject
  "SkSVGDOM"
  "Dom"
  [''SkRefCnt] -- class SK_API SkSVGDOM : public SkRefCnt {
  ""
 )

cppSkSVGObjectTypes :: C.Context
cppSkSVGObjectTypes = formInlineCppTypePairs $(qGatherInlineSkObjectEntries)

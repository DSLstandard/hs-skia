module Skia.SKSvg.Objects where

import Foreign
import Skia.Bindings.Types
import Skia.Core
import Skia.Internal.THUtils
import Skia.Objects

$(qGenerateSKObject
  "Dom"
  ''Sksvg_dom
  [''SKRefCnt] -- class SK_API SkSVGDOM : public SkRefCnt {
  ""
 )
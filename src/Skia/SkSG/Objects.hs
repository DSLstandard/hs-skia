module Skia.SkSG.Objects where

import Language.C.Inline.Cpp qualified as C
import NeatInterpolation
import Skia.Internal.Prelude
import Skia.Internal.THUtils

$( qGenerateSkObject
  "sksg::InvalidationController"
  "InvalidationController"
  [''SkRefCnt]
  [trimming|
    Receiver for invalidation events.

    Tracks dirty regions for repaint.
  |]
 )

cppSkSGObjectTypes :: C.Context
cppSkSGObjectTypes = formInlineCppTypePairs $(qGatherInlineSkObjectEntries)

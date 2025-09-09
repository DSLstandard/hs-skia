module Skia.SkResources.Objects where

import Language.C.Inline.Cpp qualified as C
import NeatInterpolation
import Skia.Internal.Prelude
import Skia.Internal.THUtils

$( qGenerateSkObject
  "skresources::ResourceProvider"
  "ResourceProvider"
  [''SkRefCnt] -- class SK_API ResourceProvider : public SkRefCnt {
  [trimming|
    ResourceProvider is an interface that lets rich-content modules defer loading of external
    resources (images, fonts, etc.) to embedding clients.
  |]
 )

cppSkResourcesObjectTypes :: C.Context
cppSkResourcesObjectTypes = formInlineCppTypePairs $(qGatherInlineSkObjectEntries)

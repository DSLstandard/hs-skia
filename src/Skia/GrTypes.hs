module Skia.GrTypes where

import Skia.Internal.Prelude
import Skia.Internal.THUtils
import NeatInterpolation
import Language.C.Inline.Cpp qualified as C

C.context $ C.cppCtx <> cppSkiaObjectTypes

C.include "gpu/GrTypes.h"

$( qGenerateSkEnum
  "GrSurfaceOrigin"
  [trimming|
    GPU SkImage and SkSurfaces can be stored such that (0, 0) in texture space may correspond to
    either the top-left or bottom-left content pixel.
  |]
  [ ("TopLeft", "GrSurfaceOrigin::kTopLeft_GrSurfaceOrigin", "")
  , ("BottomLeft", "GrSurfaceOrigin::kBottomLeft_GrSurfaceOrigin", "")
  ]
 )

$( qGenerateSkEnum
  "GrBackendApi"
  [trimming|
    Possible 3D APIs that may be used by Ganesh.
  |]
  [ ("OpenGL", "GrBackendApi::kOpenGL", "")
  , ("Vulkan", "GrBackendApi::kVulkan", "")
  , ("Metal", "GrBackendApi::kMetal", "")
  , ("Direct3D", "GrBackendApi::kDirect3D", "")
  , ("Mock", "GrBackendApi::kMock", "Mock is a backend that does not draw anything. It is used for unit tests and to measure CPU overhead.")
  , ("Unsupported", "GrBackendApi::kUnsupported", "Ganesh doesn't support some context types (e.g. Dawn) and will return Unsupported.")
  ]
 )

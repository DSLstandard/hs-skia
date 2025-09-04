cmake_minimum_required(VERSION 3.16)
project(skia_capi LANGUAGES CXX)

# ** Find 'skia' with 'pkg-config'
find_package(PkgConfig REQUIRED)
pkg_check_modules(SKIA REQUIRED skia)
include_directories(${SKIA_INCLUDE_DIRS})
link_directories(${SKIA_LIBRARY_DIRS})

# ** Bundle up all cbits/*.cpp sources
set(CBITS_DIR "${CMAKE_SOURCE_DIR}/../cbits")
file(GLOB CBITS_SOURCES "${CBITS_DIR}/src/*.cpp")
add_library(skia_capi_objs OBJECT ${CBITS_SOURCES})
target_include_directories(skia_capi_objs PRIVATE ${CBITS_DIR} ${CBITS_DIR}/fake_headers)
target_compile_options(skia_capi_objs PRIVATE -Wall ${SKIA_CFLAGS})

# Define -DSK_GANESH. Skia graphics/GPU utils related. I am not sure what this
# does exactly, but not defining it would result in compilation errors.
target_compile_definitions(skia_capi_objs PRIVATE -DSK_GANESH)

# NOTE: If we don't use PCHs, the compilation speed is intolerable - using PCHs
# is obligatory.
target_precompile_headers(skia_capi_objs PRIVATE "${CBITS_DIR}/skia_capi/sk_types.h")
target_precompile_headers(skia_capi_objs PRIVATE "${CBITS_DIR}/src/common_include_pch.hpp" INTERFACE "${CBITS_DIR}/skia_capi/sk_types.h")

# ** Misc variables

# Some Skia libraries happen to be static libraries (when Skia is built with
# `is_component_build=true`).
set(EXTRA_SKIA_STATIC_LIBS
  skcms
  pathkit
  skresources
)
set(EXTRA_SKIA_SHARED_LIBS
  skia
  skottie
  skparagraph
  sksg
  skshaper
  # Other backends skunicode_* should be linked by the user on demand.
  #
  # N.B. skia_capi has weak symbols referencing the SKUnicode constructions of
  # other backends to enable ad-hoc backend support through ld.
  skunicode_core
  svg
)

# ** Setup static library
add_library(skia_capi_static STATIC $<TARGET_OBJECTS:skia_capi_objs>)
set_target_properties(skia_capi_static PROPERTIES OUTPUT_NAME skia_capi_static)
target_link_libraries(skia_capi_static PRIVATE ${SKIA_LINK_LIBRARIES} ${EXTRA_SKIA_STATIC_LIBS})

# ** Setup shared library for `cabal repl` and the Haskell Language Server
add_library(skia_capi_shared SHARED $<TARGET_OBJECTS:skia_capi_objs>)
set_target_properties(skia_capi_shared PROPERTIES OUTPUT_NAME skia_capi_shared)
target_link_libraries(skia_capi_shared PRIVATE ${SKIA_LINK_LIBRARIES} ${EXTRA_SKIA_STATIC_LIBS} ${EXTRA_SKIA_SHARED_LIBS})
target_compile_options(skia_capi_shared PRIVATE -fPIC)

# For developing cbits/ with clangd
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
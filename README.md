# hs-skia

Bindings to Google's Skia library through the C API of https://github.com/mono/skia

Working in progress!!

# Developer notes

## Demo

- If you already have `libSkiaSharp.so` built, do `cabal run skia:exe:demo-raster-example`.
- If you have `nix`, do `nix develop` build and install `libSkiaSharp.so` into
  the nix shell environment, then do `cabal run skia:exe:demo-raster-example`.

Running the demo should generate this PNG picture file:

![Demo generated PNG picture](./assets/raster-example-output.png)

## What is Skia and SkiaSharp
https://skia.org/: "Skia is an open source 2D graphics library which provides common APIs that work across a variety of hardware and software platforms. It serves as the graphics engine for Google Chrome and ChromeOS, Android, Flutter, and many other products."

Skia is [written in C++](https://github.com/google/skia). However, Haskell's FFI
does not mix well with C++
[apparently](https://www.reddit.com/r/haskell/comments/q2wwk1/do_you_use_ffi_to_bind_your_own_cc_function_in/).
To workaround this, a C API of Skia can be coded, then a Haskell skia binding
library can be bound to that C API. Google has [a C API of
Skia](https://chromium.googlesource.com/skia/+/master/experimental/c-api-example/c.md)
but it seems to be [not
well-maintained](https://news.ycombinator.com/item?id=39439035). Fortunately,
Microsoft provides [SkiaSharp](https://github.com/mono/SkiaSharp), a binding for
Skia for .NET platforms. Microsoft's SkiaSharp project forks Google's Skia and
implement their own C API of Skia [here](https://github.com/mono/skia). This
library is a Haskell binding library to that C API and it links to
`libSkiaSharp.so`.

## How to get Mono's Skia library

You have to build and install Mono's Skia library yourself. See
https://github.com/mono/SkiaSharp/wiki/Building-SkiaSharp.

This project provides `mono-skia-package.nix` to build and install Mono's Skia
library for developers to work on the Haskell library directly, though note that
this is a crude solution.

## How are the Haskell bindings made?

This project uses a Python script to parse Mono's Skia C API headers and
auto-generate almost all bindings. These generated Haskell bindings live in
`src/Skia/Bindings/AutoGenerated.hsc`. However, some C function bindings
require manual implementation, and they are handled in
`src/Skia/Bindings/Manual.hsc`.

## Future works

- Fix the binding generator's word breaker implementation. The binding
  generator's word breaker is an algorithm to break mangled C names like
  `gr_backendrendertarget_get_gl_framebufferinfo` into
  `gr/backend/render/target/get/gl/framebuffer/info` for use in generating nice
  camel case Haskell names like `grBackendRenderTargetGetGlFramebufferInfo`.
  However, the current implementation does not work perfectly.
- Auto-generate documentations.
- Create a high-level API like
  https://learn.microsoft.com/en-us/dotnet/api/skiasharp?view=skiasharp-2.88 +
  documentation.

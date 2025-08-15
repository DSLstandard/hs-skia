# ADAPTED FROM
# https://github.com/NixOS/nixpkgs/blob/nixos-25.05/pkgs/by-name/sk/skia/package.nix
{
  lib,
  stdenv,
  fetchgit,
  expat,
  fontconfig,
  freetype,
  harfbuzzFull,
  icu,
  gn,
  libGL,
  libjpeg,
  libwebp,
  libX11,
  ninja,
  python3,
  testers,
  vulkan-headers,
  vulkan-memory-allocator,
  xcbuild,
  cctools,
  zlib,
  fixDarwinDylibNames,

  enableVulkan ? !stdenv.hostPlatform.isDarwin,
}:

stdenv.mkDerivation (finalAttrs: {
  pname = "skia";
  # Version from https://skia.googlesource.com/skia/+/refs/heads/main/RELEASE_NOTES.md
  # or https://chromiumdash.appspot.com/releases
  # plus date of the tip of the corresponding chrome/m$version branch
  version = "129-unstable-2024-09-18";

  src = fetchgit {
    url = "https://skia.googlesource.com/skia.git";
    # Tip of the chrome/m$version branch
    rev = "dda581d538cb6532cda841444e7b4ceacde01ec9";
    hash = "sha256-NZiZFsABebugszpYsBusVlTYnYda+xDIpT05cZ8Jals=";
  };

  postPatch = ''
    # System zlib detection bug workaround
    substituteInPlace BUILD.gn \
      --replace-fail 'deps = [ "//third_party/zlib" ]' 'deps = []'

    # Google Skia's GN configs seem flawed. For example, `libskparagraph.so`
    # built using the provided GN configs with flag `is_component_build=true`
    # does not expose all symbols because `-fvisibility=hidden` is
    # (erroneously?) applied to all shared library GN targets. This is a patch
    # to fix this issue.
    patch --verbose -p0 <<EOF
diff --git gn/skia/BUILD.gn gn/skia/BUILD.gn
index 8a819a358c..e467433e60 100644
--- gn/skia/BUILD.gn
+++ gn/skia/BUILD.gn
@@ -145,7 +145,7 @@ config("default") {
     cflags += [
       "-fstrict-aliasing",
       "-fPIC",
-      "-fvisibility=hidden",
+      # "-fvisibility=hidden",
     ]
     cflags_cc += [
       "-std=c++17",
    EOF
  '';

  strictDeps = true;
  nativeBuildInputs = [
    gn
    ninja
    python3
  ]
  ++ lib.optionals stdenv.hostPlatform.isDarwin [
    xcbuild
    cctools.libtool
    zlib
    fixDarwinDylibNames
  ];

  buildInputs = [
    expat
    fontconfig
    freetype
    harfbuzzFull
    icu
    libGL
    libjpeg
    libwebp
    libX11
  ]
  ++ lib.optionals enableVulkan [
    vulkan-headers
    vulkan-memory-allocator
  ];

  gnFlags =
    let
      cpu =
        {
          "x86_64" = "x64";
          "i686" = "x86";
          "arm" = "arm";
          "aarch64" = "arm64";
        }
        .${stdenv.hostPlatform.parsed.cpu.name};
    in
    [
      # Build in release mode
      "is_official_build=true"
      "is_component_build=true"
      # Don't use missing tools
      "skia_use_dng_sdk=false"
      "skia_use_wuffs=false"
      # Ensure that some Skia modules are always enabled
      "skia_enable_skparagraph=true"
      "skia_enable_skshaper=true"
      "skia_enable_skunicode=true"
      "skia_enable_skottie=true"
      "skia_enable_svg=true"
      # Use system dependencies
      "extra_cflags=[\"-I${harfbuzzFull.dev}/include/harfbuzz\"]"
      "cc=\"${stdenv.cc.targetPrefix}cc\""
      "cxx=\"${stdenv.cc.targetPrefix}c++\""
      "ar=\"${stdenv.cc.targetPrefix}ar\""
      "target_cpu=\"${cpu}\""
    ]
    ++ map (lib: "skia_use_system_${lib}=true") [
      "zlib"
      "harfbuzz"
      "libpng"
      "libwebp"
    ]
    ++ lib.optionals enableVulkan [
      "skia_use_vulkan=true"
    ]
    ++ lib.optionals stdenv.hostPlatform.isDarwin [
      "skia_use_fontconfig=true"
      "skia_use_freetype=true"
      "skia_use_metal=true"
    ];

  env.NIX_LDFLAGS = lib.optionalString stdenv.hostPlatform.isDarwin "-lz";

  # Somewhat arbitrary, but similar to what other distros are doing
  installPhase = ''
    runHook preInstall

    # Libraries
    mkdir -p $out/lib
    cp *.so *.a *.dylib $out/lib

    # Includes
    pushd ../../include
    find . -name '*.h' -exec install -Dm644 {} $out/include/skia/{} \;
    popd
    pushd ../../modules
    find . -name '*.h' -exec install -Dm644 {} $out/include/skia/modules/{} \;
    popd

    # Pkg-config
    mkdir -p $out/lib/pkgconfig
    cat > $out/lib/pkgconfig/skia.pc <<'EOF'
    prefix=${placeholder "out"}
    exec_prefix=''${prefix}
    libdir=''${prefix}/lib
    includedir=''${prefix}/include/skia
    Name: skia
    Description: 2D graphic library for drawing text, geometries and images.
    URL: https://skia.org/
    Version: ${lib.versions.major finalAttrs.version}
    Libs: -L''${libdir} -lskia
    Cflags: -I''${includedir}
    EOF

    runHook postInstall
  '';

  preFixup = ''
    # Some skia includes are assumed to be under an include sub directory by
    # other includes
    for file in $(grep -rl '#include "include/' $out/include); do
      substituteInPlace "$file" \
        --replace-fail '#include "include/' '#include "'
    done
  '';

  passthru.tests.pkg-config = testers.testMetaPkgConfig finalAttrs.finalPackage;

  meta = {
    description = "2D graphic library for drawing text, geometries and images";
    homepage = "https://skia.org/";
    license = lib.licenses.bsd3;
    maintainers = with lib.maintainers; [ fgaz ];
    platforms = with lib.platforms; arm ++ aarch64 ++ x86 ++ x86_64;
    pkgConfigModules = [ "skia" ];
  };
})
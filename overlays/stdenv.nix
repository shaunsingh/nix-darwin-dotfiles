final: prev: {
  nativeStdenv =
    prev.stdenvAdapters.withCFlags [
      "-Ofast"
      "-pipe"
      "-mcpu=apple-m1"
      # "-fuse-ld=${final.mold}/bin/mold"
    ]
    final.llvmPackages_latest.stdenv;
  # nativeStdenv = prev.stdenvAdapters.withCFlags [
  #     "-Ofast"
  #     "-pipe"
  #     "-mcpu=apple-m1"
  #     "-fuse-ld=${final.mold}/bin/mold"
  #     ]
  #   (prev.overrideCC final.llvmPackages_latest.stdenv
  #     (prev.wrapCCWith {
  #       cc = final.llvmPackages_latest.clang-unwrapped;
  #       bintools = (final.wrapBintoolsWith {
  #         coreutils = final.uutils-coreutils;
  #         libc = final.glibc;
  #         # build bintools without ld linked
  #         bintools = prev.llvmPackages_latest.bintools.overrideAttrs (o: {
  #           postInstall = "ln -sf ${prev.mold}/bin/mold $out/bin/ld";
  #         });
  #       });
  #     }));
}

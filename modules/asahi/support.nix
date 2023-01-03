{ pkgs
, lib
, inputs
, config
, ...
}:
let
  bootFiles = {
    "m1n1/boot.bin" = pkgs.runCommand "boot.bin" { } ''
      cat ${pkgs.m1n1}/build/m1n1.bin > $out
      cat ${config.boot.kernelPackages.kernel}/dtbs/apple/*.dtb >> $out
      cat ${pkgs.u-boot}/u-boot-nodtb.bin.gz >> $out
    '';
  };
in
{
  hardware.opengl.package = pkgs.mesa-asahi.drivers;
  hardware.firmware = pkgs.stdenv.mkDerivation {
    name = "asahi-peripheral-firmware";

    nativeBuildInputs = [ pkgs.asahi-fwextract pkgs.cpio ];

    buildCommand = ''
      mkdir extracted
      asahi-fwextract ${/. + ../hardware/m1/firmware} extracted
      mkdir -p $out/lib/firmware
      cat extracted/firmware.cpio | cpio -id --quiet --no-absolute-filenames
      mv vendorfw/* $out/lib/firmware
    '';
  };
  system.extraDependencies = [ pkgs.m1n1 pkgs.u-boot ];
  system.build.m1n1 = bootFiles."m1n1/boot.bin";
}

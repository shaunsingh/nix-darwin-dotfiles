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
  hardware.firmware = pkgs.asahi-peripheral-firmware;

  boot.loader.systemd-boot.extraFiles = bootFiles;
  system.extraDependencies = [ pkgs.m1n1 pkgs.u-boot ];
  system.build.m1n1 = bootFiles."m1n1/boot.bin";
}

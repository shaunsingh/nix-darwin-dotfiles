{
  pkgs,
  lib,
  inputs,
  config,
  ...
}: {
  console = let
    normal = ["161616" "33b1ff" "ff7eb6" "42be65" "08bdba" "82cfff" "78a9ff" "dde1e6"];
    bright = ["525252" "33b1ff" "ff7eb6" "42be65" "08bdba" "82cfff" "78a9ff" "ffffff"];
  in {
    colors = normal ++ bright;
    keyMap = "us";
  };
}

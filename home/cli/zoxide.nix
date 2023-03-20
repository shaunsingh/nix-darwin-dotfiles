{ pkgs
, lib
, inputs
, config
, ...
}: {
  programs.zoxide = {
    enable = true;
    options = [ "--cmd cd" ];
  };
}

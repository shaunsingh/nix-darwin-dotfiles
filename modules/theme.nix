{ pkgs, lib, config, inputs, ...}:
{
  config = {
    themes.base16 = {
      enable = true;
      customScheme = {
        enable = true;
        path = ./themes/base16-nanolight.yaml;
      };
    };
  };
}

{ pkgs, lib, config, inputs, ...}:
{
  config = {
    themes.base16 = {
      enable = true;
      scheme = "nord";
    };
  };
}

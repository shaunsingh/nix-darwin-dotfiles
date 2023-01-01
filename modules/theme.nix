{ config, base16-oxocarbon, ... }: {
  config = {
    themes.base16 = {
      enable = true;
      path = "${base16-oxocarbon}/base16-oxocarbon-dark.yaml";
    };
  };
}

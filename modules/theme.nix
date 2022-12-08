{ config, base16-carbon-dark, ... }: {
  config = {
    themes.base16 = {
      enable = true;
      path = "${base16-carbon-dark}/base16-carbon-dark.yaml";
    };
  };
}

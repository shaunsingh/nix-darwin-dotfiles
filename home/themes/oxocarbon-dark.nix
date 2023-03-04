{
  config,
  inputs,
  ...
}: {
  config = {
    themes.base16 = {
      enable = true;
      path = "${inputs.base16-oxocarbon}/base16-oxocarbon-dark.yaml";
    };
  };
}

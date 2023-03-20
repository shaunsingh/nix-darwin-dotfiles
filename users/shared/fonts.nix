{ pkgs
, lib
, inputs
, config
, ...
}: {
  fonts.fonts = with pkgs; [
    sarasa-gothic
    sf-mono-liga-bin
    otf-apple
  ];
}

{ pkgs
, lib
, inputs
, config
, ...
}: {
  fonts.fontconfig = {
    enable = true;
    antialias = true;
    hinting = {
      enable = true;
      autohint = true;
      style = "hintfull";
    };
    subpixel.lcdfilter = "default";
    defaultFonts = {
      monospace = [ "Liga SFMono Nerd Font" ];
      sansSerif = [ "SF Pro Text" ];
      serif = [ "New York Medium" ];
    };
  };
}

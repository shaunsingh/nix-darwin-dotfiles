{ pkgs
, lib
, inputs
, config
, ...
}: {
  programs.kickoff = {
    enable = true;
    settings = {
      prompt = "λ  ";
      padding = 54;
      fonts = [ "Liga SFMono Nerd Font" ];
      font_size = 21.0;
      colors = with config.lib.base16.theme; {
        background = "#${baseDARK-hex}FF";
        prompt = "#${base0C-hex}FF";
        text = "#${base04-hex}FF";
        text_query = "#${base06-hex}FF";
        text_selected = "#${base08-hex}FF";
      };
    };
  };
}

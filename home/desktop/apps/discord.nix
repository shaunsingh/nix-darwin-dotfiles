{
  pkgs,
  lib,
  inputs,
  config,
  ...
}: {
  home.packages = with pkgs; [
    discocss
    (
      if pkgs.stdenv.hostPlatform.isDarwin
      then discord
      else webcord
    )
  ];
  xdg.configFile."discocss/custom.css".text = with config.lib.base16.theme; ''
    /* monospaced font */
    * { font-family: "Liga SFMono Nerd Font" !important; }
    /* themeing*/
    .theme-dark {
      --background-primary: #${base00-hex};
      --background-secondary: #${base01-hex};
      --background-tertiary: #${base03-hex};
      --background-secondary-alt: #${base02-hex};
      --channeltextarea-background: #${base01-hex};
      --interactive-muted: #${base0A-hex};
      --background-floating: #${base01-hex};
      --text-normal: #${base06-hex};
      --header-primary: #${base05-hex};
      --interactive-active: #${base0E-hex};
      --background-accent: #${base01-hex};
    }
    .theme-dark .container-1D34oG {
      background-color: #${base00-hex};
    }
    .categoryHeader-O1zU94, .theme-dark .autocomplete-1vrmpx {
      background-color: #${base00-hex};
    }
    .theme-dark .selected-1Tbx07 {
      background-color: #${base02-hex};
    }
    /* minimal looks*/
    [aria-label="Servers sidebar"],
    [class*="chat-"] > [class*="content-"]::before,
    [class*="repliedMessage-"]::before,
    ::-webkit-scrollbar,
    [class*="form-"] [class*="attachWrapper-"],
    [class*="form-"] [class*="buttons-"],
  '';
}

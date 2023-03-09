{
  pkgs,
  lib,
  inputs,
  config,
  ...
}: {
  programs.zathura = {
    enable = true;
    options = with config.lib.base16.theme; {
      completion-bg = "#${base02}";
      completion-fg = "#${base0C}";
      completion-highlight-bg = "#${base0C}";
      completion-highlight-fg = "#${base02}";
      default-bg = "#${base00}";
      default-fg = "#${base01}";
      highlight-active-color = "#${base0D}";
      highlight-color = "#${base0A}";
      index-active-bg = "#${base0D}";
      inputbar-bg = "#${base00}";
      inputbar-fg = "#${base05}";
      notification-bg = "#${base0B}";
      notification-error-bg = "#${base08}";
      notification-error-fg = "#${base00}";
      notification-fg = "#${base00}";
      notification-warning-bg = "#${base08}";
      notification-warning-fg = "#${base00}";
      recolor = true;
      recolor-darkcolor = "#${base06}";
      recolor-keephue = true;
      recolor-lightcolor = "#${base00}";
      selection-clipboard = "clipboard";
      statusbar-bg = "#${base01}";
    };
  };
}

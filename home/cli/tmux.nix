{ pkgs
, lib
, inputs
, config
, ...
}: {
  programs.tmux = {
    enable = true;
    sensibleOnTop = true;
    extraConfig = with config.lib.base16.theme; ''
      set -g status-right-length 100
      set -g status-left-length 100
      set -g status-left " "
      set -g status-right " "
      set -g status-justify left
      set -g status-style fg=black,bg=default
      set -g window-status-current-format "#[fg=#${base00-hex},bg=#${base0C-hex}] #I #[fg=#${base05-hex},bg=#${base01-hex}] [#W] #[fg=#${base03-hex},bg=#${base01-hex}]#{s|$HOME|~|;s|.*/||:pane_current_path} "
      set -g window-status-format "#[fg=#{base00-hex},bg=#{base0F-hex}] #I #[fg=#{base04-hex},bg=#{base01-hex}] [#W] #[fg=#{base03-hex},bg=#{base01-hex}]#{s|$HOME|~|;s|.*/||:pane_current_path} "
    '';
  };
}

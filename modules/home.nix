# shared between linux and macOS
{ pkgs
, lib
, inputs
, config
, ...
}: {
  home.stateVersion = "23.05";
  home.packages = with pkgs; [
    # editors 
    neovim-nightly
    tree-sitter

    # tooling 
    luajit
    fennel
    fnlfmt
    julia-bin
    inferno
    (rust-bin.selectLatestNightlyWith (toolchain:
      toolchain.default.override {
        extensions = [ "rust-src" ];
        targets = [ "arm-unknown-linux-gnueabihf" ];
      }))

    # utils
    fd
    zstd
    (ripgrep.override { withPCRE2 = true; })
  ];

  programs.fish = {
    enable = true;
    shellAliases = with pkgs; {
      ":q" = "exit";
      git-rebase = "git rebase -i HEAD~2";
      ll =
        "${pkgs.exa}/bin/exa -lF --color-scale --no-user --no-time --no-permissions --group-directories-first --icons -a";
      ls = "${pkgs.exa}/bin/exa -lF --group-directories-first --icons -a";
      nvim = "${pkgs.neovim-nightly}/bin/nvim --startuptime /tmp/nvim-startuptime";
    };
    shellInit = ''
      set fish_greeting
    '';
  };

  programs.bat.enable = true;
  programs.exa.enable = true;
  programs.zoxide.enable = true;

  programs.git = {
    enable = true;
    userName = "shaunsingh";
    userEmail = "shaunsingh0207@gmail.com";
    delta.enable = true;
    ignores =
      [ "**/.idea/" "**/.vscode/settings.json" "**/.direnv/" "**/.DS_Store" ];
    extraConfig = {
      pull = { ff = "only"; };
      init.defaultBranch = "main";
    };
  };

  programs.starship = {
    enable = true;
    settings = {
      scan_timeout = 10;
      # prompt
      format = "$directory$git_branch$git_metrics$nix_shell$package$character";
      add_newline = false;
      line_break.disabled = true;
      directory.style = "cyan";
      character = {
        success_symbol = "[λ](green)";
        error_symbol = "[λ](red)";
      };
      # git
      git_branch = {
        style = "purple";
        symbol = "";
      };
      git_metrics = {
        disabled = false;
        added_style = "bold yellow";
        deleted_style = "bold red";
      };
      # package management
      package.format = "version [$version](bold green) ";
      nix_shell.symbol = " ";
    };
  };

  programs.tmux = {
    enable = true;
    extraConfig = ''
      # make sure fish works in tmux
      set -g default-terminal "screen-256color"
      set -sa terminal-overrides ',xterm-256color:RGB'
      # so that escapes register immidiately in vim
      set -sg escape-time 1
      set -g focus-events on
      # mouse support
      set -g mouse on
      # change prefix to C-a
      set -g prefix C-a
      bind C-a send-prefix
      unbind C-b
      # extend scrollback
      set-option -g history-limit 5000
      # vim-like pane resizing
      bind -r C-k resize-pane -U
      bind -r C-j resize-pane -D
      bind -r C-h resize-pane -L
      bind -r C-l resize-pane -R
      # vim-like pane switching
      bind -r k select-pane -U
      bind -r j select-pane -D
      bind -r h select-pane -L
      bind -r l select-pane -R
      # styling
      set -g status-style fg=black,bg=default
      set -g status-left ""
      set -g status-right ""
      set -g status-justify centre
      set -g status-position bottom
      set -g pane-active-border-style bg=default,fg=default
      set -g pane-border-style fg=default
      set -g window-status-current-format "#[fg=cyan] #[fg=black]#[bg=cyan]#I #[bg=brightblack]#[fg=brightwhite] #W#[fg=brightblack]#[bg=default]  #[bg=default] #[fg=magenta] #[fg=white]#[bg=magenta]λ #[fg=black]#[bg=brightblack] %a %d %b #[fg=magenta]%R#[fg=brightblack]#[bg=default] "
      set -g window-status-format "#[fg=magenta] #[fg=black]#[bg=magenta]#I #[bg=brightblack]#[fg=brightwhite] #W#[fg=brightblack]#[bg=default]  "
    '';
  };

  # symlinks don't work with finder + spotlight, copy them instead
  disabledModules = [ "targets/darwin/linkapps.nix" ];
  home.activation = lib.mkIf pkgs.stdenv.isDarwin {
    copyApplications =
      let
        apps = pkgs.buildEnv {
          name = "home-manager-applications";
          paths = config.home.packages;
          pathsToLink = "/Applications";
        };
      in
      lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        baseDir="$HOME/Applications/Home Manager Apps"
        if [ -d "$baseDir" ]; then
          rm -rf "$baseDir"
        fi
        mkdir -p "$baseDir"
        for appFile in ${apps}/Applications/*; do
          target="$baseDir/$(basename "$appFile")"
          $DRY_RUN_CMD cp ''${VERBOSE_ARG:+-v} -fHRL "$appFile" "$baseDir"
          $DRY_RUN_CMD chmod ''${VERBOSE_ARG:+-v} -R +w "$target"
        done
      '';
  };
}

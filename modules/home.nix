{ pkgs, lib, config, home-manager, nix-darwin, inputs, ... }: {
  home.stateVersion = "21.11";
  home.packages = with pkgs; [
    # Formatting
    nixfmt

    # Editors (neovim)
    neovim-nightly
    fennel
    fnlfmt
    (rust-bin.selectLatestNightlyWith (toolchain:
      toolchain.default.override {
        extensions = [ "rust-src" ];
        targets = [ "arm-unknown-linux-gnueabihf" ];
      }))

    # Terminal utils
    discocss
    uutils-coreutils
    python3Full
    nodejs-16_x
  ];

  programs.firefox = {
    enable = true;
    package = pkgs.runCommand "firefox-0.0.0" { } "mkdir $out";
    extensions = with pkgs.nur.repos.rycee.firefox-addons; [
      add-custom-search-engine
      amp2html
      betterttv
      darkreader
      languagetool
      musescore-downloader
      reddit-enhancement-suite
      sponsorblock
      tree-style-tab
      tridactyl
      ublock-origin
      umatrix
    ];
  };

  programs.firefox.profiles = let
    userChrome = with config.lib.base16.theme; ''
      /*
       *  Hide window controls
       */
      .titlebar-buttonbox-container{
          display: none !important;
      }

      .titlebar-placeholder,
      #TabsToolbar .titlebar-spacer{ display: none; }
      #navigator-toolbox::after{ display: none !important; }


      /*
       *  Hide all the clutter in the navbar
       */
      #main-window :-moz-any(#back-button,
      		       #forward-button,
      		       #stop-reload-button,
      		       #home-button,
      		       #library-button,
      		       #sidebar-button,
      		       #star-button,
      		       #pocket-button,
      		       #permissions-granted-icon,
      		       #fxa-toolbar-menu-button,
      		       #_d7742d87-e61d-4b78-b8a1-b469842139fa_-browser-action, /* Vimium */
      		       #ublock0_raymondhill_net-browser-action) { display: none !important; }

      /*
       *  Hide tabs if only one tab
       */
      #titlebar .tabbrowser-tab[first-visible-tab="true"][last-visible-tab="true"]{
          display: none !important;
      }

      /*
       *  Minimal theme
       */
      #navigator-toolbox {
          font-family: 'Menlo' !important;
      }

      #navigator-toolbox toolbarspring {
          display: none;
      }

      /* Hide filler */
      #customizableui-special-spring2{
      	display:none;
      }

      .tab-background{
      	padding-bottom: 0 !important;
      }

      #navigator-toolbox #urlbar-container {
          padding: 0 !important;
          margin: 0 !important;
      }

      #navigator-toolbox #urlbar {
          border: none !important;
          border-radius: 0 !important;
          box-shadow: none !important;
      }

      #navigator-toolbox #PanelUI-button {
          padding: 0 !important;
          margin: 0 !important;
          border: none !important;
      }

      #navigator-toolbox #nav-bar {
          box-shadow: none !important;
      }

      #navigator-toolbox #pageActionButton {
          display: none;
      }

      #navigator-toolbox #pageActionSeparator {
          display: none;
      }

      #fullscr-toggler {
          height: 0 !important;
      }

      #navigator-toolbox .urlbar-history-dropmarker {
          display: none;
      }

      #navigator-toolbox #tracking-protection-icon-container {
          padding-right: 0 !important;
          border: none !important;
          display: none !important;
      }

      #navigator-toolbox .tab-close-button, #navigator-toolbox #tabs-newtab-button {
          display: none;
      }

      #navigator-toolbox #urlbar {
          padding: 0 !important;
          padding-left: 1ch !important;
          font-size: 13px;
      }

      #navigator-toolbox #urlbar-background {
          border: none !important;
          margin: 0 !important;
      }

      #navigator-toolbox .toolbarbutton-1 {
          width: 22px;
      }

      #navigator-toolbox .tabbrowser-tab {
          font-size: 12px
      }

      #navigator-toolbox .tab-background {
          box-shadow: none!important;
          border: none !important;
      }

      #navigator-toolbox .tabbrowser-tab::after {
          display: none !important;
      }

      #navigator-toolbox #urlbar-zoom-button {
          border: none !important;
      }

      #appMenu-fxa-container, #appMenu-fxa-container + toolbarseparator {
          display: none !important;
      }

      #sync-setup {
          display: none !important;
      }

      /*
       *  Hamburger menu to the left
       */

      #PanelUI-button {
          -moz-box-ordinal-group: 0;
          border-left: none !important;
          border-right: none !important;
          position: absolute;
      }

      #toolbar-context-menu .viewCustomizeToolbar {
          display: none !important;
      }

      :root[uidensity=compact] #PanelUI-button {
          margin-top: -30px;
      }

      #PanelUI-button {
          margin-top: -30px;
      }

      :root[uidensity=touch] #PanelUI-button {
          margin-top: -36px;
      }

      /*
       *  Tabs to the right of the urlbar
       */

      /* Modify these to change relative widths or default height */
      #navigator-toolbox{
          --uc-navigationbar-width: 40vw;
          --uc-toolbar-height: 40px;
      }
      /* Override for other densities */
      :root[uidensity="compact"] #navigator-toolbox{ --uc-toolbar-height: 30px; }
      :root[uidensity="touch"] #navigator-toolbox{ --uc-toolbar-height: 40px; }

      :root[uidensity=compact] #urlbar-container.megabar{
          --urlbar-container-height: var(--uc-toolbar-height) !important;
          padding-block: 0 !important;
      }
      :root[uidensity=compact] #urlbar.megabar{
          --urlbar-toolbar-height: var(--uc-toolbar-height) !important;
      }

      /* prevent urlbar overflow on narrow windows */
      /* Dependent on how many items are in navigation toolbar ADJUST AS NEEDED */
      @media screen and (max-width: 1300px){
          #urlbar-container{ min-width:unset !important }
      }

      #TabsToolbar{ margin-left: var(--uc-navigationbar-width); }
      #tabbrowser-tabs{ --tab-min-height: var(--uc-toolbar-height) !important; }

      /* This isn't useful when tabs start in the middle of the window */
      .titlebar-placeholder[type="pre-tabs"],
      .titlebar-spacer[type="pre-tabs"]{ display: none }

      #navigator-toolbox > #nav-bar{
          margin-right:calc(100vw - var(--uc-navigationbar-width));
          margin-top: calc(0px - var(--uc-toolbar-height));
      }

      /* Zero window drag space  */
      :root[tabsintitlebar="true"] #nav-bar{ padding-left: 0px !important; padding-right: 0px !important; }

      /* 1px margin on touch density causes tabs to be too high */
      .tab-close-button{ margin-top: 0 !important }

      /* Hide dropdown placeholder */
      #urlbar-container:not(:hover) .urlbar-history-dropmarker{ margin-inline-start: -30px; }

      /* Fix customization view */
      #customization-panelWrapper > .panel-arrowbox > .panel-arrow{ margin-inline-end: initial !important; }
    '';
    settings = {
      "browser.startup.homepage" = "https://prettycoffee.github.io/fluidity/";
      "browser.ctrlTab.recentlyUsedOrder" = false;
      "browser.newtabpage.enabled" = false;
      "browser.bookmarks.showMobileBookmarks" = true;
      "browser.uidensity" = 1;
      "browser.urlbar.placeholderName" = "Search Using SearXNG";
      "browser.urlbar.update1" = true;
      "privacy.trackingprotection.enabled" = true;
      "privacy.trackingprotection.socialtracking.enabled" = true;
      "privacy.trackingprotection.socialtracking.annotate.enabled" = true;
      "services.sync.declinedEngines" = "addons,prefs";
      "services.sync.engine.addons" = false;
      "services.sync.engineStatusChanged.addons" = true;
      "services.sync.engine.prefs" = false;
      "services.sync.engineStatusChanged.prefs" = true;
      "gfx.webrender.all" = true;
      "trim_on_minimize" = true;
      "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
    };
  in {
    home = {
      inherit settings;
      inherit userChrome;
      id = 0;
    };
  };

  programs.alacritty = {
    enable = true;
    package = pkgs.runCommand "alacritty-0.0.0" { } "mkdir $out";
    settings = with config.lib.base16.theme; {
      window.padding.x = 45;
      window.padding.y = 45;
      window.decorations = "buttonless";
      mouse.hide_when_typing = true;
      use_thin_strokes = true;
      cursor.style = "Beam";

      font = {
        size = 15;
        normal.family = "Liga SFMono Nerd Font";
        normal.style = "Light";
        bold.family = "Liga SFMono Nerd Font";
        bold.style = "Bold";
        italic.family = "Liga SFMono Nerd Font";
        italic.style = "Italic";
      };

      colors = {
        cursor.cursor = "#${base04-hex}";
        primary.background = "#${base00-hex}";
        primary.foreground = "#${base06-hex}";
        normal = {
          black = "#${base00-hex}";
          red = "#${base0B-hex}";
          green = "#${base0C-hex}";
          yellow = "#${base0D-hex}";
          blue = "#${base07-hex}";
          magenta = "#${base0F-hex}";
          cyan = "#${base09-hex}";
          white = "#${base04-hex}";
        };
        bright = {
          black = "#${base03-hex}";
          red = "#${base0B-hex}";
          green = "#${base0C-hex}";
          yellow = "#${base0D-hex}";
          blue = "#${base07-hex}";
          magenta = "#${base0F-hex}";
          cyan = "#${base09-hex}";
          white = "#${base06-hex}";
        };
      };
    };
  };

  xdg.configFile."wezterm/wezterm.lua".text = with config.lib.base16.theme; ''
    local wezterm = require 'wezterm'
    return {
      check_for_updates = false,
      font = wezterm.font 'Liga SFMono Nerd Font',
      font_size = 14.0,
      line_height = 1.1,
      enable_tab_bar = false,
      window_decorations = "RESIZE",
      window_padding = {
        left = 45,
        right = 45,
        top = 45,
        bottom = 45,
      },
      colors = {
        foreground = "#${base06-hex}",
        background = "#${base00-hex}",
        cursor_bg = "#${base06-hex}",
        cursor_fg = "#${base00-hex}",
        selection_fg = "#${base05-hex}",
        selection_bg = "#${base02-hex}",
        scrollbar_thumb = "#${base01-hex}",
        split = "#${base01-hex}",
        ansi = {
          "#${base00-hex}",
          "#${base0B-hex}",
          "#${base0C-hex}",
          "#${base0D-hex}",
          "#${base07-hex}",
          "#${base0E-hex}",
          "#${base09-hex}",
          "#${base04-hex}",
        },
        brights = {
          "#${base03-hex}",
          "#${base0B-hex}",
          "#${base0C-hex}",
          "#${base0D-hex}",
          "#${base08-hex}",
          "#${base0F-hex}",
          "#${base09-hex}",
          "#${base06-hex}",
        },
      },
    }
  '';

  programs.fish = {
    enable = true;
    shellAliases = with pkgs; {
      ":q" = "exit";
      git-rebase = "git rebase -i HEAD~2";
      ll =
        "${pkgs.exa}/bin/exa -lF --color-scale --no-user --no-time --no-permissions --group-directories-first --icons -a";
      ls = "${pkgs.exa}/bin/exa -lF --group-directories-first --icons -a";
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

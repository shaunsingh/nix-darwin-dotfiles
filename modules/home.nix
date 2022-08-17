{ pkgs, lib, config, home-manager, nix-darwin, inputs, ... }: {

  home.stateVersion = "21.11";
  home.packages = with pkgs; [
    # Formatting
    nixfmt

    # Editors (neovim)
    neovim-nightly
    fennel
    fnlfmt

    # Terminal utils and rust alternatives :tm:
    wezterm-git
    discocss
  ];

  programs.firefox = {
    enable = true;
    package = pkgs.runCommand "firefox-0.0.0" { } "mkdir $out";
    extensions = with pkgs.nur.repos.rycee.firefox-addons; [
      ublock-origin
      tridactyl
      reddit-enhancement-suite
    ];
  };

  programs.firefox.profiles =
    let
      userChrome = with config.lib.base16.theme; ''
        :root {
        --window-colour:                #${base01-hex};
        --secondary-colour:             #${base04-hex};
        --inverted-colour:              #${base05-hex};

        --uc-identity-color-blue:       #${base08-hex};
        --uc-identity-color-turquoise:  #${base09-hex};
        --uc-identity-color-green:      #${base0A-hex};
        --uc-identity-color-yellow:     #${base0B-hex};
        --uc-identity-color-orange:     #${base0C-hex};
        --uc-identity-color-red:        #${base0D-hex};
        --uc-identity-color-pink:       #${base0E-hex};
        --uc-identity-color-purple:     #${base0F-hex};

        /* URL colour in URL bar suggestions */
        --urlbar-popup-url-color: var(--uc-identity-color-purple) !important;

        /* global border radius */
        --uc-border-radius: 0;

        /* dynamic url bar width settings */
        --uc-urlbar-width: clamp(200px, 40vw, 500px);

        /* dynamic tab width settings */
        --uc-active-tab-width:   clamp(100px, 20vw, 300px);
        --uc-inactive-tab-width: clamp( 50px, 15vw, 200px);

        /* if active only shows the tab close button on hover*/
        --show-tab-close-button-hover: none; /* DEFAULT: -moz-inline-box; */

        /* adds left and right margin to the container-tabs indicator */
        --container-tabs-indicator-margin: 10px; }

        #back-button,
        #forward-button { display: none !important; }

        /* bookmark icon */
        #star-button{ display: none !important; }

        /* zoom indicator */
        #urlbar-zoom-button { display: none !important; }

        /* Make button small as Possible, hidden out of sight */
        #PanelUI-button { margin-top: -5px; margin-bottom: 44px; }

        #PanelUI-menu-button {
        padding: 0px !important;
        max-height: 1px;

        list-style-image: none !important;
        }

        #PanelUI-menu-button .toolbarbutton-icon { width: 1px !important; }
        #PanelUI-menu-button .toolbarbutton-badge-stack { padding: 0px !important; }

        #reader-mode-button{ display: none !important; }

        /* tracking protection shield icon */
        #tracking-protection-icon-container { display: none !important; }

        /* #identity-box { display: none !important } /* hides encryption AND permission items */
        #identity-permission-box { display: none !important; } /* only hodes permission items */

        /* e.g. playing indicator (secondary - not icon) */
        .tab-secondary-label { display: none !important; }

        #pageActionButton { display: none !important; }
        #page-action-buttons { display: none !important; }

        #urlbar-go-button { display: none !important; }

        :root {

        --uc-theme-colour:                          var(--window-colour);
        --uc-hover-colour:                          var(--secondary-colour);
        --uc-inverted-colour:                       var(--inverted-colour);

        --button-bgcolor:                           var(--uc-theme-colour)    !important;
        --button-hover-bgcolor:                     var(--uc-hover-colour)    !important;
        --button-active-bgcolor:                    var(--uc-hover-colour)    !important;

        --toolbar-bgcolor:                          var(--uc-theme-colour)    !important;
        --toolbarbutton-hover-background:           var(--uc-hover-colour)    !important;
        --toolbarbutton-active-background:          var(--uc-hover-colour)    !important;
        --toolbarbutton-border-radius:              var(--uc-border-radius)   !important;
        --lwt-toolbar-field-focus:                  var(--uc-theme-colour)    !important;
        --toolbarbutton-icon-fill:                  var(--uc-inverted-colour) !important;
        --toolbar-field-focus-background-color:     var(--secondary-colour)   !important;
        --toolbar-field-color:                      var(--uc-inverted-colour) !important;
        --toolbar-field-focus-color:                var(--uc-inverted-colour) !important;

        --tabs-border-color:                        var(--uc-theme-colour)    !important;
        --tab-border-radius:                        var(--uc-border-radius)   !important;
        --lwt-text-color:                           var(--uc-inverted-colour) !important;
        --lwt-tab-text:                             var(--uc-inverted-colour) !important;

        --lwt-sidebar-background-color:             var(--uc-hover-colour)    !important;
        --lwt-sidebar-text-color:                   var(--uc-inverted-colour) !important;

        --arrowpanel-border-color:                  var(--uc-theme-colour)    !important;
        --arrowpanel-border-radius:                 var(--uc-border-radius)   !important;
        --arrowpanel-background:                    var(--uc-theme-colour)    !important;
        --arrowpanel-color:                         var(--inverted-colour)    !important;

        --autocomplete-popup-highlight-background:  var(--uc-inverted-colour) !important;
        --autocomplete-popup-highlight-color:       var(--uc-inverted-colour) !important;
        --autocomplete-popup-hover-background:      var(--uc-inverted-colour) !important;


        --tab-block-margin: 2px !important;

        }

        window,
        #main-window,
        #toolbar-menubar,
        #TabsToolbar,
        #PersonalToolbar,
        #navigator-toolbox,
        #sidebar-box,
        #nav-bar {

        -moz-appearance: none !important;

        border: none !important;
        box-shadow: none !important;
        background: var(--uc-theme-colour) !important;

        }

        /* grey out ccons inside the toolbar to make it
        - more aligned with the Black & White colour look */
        #PersonalToolbar toolbarbutton:not(:hover),
        #bookmarks-toolbar-button:not(:hover) { filter: grayscale(1) !important; }


        /* remove window control buttons */
        .titlebar-buttonbox-container { display: none !important; }


        /* remove "padding" left and right from tabs */
        .titlebar-spacer { display: none !important; }

        /* remove gap after pinned tabs */
        #tabbrowser-tabs[haspinnedtabs]:not([positionpinnedtabs])
            > #tabbrowser-arrowscrollbox
            > .tabbrowser-tab[first-visible-unpinned-tab] { margin-inline-start: 0 !important; }


        /* remove tab shadow */
        .tabbrowser-tab
            >.tab-stack
            > .tab-background { box-shadow: none !important;  }


        /* tab background */
        .tabbrowser-tab
            > .tab-stack
            > .tab-background { background: var(--uc-theme-colour) !important; }


        /* active tab background */
        .tabbrowser-tab[selected]
            > .tab-stack
            > .tab-background { background: var(--uc-hover-colour) !important; }


        /* multi tab selection */
        #tabbrowser-tabs:not([noshadowfortests]) .tabbrowser-tab:is([visuallyselected=true], [multiselected]) > .tab-stack > .tab-background:-moz-lwtheme { background: var(--uc-hover-colour) !important; }


        /* tab close button options */
        .tabbrowser-tab:not([pinned]) .tab-close-button { display: var(--show-tab-close-button) !important; }
        .tabbrowser-tab:not([pinned]):hover .tab-close-button { display: var(--show-tab-close-button-hover) !important }


        /* adaptive tab width */
        .tabbrowser-tab[selected][fadein]:not([pinned]) { max-width: var(--uc-active-tab-width) !important; }
        .tabbrowser-tab[fadein]:not([selected]):not([pinned]) { max-width: var(--uc-inactive-tab-width) !important; }


        /* container tabs indicator */
        .tabbrowser-tab[usercontextid]
            > .tab-stack
            > .tab-background
            > .tab-context-line {

        margin: -1px var(--container-tabs-indicator-margin) 0 var(--container-tabs-indicator-margin) !important;

        border-radius: var(--tab-border-radius) !important;

        }


        /* show favicon when media is playing but tab is hovered */
        .tab-icon-image:not([pinned]) { opacity: 1 !important; }


        /* Makes the speaker icon to always appear if the tab is playing (not only on hover) */
        .tab-icon-overlay:not([crashed]),
        .tab-icon-overlay[pinned][crashed][selected] {

        top: 5px !important;
        z-index: 1 !important;

        padding: 1.5px !important;
        inset-inline-end: -8px !important;
        width: 16px !important; height: 16px !important;

        border-radius: 10px !important;

        }

        /* style and position speaker icon */
        .tab-icon-overlay:not([sharing], [crashed]):is([soundplaying], [muted], [activemedia-blocked]) {

        stroke: transparent !important;
        background: transparent !important;
        opacity: 1 !important; fill-opacity: 0.8 !important;

        color: currentColor !important;

        stroke: var(--uc-theme-colour) !important;
        background-color: var(--uc-theme-colour) !important;

        }


        /* change the colours of the speaker icon on active tab to match tab colours */
        .tabbrowser-tab[selected] .tab-icon-overlay:not([sharing], [crashed]):is([soundplaying], [muted], [activemedia-blocked]) {

        stroke: var(--uc-hover-colour) !important;
        background-color: var(--uc-hover-colour) !important;

        }


        .tab-icon-overlay:not([pinned], [sharing], [crashed]):is([soundplaying], [muted], [activemedia-blocked]) { margin-inline-end: 9.5px !important; }


        .tabbrowser-tab:not([image]) .tab-icon-overlay:not([pinned], [sharing], [crashed]) {

        top: 0 !important;

        padding: 0 !important;
        margin-inline-end: 5.5px !important;
        inset-inline-end: 0 !important;

        }


        .tab-icon-overlay:not([crashed])[soundplaying]:hover,
        .tab-icon-overlay:not([crashed])[muted]:hover,
        .tab-icon-overlay:not([crashed])[activemedia-blocked]:hover {

        color: currentColor !important;
        stroke: var(--uc-inverted-colour) !important;
        background-color: var(--uc-inverted-colour) !important;
        fill-opacity: 0.95 !important;

        }


        .tabbrowser-tab[selected] .tab-icon-overlay:not([crashed])[soundplaying]:hover,
        .tabbrowser-tab[selected] .tab-icon-overlay:not([crashed])[muted]:hover,
        .tabbrowser-tab[selected] .tab-icon-overlay:not([crashed])[activemedia-blocked]:hover {

        color: currentColor !important;
        stroke: var(--uc-inverted-colour) !important;
        background-color: var(--uc-inverted-colour) !important;
        fill-opacity: 0.95 !important;

        }


        /* speaker icon colour fix */
        #TabsToolbar .tab-icon-overlay:not([crashed])[soundplaying],
        #TabsToolbar .tab-icon-overlay:not([crashed])[muted],
        #TabsToolbar .tab-icon-overlay:not([crashed])[activemedia-blocked] { color: var(--uc-inverted-colour) !important; }


        /* speaker icon colour fix on hover */
        #TabsToolbar .tab-icon-overlay:not([crashed])[soundplaying]:hover,
        #TabsToolbar .tab-icon-overlay:not([crashed])[muted]:hover,
        #TabsToolbar .tab-icon-overlay:not([crashed])[activemedia-blocked]:hover { color: var(--uc-theme-colour) !important; }

        #nav-bar {

        border:     none !important;
        box-shadow: none !important;
        background: transparent !important;

        }


        /* remove border below whole nav */
        #navigator-toolbox { border-bottom: none !important; }


        #urlbar,
        #urlbar * {

        outline: none !important;
        box-shadow: none !important;

        }


        #urlbar-background { border: var(--uc-hover-colour) !important; }


        #urlbar[focused="true"]
            > #urlbar-background,
        #urlbar:not([open])
            > #urlbar-background { background: transparent !important; }


        #urlbar[open]
            > #urlbar-background { background: var(--uc-theme-colour) !important; }


        .urlbarView-row:hover
            > .urlbarView-row-inner,
        .urlbarView-row[selected]
            > .urlbarView-row-inner { background: var(--uc-hover-colour) !important; }


        /* transition to oneline */
        @media (min-width: 1000px) {


        /* move tabs bar over */
        #TabsToolbar { margin-left: var(--uc-urlbar-width) !important; }


        /* move entire nav bar  */
        #nav-bar { margin: calc((var(--urlbar-min-height) * -1) - 8px) calc(100vw - var(--uc-urlbar-width)) 0 0 !important; }


        } /* end media query */

        /* Container Tabs */
        .identity-color-blue      { --identity-tab-color: var(--uc-identity-color-blue)      !important; --identity-icon-color: var(--uc-identity-color-blue)      !important; }
        .identity-color-turquoise { --identity-tab-color: var(--uc-identity-color-turquoise) !important; --identity-icon-color: var(--uc-identity-color-turquoise) !important; }
        .identity-color-green     { --identity-tab-color: var(--uc-identity-color-green)     !important; --identity-icon-color: var(--uc-identity-color-green)     !important; }
        .identity-color-yellow    { --identity-tab-color: var(--uc-identity-color-yellow)    !important; --identity-icon-color: var(--uc-identity-color-yellow)    !important; }
        .identity-color-orange    { --identity-tab-color: var(--uc-identity-color-orange)    !important; --identity-icon-color: var(--uc-identity-color-orange)    !important; }
        .identity-color-red       { --identity-tab-color: var(--uc-identity-color-red)       !important; --identity-icon-color: var(--uc-identity-color-red)       !important; }
        .identity-color-pink      { --identity-tab-color: var(--uc-identity-color-pink)      !important; --identity-icon-color: var(--uc-identity-color-pink)      !important; }
        .identity-color-purple    { --identity-tab-color: var(--uc-identity-color-purple)    !important; --identity-icon-color: var(--uc-identity-color-purple)    !important; }
      '';
      settings = {
        "browser.startup.homepage" = "https://searx.tiekoetter.com/";
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
    in
    {
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
      window.decorations = "none";
      window.dynamic_title = true;
      live_config_reload = true;
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

  xdg.configFile."wezterm/wezterm.lua".text = with config.lib.base16.theme;
    ''
      local wezterm = require 'wezterm'

      return {
        font = wezterm.font 'Liga SFMono Nerd Font',
        font_size = 14.0,
        line_height = 1.1,
        enable_tab_bar = false,
        window_decorations = "NONE",
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
      ll = "${pkgs.exa}/bin/exa -lF --color-scale --no-user --no-time --no-permissions --group-directories-first --icons -a";
      ls = "${pkgs.exa}/bin/exa -lF --group-directories-first --icons -a";
    };
    shellInit = ''
      set fish_greeting
    '';
  };

  programs.bat.enable = true;
  programs.exa.enable = true;
  programs.zoxide.enable = true;

  # programs.nushell.enable = true;
  # home.file."Library/Application Support/org.nushell.nu/config.toml".text = ''
  #   skip_welcome_message = true
  #   disable_table_indexes = true
  #   table_mode = "none"
  #   prompt = "__zoxide_hook; STARSHIP_SHELL= starship prompt"
  #   startup = [
  #       "zoxide init nushell --hook prompt | save ~/.cache/zoxide/init.nu",
  #       "source ~/.cache/zoxide/init.nu",
  #       "def nuopen [arg, --raw (-r)] { if $raw { open -r $arg } else { open $arg } }
  #       "alias open = ^open"
  #       "alias ll = ${pkgs.exa}/bin/exa -lF --color-scale --no-user --no-time --no-permissions --group-directories-first --icons -a"
  #       "alias ls = ${pkgs.exa}/bin/exa -lF --group-directories-first --icons -a"
  #   ]
  #   
  #   [line_editor]
  #   keyseq_timeout_ms = 0
  #   edit_mode = "vi"
  # '';

  programs.git = {
    enable = true;
    userName = "shaunsingh";
    userEmail = "shaunsingh0207@gmail.com";
    delta = {
      enable = true;
      options = {
        syntax-theme = "Nord";
        line-numbers = true;
      };
    };
    ignores = [ ".dir-locals.el" ".envrc" ".DS_Store" ];
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

  xdg.configFile."discocss/custom.css".text = with config.lib.base16.theme;
    ''
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

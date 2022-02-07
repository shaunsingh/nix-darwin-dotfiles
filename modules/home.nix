{ pkgs, lib, config, home-manager, nix-darwin, inputs, ... }: {

home.stateVersion = "21.11";
home.packages = with pkgs; [
  # Language Servers
  nodePackages.pyright
  rust-analyzer

  # Formatting
  nixfmt
  black
  shellcheck

  # Terminal utils and rust alternatives :tm:
  xcp
  lsd
  procs
  tree
  zoxide
  bottom
  discocss
];

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

xdg.dataFile."discocss/custom.css".source = ../configs/custom.css;

programs.firefox = {
  enable = true;
  package = pkgs.runCommand "firefox-0.0.0" { } "mkdir $out";
  extensions = with pkgs.nur.repos.rycee.firefox-addons; [
    ublock-origin
    tridactyl
    reddit-enhancement-suite
  ];
};

programs.firefox.profiles = let
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
    window.decorations = "transparent";
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
        black =   "#${base00-hex}";
        red =     "#${base0B-hex}";
        green =   "#${base0C-hex}";
        yellow =  "#${base0D-hex}";
        blue =    "#${base07-hex}";
        magenta = "#${base0F-hex}";
        cyan =    "#${base09-hex}";
        white =   "#${base04-hex}";
      };
      bright = {
        black =   "#${base03-hex}";
        red =     "#${base0B-hex}";
        green =   "#${base0C-hex}";
        yellow =  "#${base0D-hex}";
        blue =    "#${base07-hex}";
        magenta = "#${base0F-hex}";
        cyan =    "#${base09-hex}";
        white =   "#${base06-hex}";
      };
    };
  };
};

programs.fish.enable = true;
programs.fish.shellAliases = with pkgs; {
    ":q" = "exit";
    vi = "emacsclient -c";
    git-rebase = "git rebase -i HEAD~2";
    ls = "lsd";
    ps = "ps";
    tree = "tree -a -C";
    cat = "bat";
    top = "btm";
    cp = "xcp";
    find = "fd";
  };

programs.fish.interactiveShellInit = ''
  set -g fish_greeting ""
  zoxide init fish --cmd cd | source
  set -x PATH ~/.config/emacs/bin $PATH

  set -g fish_greeting ""
  set -U fish_color_autosuggestion      brblack
  set -U fish_color_cancel              -r
  set -U fish_color_command             green
  set -U fish_color_comment             magenta
  set -U fish_color_cwd                 green
  set -U fish_color_cwd_root            red
  set -U fish_color_end                 magenta
  set -U fish_color_error               red
  set -U fish_color_escape              cyan
  set -U fish_color_history_current     --bold
  set -U fish_color_host                normal
  set -U fish_color_normal              normal
  set -U fish_color_operator            cyan
  set -U fish_color_param               blue
  set -U fish_color_quote               yellow
  set -U fish_color_redirection         yellow
  set -U fish_color_search_match        'yellow' '--background=brightblack'
  set -U fish_color_selection           'white' '--bold' '--background=brightblack'
  set -U fish_color_status              red
  set -U fish_color_user                green
  set -U fish_color_valid_path          --underline
  set -U fish_pager_color_completion    normal
  set -U fish_pager_color_description   yellow
  set -U fish_pager_color_prefix        'white' '--bold' '--underline'
  set -U fish_pager_color_progress      'white' '--background=cyan'

  # prompt
  set fish_prompt_pwd_dir_length 1
  set __fish_git_prompt_show_informative_status 1

  set fish_color_command green
  set fish_color_param $fish_color_normal

  set __fish_git_prompt_showdirtystate 'yes'
  set __fish_git_prompt_showupstream 'yes'

  set __fish_git_prompt_color_branch brown
  set __fish_git_prompt_color_dirtystate red
  set __fish_git_prompt_color_stagedstate yellow
  set __fish_git_prompt_color_upstream cyan
  set __fish_git_prompt_color_cleanstate green
  set __fish_git_prompt_color_invalidstate red

  set __fish_git_prompt_char_dirtystate ' ✕ '
  set __fish_git_prompt_char_stateseparator ' '
  set __fish_git_prompt_char_untrackedfiles '++'
  set __fish_git_prompt_char_cleanstate ' ✓ '
  set __fish_git_prompt_char_stagedstate '-> '
  set __fish_git_prompt_char_conflictedstate "✕ "

  set __fish_git_prompt_char_upstream_prefix ""
  set __fish_git_prompt_char_upstream_equal ""
  set __fish_git_prompt_char_upstream_ahead ' >= '
  set __fish_git_prompt_char_upstream_behind ' <= '
  set __fish_git_prompt_char_upstream_diverged ' <=> '

  function _print_in_color
    set -l string $argv[1]
    set -l color  $argv[2]

    set_color $color
    printf $string
    set_color normal
  end

  function _prompt_color_for_status
    if test $argv[1] -eq 0
      echo magenta
    else
      echo red
    end
  end

  function fish_prompt
      set -l last_status $status

      set -l nix_shell_info (
        if test -n "$IN_NIX_SHELL"
          echo -n " [nix-shell]"
        end
      )

      if test $HOME != $PWD
          _print_in_color ""(prompt_pwd) blue
      end
      __fish_git_prompt " (%s)"

      _print_in_color "$nix_shell_info λ " (_prompt_color_for_status $last_status) ]

  end
'';

programs.bat = {
  enable = true;
  config = { theme = "Nord"; };
};

programs.tmux.enable = true;
  programs.tmux.extraConfig = ''
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
}

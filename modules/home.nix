# Home.nix
# Home Manager allows you to use Nix’s declarative approach to manage your user-level configuration and packages. It works on any *nix system supported by Nix, including MacOS.

# [[file:../nix-config.org::*Home.nix][Home.nix:1]]
{ pkgs, lib, config, home-manager, nix-darwin, inputs, ... }: {
# Home.nix:1 ends here

# Hooks
# Ignore this, just some post-installation stuff here and there

# [[file:../nix-config.org::*Hooks][Hooks:1]]
system.activationScripts.postUserActivation.text = ''
  emacs --batch --eval "(progn (require 'org) (setq org-confirm-babel-evaluate nil) (org-babel-tangle-file \"nix-config.org\"))"
  emacs --batch --eval "(progn (require 'org) (setq org-confirm-babel-evaluate nil) (org-babel-tangle-file \"configs/doom/config.org\"))"
  ln -s ~/nix-darwin-dotfiles/configs/doom/ ~/.config
  if [ -d ~/.config/emacs ]; then
    git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.config/emacs
    ~/.config/emacs/bin/doom install
  fi
'';
# Hooks:1 ends here

# Doom-emacs
# Nix via doom-emacs is very, /very/ annoying. Initially I was using [[https://github.com/vlaci/nix-doom-emacs][Nix-doom-emacs]]. However, this has a few drawbacks
# 1. It doesn't support straight =:recipe=, so all packages must be from melpa or elpa
# 2. It pins the version of doom, so you need to update doom and its dependencies painstakingly manually
# 3. It just ends up breaking anyways.

# A simpler solution is just to have nix clone =doom-emacs= to =~/.config/emacs=, and the user can handle doom manually

# [[file:../nix-config.org::*Doom-emacs][Doom-emacs:1]]
#   home-manager.users.shauryasingh.home.file = {
#     "~/.config/doom" = {
#       recursive = true;
#       source = ../configs/doom;
#     };
#   };
# Doom-emacs:1 ends here

# Git
# As opposed to what the xcode CLT provides, I want lfs enabled with git, and use =delta= instead of the default diff tool (rust alternatives go brr). MacOS is also quite annoying with its =.DS_Store='s everywhere, so lets ignore that

# [[file:../nix-config.org::*Git][Git:1]]
home-manager.users.shauryasingh.programs.git = {
  enable = true;
  userName = "shaunsingh";
  userEmail = "shaunsingh0207@gmail.com";
  lfs.enable = true;
  delta = {
    enable = true;
    options = {
      syntax-theme = "Nord";
      line-numbers = true;

      width = 1;
      navigate = false;

      hunk-header-style = "file line-number syntax";
      hunk-header-decoration-style = "bold black";

      file-modified-label = "modified:";

      zero-style = "dim";

      minus-style = "bold red";
      minus-non-emph-style = "dim red";
      minus-emph-style = "bold red";
      minus-empty-line-marker-style = "normal normal";

      plus-style = "green normal bold";
      plus-non-emph-style = "dim green";
      plus-emph-style = "bold green";
      plus-empty-line-marker-style = "normal normal";

      whitespace-error-style = "reverse red";
    };
  };
  ignores = [ ".dir-locals.el" ".envrc" ".DS_Store" ];
};
# Git:1 ends here

# IdeaVim
# Intellij Idea ships with a very nice Vim emulation plugin. This is configured via a vimrc-like file (=~/.ideavimrc=). Since it doesn't have proper support in home-manger, we can just generate a file and symlink it into place

# [[file:../nix-config.org::*IdeaVim][IdeaVim:1]]
home-manager.users.shauryasingh.home.file = {
  ".ideavimrc".text = ''
    " settings
    set ignorecase
    set smartcase
    set scrolloff=3 " 3 lines above/below cursor when scrolling
    set nonumber
    set clipboard+=unnamed
    set multiple-cursors
    set numberwidth=2
    set expandtab=true
    set shiftwidth=4

    " plugins
    set easymotion
    set NERDTree
    set surround
    set highlightedyank


    " bindings
    let mapleader = " "
    nmap <leader>. :action GotoFile<cr>
    nmap <leader>fr :action RecentFiles<cr>
    nmap <leader>ww <Plug>(easymotion-w)
    nmap <leader>tz :action Enter Zen Mode<cr>
    nmap <leader>op :NERDTreeToggle<cr>
    nmap <leader>ot :Terminal<cr>
    nmap <leader>: :action SearchEverywhere<cr>
    nmap <leader>/ :action Find<cr>

    " use ; to enter command
    nmap ; :

    " use jk for escaping
    inoremap jk <Esc>
    cnoremap jk <Esc>

    " move by visual lines"
    nmap j gj
    nmap k gk

    " use C-hjkl to navigate splits
    nmap <C-h> <c-w>h
    nmap <C-l> <c-w>l
    nmap <C-k> <c-w>k
    nmap <C-j> <c-w>j

    nmap <leader>E :action Tool_External Tools_emacsclient<cr>
  '';
# IdeaVim:1 ends here

# Discocss
# [[https://github.com/mlvzk/discocss][Discocss]] is a way to inject custom CSS into discord. Similar to ideavim, it doesn't have proper support but we can generate a file for =~/.config/discocss/custom.css=

# [[file:../nix-config.org::*Discocss][Discocss:1]]
".config/discocss/custom.css".text = ''
    /*
        Discord Nord
        https://github.com/joinemm/discord-css
    */

    /* define colors */
    :root {
        --background-primary: #242730;
        --background-secondary: #2a2e38;
        --background-secondary-alt: #2a2e38;
        --background-tertiary: #242730;
        --background-accent: #242730;
        --channeltextarea-background: #242730;
        --text-normal: #6c605a;
        --text-muted: #ce9c85;
        --channels-default: #a09c80;
        --interactive-normal: #242730;
        --interactive-hover: #a09c80;
        --interactive-active: #a09c80;
        --interactive-muted: #665c54;
        --header-primary: #6c605a;
        --header-secondary: #5c605a;
        --background-floating: #242730;
        --scrollbar-auto-thumb: #1d2021;
        --scrollbar-auto-track: #3c3836;
        --text-link: #8f8678;
        --selection: #92837480;
    }

    * {
        font-family: Liga SFMono Nerd Font;
        font-size: 11px;
    }

    /* main backgrounds */
    .scrollerThemed-2oenus.themeDark-2cjlUp.scrollerTrack-1ZIpsv>.scroller-2FKFPG::-webkit-scrollbar-track,
    .scrollerThemed-2oenus.themeDark-2cjlUp.scrollerTrack-1ZIpsv>.scrollerStore-390omS::-webkit-scrollbar-track,
    .theme-dark .scrollerWrap-2lJEkd.scrollerTrack-1ZIpsv>.scroller-2FKFPG::-webkit-scrollbar-track,
    .theme-dark .scrollerWrap-2lJEkd.scrollerTrack-1ZIpsv>.scrollerStore-390omS::-webkit-scrollbar-track,
    .theme-dark .da-messageGroupWrapper,
    .theme-dark .bodyInner-245q0L,
    .theme-dark .bottomDivider-1K9Gao,
    .theme-dark .headerNormal-T_seeN,
    .theme-dark .root-1gCeng,
    .tabBody-3YRQ8W,
    .theme-dark .container-1D34oG
    .theme-dark .uploadModal-2ifh8j,
    .theme-dark .modal-yWgWj-,
    .uploadModal-2ifh8j,
    .theme-dark .emojiAliasInput-1y-NBz .emojiInput-1aLNse,
    .theme-dark .selected-1Tbx07,
    .theme-dark .option-96V44q.selected-rZcOL- {
        background-color: var(--background-primary) !important;
    }

    .da-popouts .da-container,
    .da-friendsTableHeader,
    .da-friendsTable,
    .da-autocomplete,
    .da-themedPopout,
    .da-footer,
    .da-userPopout>*,
    .da-autocompleteHeaderBackground,
    .theme-dark .bar-2Qqk5Z,
    .theme-dark .payment-xT17Mq,
    .theme-dark .paymentPane-3bwJ6A,
    .theme-dark .paginator-166-09,
    .theme-dark .codeRedemptionRedirect-1wVR4b,
    .theme-dark .scrollerThemed-2oenus.themedWithTrack-q8E3vB .scroller-2FKFPG::-webkit-scrollbar-thumb,
    .theme-dark .footer-3mqk7D,
    .theme-dark .footer-2gL1pp,
    .scrollerThemed-2oenus.themeDark-2cjlUp .scroller-2FKFPG::-webkit-scrollbar-thumb,
    .theme-dark .scrollerWrap-2lJEkd .scroller-2FKFPG::-webkit-scrollbar-thumb,
    .theme-dark .inset-3sAvek,
    .theme-dark .quickMessage-1yeL4E,
    .wrapper-2aW0bm,
    .theme-dark .autocomplete-1vrmpx,
    .searchBar-3dMhjb,
    .theme-dark .body-3iLsc4,
    .theme-dark .footer-2gL1pp,
    .theme-dark .footer-1fjuF6,
    .cardPrimary-1Hv-to,
    .theme-dark .card-FDVird:before,
    .theme-dark .colorPickerCustom-2CWBn2 {
        background-color: var(--background-secondary);
    }

    /* scrollbars */
    .da-messagesWrapper .da-scroller::-webkit-scrollbar,
    .da-messagesWrapper .da-scroller::-webkit-scrollbar-track-piece {
        background-color: var(--background-tertiary) !important;
        border-color: rgba(0, 0, 0, 0) !important;
    }

    .da-scrollerThemed .da-scroller::-webkit-scrollbar-thumb,
    .da-scrollerWrap .da-scroller::-webkit-scrollbar-thumb {
        background-color: var(--background-secondary) !important;
        border-color: var(--background-tertiary) !important;
    }

    .theme-dark .scrollerThemed-2oenus.themedWithTrack-q8E3vB .scroller-2FKFPG::-webkit-scrollbar-track-piece {
        background-color: var(--background-primary) !important;
        border-color: rgba(0, 0, 0, 0) !important;
    }

    .theme-dark .selectorSelected-1_M1WV,
    .newMessagesBar-mujexs,
    .theme-dark .searchAnswer-3Dz2-q,
    .theme-dark .searchFilter-2ESiM3,
    .theme-dark .progress-1IcQ3A,
    .themeDefault-24hCdX,
    .theme-dark .lookFilled-1Gx00P.colorPrimary-3b3xI6 {
        background-color: var(--background-accent);
    }

    .theme-dark .container-3ayLPN {
        background-color: var(--background-tertiary);
    }

    .theme-dark .option-96V44q.selected-rZcOL- {
        background-color: var(--background-floating);
    }

    .theme-dark .pageIndicator-1gAbyA,
    .theme-dark .pageButtonNext-V2kUq0,
    .theme-dark .pageButtonPrev-1Y-47D {
        border-color: var(--background-accent);
    }

    .scroller-2FKFPG::-webkit-scrollbar,
    .barButtonBase-3UKlW2,
    .theme-dark .scrollerThemed-2oenus.themeGhostHairline-DBD-2d .scroller-2FKFPG::-webkit-scrollbar-thumb {
        background: transparent;
    }

    /* remove gradients */
    .theme-dark .da-option:after, .theme-dark .option-96V44q:after {
        background-image: none !important;
    }

    /* search text */
    .theme-dark .searchOption-zQ-1l6 .answer-1n6g43,
    .theme-dark .option-96V44q .filter-3Y_im- {
        color: var(--text-muted)
    }

    .theme-dark .searchOption-zQ-1l6 .filter-3Y_im- {
        color: var(--text-normal)
    }

    /* side panel bottom section border */
    .panels-j1Uci_ {
        border-top: 2px solid var(--background-modifier-accent);
    }

    .container-1giJp5 {
        border-width: 2px;
    }

    /* selected text */
    ::selection {
        background: var(--selection);
    }

    /* hide that stupid nitro gift button */
    .button-38aScr[aria-label="Send a gift"] {
        display: none;
    }

    /* hide blocked messages */
    [class="groupStart-23k01U"] {
      display: none;
    }

    /* enhanceddiscord server count text */
    .theme-dark .keybind-KpFkfr {
        color: var(--channels-default)
    }

    /* unloaded emojis */
    .theme-dark .imageLoading-bpSr0M {
        background-image: none;
        background: var(--background-primary);
        border-radius: 50%;
    }

    .sprite-2iCowe {
      filter: none !important;
    }

    /* Doom style (dark) (c) Pavel Pertsev (original style at https://github.com/morhetz/gruvbox) */

    .hljs {
      display: block;
      overflow-x: auto;
      padding: 0.5em;
      background: #242730;
    }

    .hljs,
    .hljs-subst {
      color: #ebdbb2;
    }

    /* Doom Red */
    .hljs-deletion,
    .hljs-formula,
    .hljs-keyword,
    .hljs-link,
    .hljs-selector-tag {
      color: #fb4934;
    }

    /* Doom Blue */
    .hljs-built_in,
    .hljs-emphasis,
    .hljs-name,
    .hljs-quote,
    .hljs-strong,
    .hljs-title,
    .hljs-variable {
      color: #83a598;
    }

    /* Doom Yellow */
    .hljs-attr,
    .hljs-params,
    .hljs-template-tag,
    .hljs-type {
      color: #fabd2f;
    }

    /* Doom Purple */
    .hljs-builtin-name,
    .hljs-doctag,
    .hljs-literal,
    .hljs-number {
      color: #8f3f71;
    }

    /* Doom Orange */
    .hljs-code,
    .hljs-meta,
    .hljs-regexp,
    .hljs-selector-id,
    .hljs-template-variable {
      color: #fe8019;
    }

    /* Doom Green */
    .hljs-addition,
    .hljs-meta-string,
    .hljs-section,
    .hljs-selector-attr,
    .hljs-selector-class,
    .hljs-string,
    .hljs-symbol {
      color: #b8bb26;
    }

    /* Doom Aqua */
    .hljs-attribute,
    .hljs-bullet,
    .hljs-class,
    .hljs-function,
    .hljs-function .hljs-keyword,
    .hljs-meta-keyword,
    .hljs-selector-pseudo,
    .hljs-tag {
      color: #8ec07c;
    }

    /* Doom Gray */
    .hljs-comment {
      color: #928374;
    }

    /* Doom Purple */
    .hljs-link_label,
    .hljs-literal,
    .hljs-number {
      color: #d3869b;
    }
    .hljs-comment,
    .hljs-emphasis {
      font-style: italic;
    }

    .hljs-section,
    .hljs-strong,
    .hljs-tag {
      font-weight: bold;
    }

    /* Auto Hide by \xynstr#0300 */
        /* Transition */
        .sidebar-2K8pFh:hover {
            width: 240px !important;
            transition: 0.1s ease !important;
            transition-delay: 0.3s !important;
        }
        /* Detects screen size to show channels if screen is big enough */
        @media screen and (max-width: 1100px) {
            .sidebar-2K8pFh {
                width: 0px !important;
                transition: 0.3s ease !important;
                position: fixed !important;
                height: calc(100% - 47.988px) !important;
                z-index: 1 !important;
                bottom: 0px !important;
            }
            .wrapper-3NnKdC:hover + .base-3dtUhz > .content-98HsJk > .sidebar-2K8pFh {
                width: 240px !important;
            }
            .sidebar-2K8pFh:hover {
                width: 240px !important;
  '';
};
# Discocss:1 ends here

# Firefox
# Although safari is my main browser, firefox looks very appealing with its excellent privacy and speed

# [[file:../nix-config.org::*Firefox][Firefox:1]]
home-manager.users.shauryasingh.programs.firefox.enable = true;
# Firefox:1 ends here



# GUI apps are very finicky with nix, and so I create a fake package so that we can still use the configuration from =home-manager= without having to install it via nix. The user can then install firefox manually to =~/Applications=

# [[file:../nix-config.org::*Firefox][Firefox:2]]
home-manager.users.shauryasingh.programs.firefox.package =
  pkgs.runCommand "firefox-0.0.0" { } "mkdir $out";
home-manager.users.shauryasingh.programs.firefox.extensions =
    with pkgs.nur.repos.rycee.firefox-addons; [
      ublock-origin
      tridactyl
      duckduckgo-privacy-essentials
      reddit-enhancement-suite
      betterttv
    ];
# Firefox:2 ends here



# Now for the configuration. We want firefox to use the css at [[./configs/userChrome.css]], and we want to configure the UI. Lets also enable the (rust powered ftw) webrender/servo renderer.

# [[file:../nix-config.org::*Firefox][Firefox:3]]
home-manager.users.shauryasingh.programs.firefox.profiles = let
  userChrome = builtins.readFile ../configs/userChrome.css;
  settings = {
    "app.update.auto" = true;
    "browser.startup.homepage" = "https://tilde.cade.me";
    "browser.search.region" = "US";
    "browser.search.countryCode" = "US";
    "browser.ctrlTab.recentlyUsedOrder" = false;
    "browser.newtabpage.enabled" = false;
    "browser.bookmarks.showMobileBookmarks" = true;
    "browser.uidensity" = 1;
    "browser.urlbar.placeholderName" = "SearX";
    "browser.urlbar.update1" = true;
    "identity.fxaccounts.account.device.name" = config.networking.hostName;
    "privacy.trackingprotection.enabled" = true;
    "privacy.trackingprotection.socialtracking.enabled" = true;
    "privacy.trackingprotection.socialtracking.annotate.enabled" = true;
    "reader.color_scheme" = "sepia";
    "services.sync.declinedEngines" = "addons,passwords,prefs";
    "services.sync.engine.addons" = false;
    "services.sync.engineStatusChanged.addons" = true;
    "services.sync.engine.passwords" = false;
    "services.sync.engine.prefs" = false;
    "services.sync.engineStatusChanged.prefs" = true;
    "signon.rememberSignons" = false;
    "gfx.webrender.all" = true;
    "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
  };
in {
  home = {
    inherit settings;
    inherit userChrome;
    id = 0;
  };
};
# Firefox:3 ends here

# Alacritty
# Alacritty is my terminal emulator of choice. Similar to firefox, we want to create a fake package, and then configure it as normal

# [[file:../nix-config.org::*Alacritty][Alacritty:1]]
home-manager.users.shauryasingh.programs.alacritty = {
  enable = true;
  # We need to give it a dummy package
  package = pkgs.runCommand "alacritty-0.0.0" { } "mkdir $out";
  settings = {
    window.padding.x = 45;
    window.padding.y = 45;
    window.decorations = "buttonless";
    window.dynamic_title = true;
    live_config_reload = true;
    mouse.hide_when_typing = true;
    use_thin_strokes = true;
    cursor.style = "Beam";

    font = {
      size = 14;
      normal.family = "Liga SFMono Nerd Font";
      normal.style = "Light";
      bold.family = "Liga SFMono Nerd Font";
      bold.style = "Bold";
      italic.family = "Liga SFMono Nerd Font";
      italic.style = "Italic";
    };

    colors = {
      cursor.cursor = "#bbc2cf";
      primary.background = "#242730";
      primary.foreground = "#bbc2cf";
      normal = {
        black =   "#2a2e38";
        red =     "#ff665c";
        green =   "#7bc275";
        yellow =  "#FCCE7B";
        blue =    "#5cEfFF";
        magenta = "#C57BDB";
        cyan =    "#51afef";
        white =   "#bbc2cf";
      };
      bright = {
        black =    "#484854";
        red =      "#ff665c";
        green =    "#7bc275";
        yellow =   "#fcce7b";
        blue =     "#5cefff";
        magenta =  "#c57bdb";
        cyan =     "#51afef";
        white =    "#bbc2cf";
      };
    };
  };
};
# Alacritty:1 ends here

# Kitty
# I no longer use kitty (its quite slow to start and has too many features I don't need), but I keep the config around just in case

# [[file:../nix-config.org::*Kitty][Kitty:1]]
# home-manager.users.shauryasingh.programs.kitty = {
#   enable = true;
#   package = builtins.path {
#     path = /Applications/kitty.app/Contents/MacOS;
#     filter = (path: type: type == "directory" || builtins.baseNameOf path == "kitty");
#   };
#   # enable = true;
#   settings = {
#     font_family = "Liga SFMono Nerd Font";
#     font_size = "14.0";
#     adjust_line_height = "120%";
#     disable_ligatures = "cursor";
#     hide_window_decorations = "yes";
#     scrollback_lines = "50000";
#     cursor_blink_interval = "0.5";
#     cursor_stop_blinking_after = "10.0";
#     window_border_width = "0.7pt";
#     draw_minimal_borders = "yes";
#     macos_option_as_alt = "no";
#     cursor_shape = "beam";

#     foreground           =   "#D8DEE9";
#     background           =   "#2E3440";
#     selection_foreground =   "#000000";
#     selection_background =   "#FFFACD";
#     url_color            =   "#0087BD";
#     cursor               =   "#81A1C1";
#     color0               =   "#3B4252";
#     color8               =   "#4C566A";
#     color1               =   "#BF616A";
#     color9               =   "#BF616A";
#     color2               =   "#A3BE8C";
#     color10              =   "#A3BE8C";
#     color3               =   "#EBCB8B";
#     color11              =   "#EBCB8B";
#     color4               =   "#81A1C1";
#     color12              =   "#81A1C1";
#     color5               =   "#B48EAD";
#     color13              =   "#B48EAD";
#     color6               =   "#88C0D0";
#     color14              =   "#8FBCBB";
#     color7               =   "#E5E9F0";
#     color15              =   "#B48EAD";
#   };
# };
# Kitty:1 ends here

# Fish
# I like to use the fish shell. Although it isn't POSIX, it has the best autocompletion and highlighting I've seen.

# [[file:../nix-config.org::*Fish][Fish:1]]
programs.fish.enable = true;
environment.shells = with pkgs; [ fish ];
users.users.shauryasingh = {
  home = "/Users/shauryasingh";
  shell = pkgs.fish;
};
# Fish:1 ends here

# Settings fish as default
# On macOS nix doesn't set the fish shell to the main shell by default (like it does on NixOS), so lets do that manually

# [[file:../nix-config.org::*Settings fish as default][Settings fish as default:1]]
system.activationScripts.postActivation.text = ''
  # Set the default shell as fish for the user
  sudo chsh -s ${lib.getBin pkgs.fish}/bin/fish shauryasingh
'';
# Settings fish as default:1 ends here

# Aliases
# I also like to alias common commands with other, better rust alternatives :tm:

# [[file:../nix-config.org::*Aliases][Aliases:1]]
programs.fish.shellAliases = with pkgs; {
  ":q" = "exit";
  vi = "emacsclient -c";
  git-rebsae = "git rebase -i HEAD~2";
  ll =
    "exa -lF --color-scale --no-user --no-time --no-permissions --group-directories-first --icons -a";
  ls = "exa -lF --group-directories-first --icons -a";
  ps = "ps";
  tree = "tree -a -C";
  cat = "bat";
  top = "btm";
  cp = "xcp";
  find = "fd";
  calc = "emacs -f full-calc";
  neovide =
    "/Applications/Neovide.app/Contents/MacOS/neovide --frameless --multigrid";
  nix-fish = "nix-shell --command fish";
};
# Aliases:1 ends here

# Prompt
# I like to make my prompt look pretty (along with some =nix-shell= and =git= integration)

# [[file:../nix-config.org::*Prompt][Prompt:1]]
programs.fish.promptInit = ''

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
  set __fish_git_prompt_color_dirtystate FCBC47
  set __fish_git_prompt_color_stagedstate yellow
  set __fish_git_prompt_color_upstream cyan
  set __fish_git_prompt_color_cleanstate green
  set __fish_git_prompt_color_invalidstate red

  set __fish_git_prompt_char_dirtystate '~~'
  set __fish_git_prompt_char_stateseparator ' '
  set __fish_git_prompt_char_untrackedfiles ' ...'
  set __fish_git_prompt_char_cleanstate '✓'
  set __fish_git_prompt_char_stagedstate '-> '
  set __fish_git_prompt_char_conflictedstate "✕"

  set __fish_git_prompt_char_upstream_prefix ""
  set __fish_git_prompt_char_upstream_equal ""
  set __fish_git_prompt_char_upstream_ahead '>>='
  set __fish_git_prompt_char_upstream_behind '=<<'
  set __fish_git_prompt_char_upstream_diverged '<=>'

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
# Prompt:1 ends here

# Init
# I also want to disable the default greeting, and use tmux with fish. Lets also set =nvim= as the default editor, and add emacs to my path

# [[file:../nix-config.org::*Init][Init:1]]
programs.fish.interactiveShellInit = ''
  set -g fish_greeting ""
  if not set -q TMUX
    tmux new-session -A -s main
  end

  zoxide init fish --cmd cd | source

  set -x EDITOR "nvim"
  set -x PATH ~/.config/emacs/bin $PATH
'';
# Init:1 ends here

# Neovim
# Lastly, I didn't feel like nix-ifying my neovim lua config. Lets cheat a bit and just symlink it instead

# [[file:../nix-config.org::*Neovim][Neovim:1]]
home-manager.users.shauryasingh.programs.neovim = {
  enable = true;
  package = pkgs.neovim-nightly;
  vimAlias = true;
  extraPackages = with pkgs; [
      tree-sitter
      # neovide-git
      nodejs
      tree-sitter
  ];
  extraConfig = builtins.concatStringsSep "\n" [
    ''
      lua << EOF
      ${lib.strings.fileContents ../configs/nvim/nix.lua}
      EOF
    ''
  ];
};
# Neovim:1 ends here

# Bat
# Bat is another rust alternative :tm: to cat, and provides syntax highlighting. Lets theme it to match nord

# [[file:../nix-config.org::*Bat][Bat:1]]
home-manager.users.shauryasingh.programs.bat = {
  enable = true;
  config = { theme = "Nord"; };
};
# Bat:1 ends here

# Tmux
# Lastly, lets make tmux look just as pretty as our prompt, and enable truecolor support.

# [[file:../nix-config.org::*Tmux][Tmux:1]]
programs.tmux.enable = true;
  programs.tmux.extraConfig = ''
    # make sure fish works in tmux
    set -g  default-terminal   "xterm-256color"
    set -sa terminal-overrides ',xterm-256color:RGB'
    # so that escapes register immidiately in vim
    set -sg escape-time 1
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
    set -g status-style fg=white,bg=default
    set -g status-left ""
    set -g status-right ""
    set -g status-justify centre
    set -g status-position bottom
    set -g pane-active-border-style bg=default,fg=default
    set -g pane-border-style fg=default
    set -g window-status-current-format "#[fg=cyan]#[fg=black]#[bg=cyan]#I #[bg=brightblack]#[fg=white] #W#[fg=brightblack]#[bg=default] #[bg=default] #[fg=magenta]#[fg=black]#[bg=magenta]λ #[fg=white]#[bg=brightblack] %a %d %b #[fg=magenta]%R#[fg=brightblack]#[bg=default]"
    set -g window-status-format "#[fg=magenta]#[fg=black]#[bg=magenta]#I #[bg=brightblack]#[fg=white] #W#[fg=brightblack]#[bg=default] "
  '';
}
# Tmux:1 ends here

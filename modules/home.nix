{ pkgs, lib, config, home-manager, nix-darwin, ... }: {
  home-manager.users.shauryasingh.programs.git = {
    package = pkgs.gitFull;
    enable = true;
    userName = "shaunsingh";
    userEmail = "shaunsingh0207@gmail.com";
    lfs.enable = true;
    delta = {
      enable = true;
      options = { syntax-theme = "Nord"; };
    };
    ignores = [ ".dir-locals.el" ".envrc" ".DS_Store" ];
  };
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
    ".config/discocss/custom.css".text = ''
      /*
          Discord Nord
          https://github.com/joinemm/discord-css
      */

      /* define colors */
      :root {
          --background-primary: #2E3440;
          --background-secondary: #3B4252;
          --background-secondary-alt: #3B4252;
          --background-tertiary: #2E3440;
          --background-accent: #2E3440;
          --channeltextarea-background: #2E3440;
          --text-normal: #D8DEE9;
          --text-muted: #616E88;
          --channels-default: #616E88;
          --interactive-normal: #616E88;
          --interactive-hover: #8FBCBB;
          --interactive-active: #D8DEE9;
          --interactive-muted: #81A1C1;
          --header-primary: #D8DEE9;
          --header-secondary: #8FBCBB;
          --background-floating: #2E3440;
          --scrollbar-auto-thumb: #3B4252;
          --scrollbar-auto-track: #2E3440;
          --text-link: #88C0D0;
          --selection: #3B4252;
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

      /* Nord style (dark) */

      .hljs {
        display: block;
        overflow-x: auto;
        padding: 0.5em;
        background: #2E3440;
      }

      .hljs,
      .hljs-subst {
        color: #616E88;
      }

      /* Nord Red */
      .hljs-deletion,
      .hljs-formula,
      .hljs-keyword,
      .hljs-link,
      .hljs-selector-tag {
        color: #BF616A;
      }

      /* Nord Blue */
      .hljs-built_in,
      .hljs-emphasis,
      .hljs-name,
      .hljs-quote,
      .hljs-strong,
      .hljs-title,
      .hljs-variable {
        color: #E8CB8B;
      }

      /* Nord Yellow */
      .hljs-attr,
      .hljs-params,
      .hljs-template-tag,
      .hljs-type {
        color: #81A1C1;
      }

      /* Nord Purple */
      .hljs-builtin-name,
      .hljs-doctag,
      .hljs-literal,
      .hljs-number {
        color: #Bf616A;
      }

      /* Nord Orange */
      .hljs-code,
      .hljs-meta,
      .hljs-regexp,
      .hljs-selector-id,
      .hljs-template-variable {
        color: #D08770;
      }

      /* Nord Green */
      .hljs-addition,
      .hljs-meta-string,
      .hljs-section,
      .hljs-selector-attr,
      .hljs-selector-class,
      .hljs-string,
      .hljs-symbol {
        color: #A3Be8C;
      }

      /* Nord Aqua */
      .hljs-attribute,
      .hljs-bullet,
      .hljs-class,
      .hljs-function,
      .hljs-function .hljs-keyword,
      .hljs-meta-keyword,
      .hljs-selector-pseudo,
      .hljs-tag {
        color: #E8CB8B;
      }

      /* Nord Gray */
      .hljs-comment {
        color: #616E88;
      }

      /* Nord Purple */
      .hljs-link_label,
      .hljs-literal,
      .hljs-number {
        color: #B48EAD;
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
  # home-manager.users.shauryasingh.home.file."~/.config/nvim".source = config.lib.file.mkOutOfStoreSymlink ../configs/nvim;
  home-manager.users.shauryasingh.programs.htop.settings = {
    color_scheme = 0;
    cpu_count_from_one = 0;
    delay = 15;
    tree_view = 1;
    show_cpu_usage = 1;
    hide_function_bar = 2;
    highlight_base_name = 0;
    highlight_megabytes = 1;
    highlight_threads = 1;
    highlight_changes = 0;
    highlight_changes_delay_secs = 5;
  };
  home-manager.users.shauryasingh.programs.alacritty = {
    enable = true;
    package = builtins.path {
      path = /Applications/Alacritty.app/Contents/MacOS;
      filter = (path: type:
        type == "directory" || builtins.baseNameOf path == "alacritty");
    };
    settings = {
      window.padding.x = 45;
      window.padding.y = 45;
      window.decorations = "none";
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
        cursor.cursor = "#81a1c1";
        primary.background = "#2e3440";
        primary.foreground = "#d8dee9";
        normal = {
          black = "#3B4252";
          red = "#BF616A";
          green = "#A3BE8C";
          yellow = "#EBCB8B";
          blue = "#81A1C1";
          magenta = "#B48EAD";
          cyan = "#88C0D0";
          white = "#E5E9F0";
        };
        bright = {
          black = "#4c566a";
          red = "#bf616a";
          green = "#a3be8c";
          yellow = "#ebcb8b";
          blue = "#81a1c1";
          magenta = "#b48ead";
          cyan = "#8fbcbb";
          white = "#eceff4";
        };
      };
    };
  };
  # home-manager.users.shauryasingh.programs.kitty = {
  #   enable = true;
  #   package = builtins.path {
  #     path = /Applications/kitty.app/Contents/MacOS;
  #     filter = (path: type: type == "directory" || builtins.baseNameOf path == "kitty");
  #   };
  #   # enable = true;
  #   settings = {
  #   font_family = "Liga SFMono Nerd Font";
  #   	font_size = "14.0";
  #   	adjust_line_height = "120%";
  #   	disable_ligatures = "cursor";
  #   	hide_window_decorations = "yes";
  #   	scrollback_lines = "50000";
  #   	cursor_blink_interval = "0.5";
  #   	cursor_stop_blinking_after = "10.0";
  #   	window_border_width = "0.7pt";
  #   	draw_minimal_borders = "yes";
  #   	macos_option_as_alt = "no";
  #   	cursor_shape = "beam";

  #   	foreground           =   "#D8DEE9";
  #   	background           =   "#2E3440";
  #   	selection_foreground =   "#000000";
  #   	selection_background =   "#FFFACD";
  #   	url_color            =   "#0087BD";
  #   	cursor               =   "#81A1C1";
  #   	color0               =   "#3B4252";
  #   	color8               =   "#4C566A";
  #   	color1               =   "#BF616A";
  #   	color9               =   "#BF616A";
  #   	color2               =   "#A3BE8C";
  #   	color10              =   "#A3BE8C";
  #   	color3               =   "#EBCB8B";
  #   	color11              =   "#EBCB8B";
  #   	color4               =   "#81A1C1";
  #   	color12              =   "#81A1C1";
  #   	color5               =   "#B48EAD";
  #   	color13              =   "#B48EAD";
  #   	color6               =   "#88C0D0";
  #   	color14              =   "#8FBCBB";
  #   	color7               =   "#E5E9F0";
  #   	color15              =   "#B48EAD";
  #   };
  # };
  programs.fish.enable = true;
  environment.shells = with pkgs; [ fish ];
  users.users.shauryasingh = {
    home = "/Users/shauryasingh";
    shell = pkgs.fish;
  };
  system.activationScripts.postActivation.text = ''
    # Set the default shell as fish for the user
    sudo chsh -s ${lib.getBin pkgs.fish}/bin/fish shauryasingh 
  '';
  programs.fish.shellAliases = with pkgs; {
    ":q" = "exit";
    vi = "emacsclient -c";
    git-rebsae = "git rebase -i HEAD~2";
    ll = "exa -lF --color-scale --no-user --no-time --no-permissions --group-directories-first --icons -a";
    ls = "exa -lF --group-directories-first --icons -a";
    tree = "tree -a -C";
    cat = "bat";
    find = "fd";
    sed = "sd";
    neovide = "/Applications/Neovide.app/Contents/MacOS/neovide --frameless --multigrid";
  };
  home-manager.users.shauryasingh.programs.bat = {
    enable = true;
    config = { theme = "Nord"; };
  };
  home-manager.users.shauryasingh.home.packages = with pkgs; [
    (ripgrep.override { withPCRE2 = true; })
    wget
    exa
    tree
    fd
    sd
    discocss
    neovim
    ## neovide
  ];
  programs.fish.interactiveShellInit = ''
    set -g fish_greeting ""
    if not set -q TMUX
      tmux new-session -A -s main
    end

    set -x EDITOR "nvim"
    set -x PATH ~/.config/emacs/bin $PATH
    set -x PATH ~/.config/scripts $PATH
  '';
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

        if test $HOME != $PWD
            _print_in_color ""(prompt_pwd) blue
        end
        __fish_git_prompt " (%s)"

        _print_in_color " λ " (_prompt_color_for_status $last_status)
    end
  '';
  programs.tmux.enable = true;
  programs.tmux.enableVim = true;
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
    unbind C-b
    bind C-a send-prefix
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
    # and now unbind keys
    unbind Up
    unbind Down
    unbind Left
    unbind Right
    unbind C-Up
    unbind C-Down
    unbind C-Left
    # styling
    set -g status-bg default
    set -g status-fg white
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

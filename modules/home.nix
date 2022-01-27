{ pkgs, lib, config, home-manager, nix-darwin, inputs, ... }: {

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

  home.file = {
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
 };

  xdg.dataFile."discocss/custom.css".source = ../configs/custom.css;

  programs.firefox.enable = true;

  programs.firefox.package =
    pkgs.runCommand "firefox-0.0.0" { } "mkdir $out";
  programs.firefox.extensions =
      with pkgs.nur.repos.rycee.firefox-addons; [
        ublock-origin
        tridactyl
        reddit-enhancement-suite
        betterttv
        theme-nord-polar-night
      ];

  programs.firefox.profiles = let
    userChrome = builtins.readFile ../configs/userChrome.css;
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
      "reader.color_scheme" = "sepia";
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
    git-rebsae = "git rebase -i HEAD~2";
    ls = "lsd";
    ps = "ps";
    tree = "tree -a -C";
    cat = "bat";
    top = "btm";
    cp = "xcp";
    find = "fd";
    calc = "emacs -f full-calc";
  };

  programs.fish.interactiveShellInit = ''
    set -g fish_greeting ""
    zoxide init fish --cmd cd | source
    set -x EDITOR "nvim"
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
    set -g window-status-current-format "#[fg=cyan]#[fg=white]#[bg=cyan]#I #[bg=brightwhite]#[fg=black] #W#[fg=brightwhite]#[bg=default] #[bg=default] #[fg=magenta]#[fg=white]#[bg=magenta]λ #[fg=black]#[bg=brightwhite] %a %d %b #[fg=magenta]%R#[fg=brightwhite]#[bg=default]"
    set -g window-status-format "#[fg=magenta]#[fg=white]#[bg=magenta]#I #[bg=brightwhite]#[fg=black] #W#[fg=brightwhite]#[bg=default] "
  '';
}

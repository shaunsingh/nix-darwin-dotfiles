{ pkgs, lib, config, home-manager, nix-darwin, inputs, ... }: {

  home-manager.users.shauryasingh.programs.git = {
    enable = true;
    userName = "shaunsingh";
    userEmail = "shaunsingh0207@gmail.com";
    delta = {
      enable = true;
      options = {
        syntax-theme = "GitHub";
        line-numbers = true;
      };
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
 };

  home-manager.users.shauryasingh.xdg.dataFile."discocss/custom.css".source = ../configs/custom.css;

  home-manager.users.shauryasingh.programs.firefox.enable = true;

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

  home-manager.users.shauryasingh.programs.alacritty = {
    enable = true;
    package = pkgs.runCommand "alacritty-0.0.0" { } "mkdir $out";
    settings = {
      window.padding.x = 45;
      window.padding.y = 45;
      window.decorations = "transparent";
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
        cursor.cursor = "#37474F";
        primary.background = "#FFFFFF";
        primary.foreground = "#37474F";
        normal = {
          black =   "#000000";
          red =     "#FF6961";
          green =   "#028e2c";
          yellow =  "#FFaB91";
          blue =    "#0098dd";
          magenta = "#673AB7";
          cyan =    "#81A1C1";
          white =   "#FAFAFA";
        };
        bright = {
          black =   "#000000";
          red =     "#FF6961";
          green =   "#028e2c";
          yellow =  "#FFaB91";
          blue =    "#0098dd";
          magenta = "#673AB7";
          cyan =    "#81A1C1";
          white =   "#FAFAFA";
        };
      };
    };
  };

  programs.fish.enable = true;
  environment.shells = with pkgs; [ fish ];
  users.users.shauryasingh = {
    home = "/Users/shauryasingh";
    shell = pkgs.fish;
  };

#  system.activationScripts.postActivation.text = ''
    # Set the default shell as fish for the user
#    sudo chsh -s ${lib.getBin pkgs.fish}/bin/fish shauryasingh
#  '';

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

  programs.fish.interactiveShellInit = ''
    set -g fish_greeting ""
    zoxide init fish --cmd cd | source
    set -x EDITOR "nvim"
    set -x PATH ~/.config/emacs/bin $PATH
  '';

  home-manager.users.shauryasingh.programs.bat = {
    enable = true;
    config = { theme = "GitHub"; };
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

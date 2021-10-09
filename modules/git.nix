{ pkgs, lib, config, ... }: {
  home-manager.users.shauryasingh.programs.git = {
    package = pkgs.git;
    enable = true;
    userName = "shaunsingh";
    userEmail = "shaunsingh0207@gmail.com";
    ignores = [
      ".dir-locals.el"
      ".envrc"
      ".DS_Store"
    ];
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
  home-manager.users.shauryasingh.programs.htop.settings = {
    color_scheme = 0;
    cpu_count_from_one = 0;
    delay = 15;
    tree_view = 1;
    show_cpu_usage = 1;
    hide_function_bar = 2;
    fields = with config.lib.htop.fields; [
      PID
      USER
      PRIORITY
      NICE
      M_SIZE
      M_RESIDENT
      M_SHARE
      STATE
      PERCENT_CPU
      PERCENT_MEM
      TIME
      COMM
    ];
    highlight_base_name = 0;
    highlight_megabytes = 1;
    highlight_threads = 1;
    highlight_changes = 0;
    highlight_changes_delay_secs = 5;
  };
  home-manager.users.shauryasingh.programs.alacritty = {
    enable = true;
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
        cursor.cursor = "#81a1c1";
        primary.background = "#2e3440";
        primary.foreground = "#d8dee9";
        normal = {
          black =    "#3B4252";
          red =      "#BF616A";
          green =    "#A3BE8C";
          yellow =   "#EBCB8B";
          blue =     "#81A1C1";
          magenta =  "#B48EAD";
          cyan =     "#88C0D0";
          white =    "#E5E9F0";
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
  home-manager.users.shauryasingh.programs.kitty = {
    # enable = true;
    settings = {
	font_family = "Liga SFMono Nerd Font";
	font_size = "14.0";
	adjust_line_height = "120%";
	disable_ligatures = "cursor";
	hide_window_decorations = "yes";
	scrollback_lines = "50000";
	cursor_blink_interval = "0.5";
	cursor_stop_blinking_after = "10.0";
	window_border_width = "0.7pt";
	draw_minimal_borders = "yes";
	macos_option_as_alt = "no";
	cursor_shape = "beam";
	
	foreground           =   "#D8DEE9";
	background           =   "#2E3440";
	selection_foreground =   "#000000";
	selection_background =   "#FFFACD";
	url_color            =   "#0087BD";
	cursor               =   "#81A1C1";
	color0               =   "#3B4252";
	color8               =   "#4C566A";
	color1               =   "#BF616A";
	color9               =   "#BF616A";
	color2               =   "#A3BE8C";
	color10              =   "#A3BE8C";
	color3               =   "#EBCB8B";
	color11              =   "#EBCB8B";
	color4               =   "#81A1C1";
	color12              =   "#81A1C1";
	color5               =   "#B48EAD";
	color13              =   "#B48EAD";
	color6               =   "#88C0D0";
	color14              =   "#8FBCBB";
	color7               =   "#E5E9F0";
	color15              =   "#B48EAD";
    };
  };
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
    ls = "exa -F --group-directories-first --icons -a";
    tree = "tree -a -C";
    cat = "bat --paging=never -p";
  };
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

{ config, inputs, inputs', lib, pkgs, system, ... }:

/*
  home-manager configuration
  Useful links:
  - Home Manager Manual: https://nix-community.gitlab.io/home-manager/
  - Appendix A. Configuration Options: https://nix-community.gitlab.io/home-manager/options.html
*/
{
  config = {
    themes.base16 = {
      enable = true;
      path = "${inputs.base16-oxocarbon}/base16-oxocarbon-dark.yaml";
    };
  };

  home = {
    packages = __attrValues {
      inherit (pkgs)
        bat
        difftastic
        nixpkgs-fmt
        nixpkgs-review
        uutils-coreutils
        (ripgrep.override {
          withPCRE2 = true;
        });

      inherit (pkgs.gitAndTools) gh;
      inherit (inputs'.agenix.packages) agenix;
    };
    sessionVariables = {
      MANPAGER = "nvim +Man! -c 'nnoremap i <nop>'";
    };
    file = {
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
  };

  programs = {

    zoxide = {
      enable = true;
      options = [ "--cmd cd" ];
    };

    tmux = {
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
    starship = {
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
    git = {
      enable = true;
      userName = "shaunsingh";
      userEmail = "shaunsingh0207@gmail.com";
      ignores = [ "**/.idea/" "**/.vscode/settings.json" "**/.direnv/" "**/.DS_Store" ];
      extraConfig = {
        pull = { ff = "only"; };
        init.defaultBranch = "main";
        extraConfig = {
          diff.tool = "difftastic";
          pager.difftool = true;
  
          difftool = {
            prompt = false;
            difftastic.cmd = ''${lib.getExe pkgs.difftastic} "$LOCAL" "$REMOTE"'';
          };
        };
      };
    };
    fish = {
      enable = true;
      shellAliases = with pkgs; {
        ":q" = "exit";
        git-rebase = "git rebase -i HEAD~2";
        ll = "${pkgs.eza}/bin/eza -lF --color-scale --no-user --no-time --no-permissions --group-directories-first --icons -a";
        ls = "${pkgs.eza}/bin/eza -lF --group-directories-first --icons -a";
        cp = "${pkgs.xcp}/bin/xcp";
        top = "${pkgs.bottom}/bin/btm";
        cat = "${pkgs.bat}/bin/bat --paging=never";
      };
      shellInit = ''
        set fish_greeting
      '';
    };

    eza.enable = true;
    bat.enable = true;

    direnv = {
      enable = true;
      nix-direnv.enable = true;
    };

    home-manager = {
      enable = true;
      path = "${inputs.home}";
    };

    nix-index-database.comma.enable = lib.mkDefault true;
  };
}
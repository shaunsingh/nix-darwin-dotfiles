{ inputs, lib, pkgs, ... }:

/*
  home-manager configuration
  Useful links:
  - Home Manager Manual: https://nix-community.gitlab.io/home-manager/
  - Appendix A. Configuration Options: https://nix-community.gitlab.io/home-manager/options.html
*/
{
  home = {
    packages = builtins.attrValues {
      inherit (pkgs)
        bat
        ripgrep
        nixpkgs-fmt
        nixpkgs-review
        uutils-coreutils;

      inherit (pkgs.gitAndTools) gh;
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

    starship = {
      enable = true;
      enableFishIntegration = false;
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
      };
    };
    fish = {
      enable = true;
      interactiveShellInit = ''
        if test "$TERM" != "dumb"
          eval (${pkgs.starship}/bin/starship init fish)
        end
      '';
      shellAliases = {
        ":q" = "exit";
        git-rebase = "git rebase -i HEAD~2";
        ll = "${pkgs.eza}/bin/eza -lF --color-scale --no-user --no-time --no-permissions --group-directories-first --icons -a";
        ls = "${pkgs.eza}/bin/eza -lF --group-directories-first --icons -a";
        cp = "${pkgs.xcp}/bin/xcp";
        top = "${pkgs.bottom}/bin/btm";
        cat = "${pkgs.bat}/bin/bat --paging=never";
      };
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

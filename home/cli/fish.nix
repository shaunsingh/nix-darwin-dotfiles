{ pkgs
, lib
, inputs
, config
, ...
}: {
  programs.fish = {
    enable = true;
    shellAliases = with pkgs; {
      ":q" = "exit";
      git-rebase = "git rebase -i HEAD~2";
      ll = "${pkgs.exa}/bin/exa -lF --color-scale --no-user --no-time --no-permissions --group-directories-first --icons -a";
      ls = "${pkgs.exa}/bin/exa -lF --group-directories-first --icons -a";
      cp = "${pkgs.xcp}/bin/xcp";
      top = "${pkgs.bottom}/bin/btm";
      cat = "${pkgs.bat}/bin/bat --paging=never";
      nvim = "${pkgs.neovim-nightly}/bin/nvim --startuptime /tmp/nvim-startuptime";
    };
    shellInit = ''
      set fish_greeting
    '';
  };
}

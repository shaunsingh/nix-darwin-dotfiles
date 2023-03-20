{ pkgs
, lib
, inputs
, config
, ...
}: {
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
}

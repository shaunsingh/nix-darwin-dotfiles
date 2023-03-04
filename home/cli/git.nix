{
  pkgs,
  lib,
  inputs,
  config,
  ...
}: {
  programs.git = {
    enable = true;
    userName = "shaunsingh";
    userEmail = "shaunsingh0207@gmail.com";
    delta.enable = true;
    ignores = ["**/.idea/" "**/.vscode/settings.json" "**/.direnv/" "**/.DS_Store"];
    extraConfig = {
      pull = {ff = "only";};
      init.defaultBranch = "main";
    };
  };
}

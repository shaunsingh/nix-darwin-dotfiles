{ pkgs, ... }: {
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
}

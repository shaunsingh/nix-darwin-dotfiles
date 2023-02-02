# shared between linux and macOS
{ pkgs
, lib
, inputs
, config
, ...
}: {
  home.stateVersion = "23.05";
  home.packages = with pkgs; [
    # clang/coreutils
    uutils-coreutils
    clang_14

    # utils
    (ripgrep.override { withPCRE2 = true; })
    procs
    hyperfine
    skim
    gitoxide
 
    # editors 
    neovim

    # tooling 
    luajit
    fennel
    fnlfmt
    julia-bin
    inferno
    (rust-bin.selectLatestNightlyWith (toolchain:
      toolchain.default.override {
        extensions = [ "rust-src" ];
        targets = [ "arm-unknown-linux-gnueabihf" ];
      }))

  ] ++ lib.optionals stdenv.isLinux [
    # notifications
    brightness
    volume
    microphone

    # screenshotting
    screenshot
    ocrScript
  ];

  # use mold for rust
  home.file.".cargo/config.toml".text = ''
    [target.arm-unkown-linux-gnueabihf]
    "linker = "clang"
    "rustflags = ["-C", "link-arg=-fuse-ld=${pkgs.mold}/bin/mold"]
  '';

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

      nvim = "${pkgs.neovim}/bin/nvim --startuptime /tmp/nvim-startuptime";
    };
    shellInit = ''
      set fish_greeting
    '';
  };

  programs.bat.enable = true;
  programs.exa.enable = true;
  programs.zoxide = {
    enable = true;
    options = [ "--cmd cd" ];
  };

  programs.git = {
    enable = true;
    userName = "shaunsingh";
    userEmail = "shaunsingh0207@gmail.com";
    delta.enable = true;
    ignores =
      [ "**/.idea/" "**/.vscode/settings.json" "**/.direnv/" "**/.DS_Store" ];
    extraConfig = {
      pull = { ff = "only"; };
      init.defaultBranch = "main";
    };
  };

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

  # symlinks don't work with finder + spotlight, copy them instead
  disabledModules = [ "targets/darwin/linkapps.nix" ];
  home.activation = lib.mkIf pkgs.stdenv.isDarwin {
    copyApplications =
      let
        apps = pkgs.buildEnv {
          name = "home-manager-applications";
          paths = config.home.packages;
          pathsToLink = "/Applications";
        };
      in
      lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        baseDir="$HOME/Applications/Home Manager Apps"
        if [ -d "$baseDir" ]; then
          rm -rf "$baseDir"
        fi
        mkdir -p "$baseDir"
        for appFile in ${apps}/Applications/*; do
          target="$baseDir/$(basename "$appFile")"
          $DRY_RUN_CMD cp ''${VERBOSE_ARG:+-v} -fHRL "$appFile" "$baseDir"
          $DRY_RUN_CMD chmod ''${VERBOSE_ARG:+-v} -R +w "$target"
        done
      '';
  };
}

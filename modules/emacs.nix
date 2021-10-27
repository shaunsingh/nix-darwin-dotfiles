{ pkgs, lib, config, ... }: {
  fonts = {
    enableFontDir = true;
    fonts = with pkgs; [ alegreya overpass alegreya-sans ibm-plex ];
  };
  # services.mbsync.enable = true;
  # services.mbsync.postExec = ''
  #   if pgrep -f 'mu server'; then
  #       ${config.home-manager.users.shauryasingh.programs.emacs.package}/bin/emacsclient \
  #         -e '(mu4e-update-index)'
  #   else
  #       ${pkgs.mu}/bin/mu index --nocolor
  #   fi
  # '';
  # accounts.email.accounts.fastmail.primary = false;
  # accounts.email.accounts.work = let
  #   mailAddr = name: domain: "${name}@${domain}";
  #   maildirBasePath = ".mail";
  #   certificatesFile = "/etc/ssl/certs/ca-certificates.crt";
  # in rec {
  #   mu.enable = true;
  #   msmtp.enable = true;
  #   primary = true;
  #   address = mailAddr "shaunsingh0207" "gmail.com";
  #   userName = address;
  #   realName = "Shaurya Singh";
  # 
  #   mbsync = {
  #     enable = true;
  #     create = "both";
  #     expunge = "both";
  #     remove = "both";
  #   };
  # 
  #   imap.host = "imap.gmail.com";
  #   smtp = {
  #     host = "smtp.gmail.com";
  #     port = 587;
  #     tls.useStartTls = true;
  #   };
  #   passwordCommand = "${pkgs.writeShellScript "home-mbsyncPass" ''
  #     TODO: write shell script to grab gmail pass
  #   ''}";
  # };
  home-manager.users.shauryasingh.home.file."~/.config/tree-sitter".source = (pkgs.runCommand "grammars" {} ''
    mkdir -p $out/bin
    ${lib.concatStringsSep "\n"
      (lib.mapAttrsToList (name: src: "name=${name}; ln -s ${src}/parser $out/bin/\${name#tree-sitter-}.so") pkgs.tree-sitter.builtGrammars)};
  '');
  services.emacs = {
    package = pkgs.emacsGcc;
    enable = true;
  };
  home-manager.users.shauryasingh.home.packages = with pkgs; [
    (ripgrep.override { withPCRE2 = true; })
    gnutls
    gnuplot
    sqlite
    tree-sitter
    (aspellWithDicts (dicts: with dicts; [ en en-computers en-science ]))
    (texlive.combine {
      inherit (texlive)
        scheme-small dvipng dvisvgm l3packages xcolor soul adjustbox collectbox
        amsmath siunitx cancel mathalpha capt-of chemfig wrapfig mhchem fvextra
        cleveref latexmk tcolorbox environ arev amsfonts simplekv alegreya
        sourcecodepro newpx;
    })
    sdcv
    emacsGcc
    # Language stuff
    python39Packages.grip
    python39Packages.pyflakes
    python39Packages.isort
    python39Packages.pytest
    nodePackages.pyright
    pipenv
    nixfmt
    black
    rust-analyzer
    rustup
    shellcheck
  ];
}

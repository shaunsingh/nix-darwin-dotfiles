{ pkgs, lib, config, ... }: {
  system.activationScripts.postUserActivation.text = ''
    # Clone to $XDG_CONFIG_HOME because Emacs expects this location.
    if [[ ! -d "~/.config/emacs" ]]; then
      git clone --depth 1 https://github.com/hlissner/doom-emacs "~/.config/emacs"
    fi
  '';
  fonts = {
    enableFontDir = true;
    fonts = with pkgs; [ alegreya overpass ];
  };
  services.emacs = {
    enable = true;
    package = pkgs.emacs;
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
  home-manager.users.shauryasingh.home.packages = with pkgs; [
    gnutls
    gnuplot
    sqlite
    (aspellWithDicts (dicts: with dicts; [ en en-computers en-science ]))
    (pkgs.texlive.combine {
      inherit (pkgs.texlive)
        scheme-small dvipng dvisvgm l3packages xcolor soul adjustbox collectbox
        amsmath siunitx cancel mathalpha capt-of chemfig wrapfig mhchem fvextra
        cleveref latexmk tcolorbox environ;
    })
    sdcv
    emacs
  ];
}

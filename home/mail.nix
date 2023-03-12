{
  pkgs,
  lib,
  inputs,
  config,
  ...
}: {
  programs.mbsync.enable = true;
  programs.msmtp.enable = true;
  programs.astroid = {
    enable = true;
    externalEditor = "foot nvim -c 'set ft=mail' '+set fileencoding=utf-8' '+set ff=unix' '+set enc=utf-8' '+set fo+=w' %1";
  };
  programs.notmuch = {
    enable = true;
    hooks = {
      preNew = "mbsync --all";
    };
  };

  accounts.email = {
    maildirBasePath = ".local/share/mail";
    accounts = {
      "shaunsingh0207" = {
        address = "shaunsingh0207@gmail.com";
        userName = "shaunsingh0207@gmail.com";
        realName = "Shaurya Singh";
        primary = true;

        imap.host = "imap.gmail.com";
        smtp.host = "smtp.gmail.com";

        gpg = {
          key = "532F9FD72025A5304A96E76F5ED6D1FF7704CF89";
          signByDefault = true;
        };
        signature = {
          text = ''
            Thanks,
            Shaurya Singh
          '';
          showSignature = "append";
        };

        passwordCommand = "${pkgs.gnupg}/bin/gpg -q --for-your-eyes-only --no-tty --exit-on-status-write-error --batch --passphrase-file ~/.config/mail/shaunsingh0207.pass -d ~/.cache/mail/shaunsingh0207.pass.gpg";

        msmtp.enable = true;
        notmuch.enable = true;
        astroid.enable = true;

        mbsync = {
          enable = true;
          create = "both";
          expunge = "both";
          patterns = ["*" "![Gmail]*" "[Gmail]/Sent Mail" "[Gmail]/Starred" "[Gmail]/All Mail"];
        };
      };
    };
  };
}

{
  pkgs,
  lib,
  inputs,
  config,
  ...
}: {
  accounts.email = {
    maildirBasePath = ".local/share/mail";
    accounts = {
      "shaunsingh0207" = {
        address = "shaunsingh0207@gmail.com";
        userName = "shaunsingh@gmail.com";
        realName = "Shaurya Singh";
        passwordCommand = "${pkgs.gnupg}/bin/gpg -q --for-your-eyes-only --no-tty --exit-on-status-write-error --batch --passphrase-file ${config.home.homeDirectory}/.config/mail/shaunsingh0207.pass -d ${config.home.homeDirectory}/.cache/mail/shaunsingh0207.pass.gpg";
        imap.host = "imap.gmail.com";
        smtp.host = "smtp.gmail.com";
        mbsync = {
          enable = true;
          create = "both";
          expunge = "both";
          patterns = ["*" "![Gmail]*" "[Gmail]/Sent Mail" "[Gmail]/Starred" "[Gmail]/All Mail"];
          extraConfig = {
            channel = {
              Sync = "All";
            };
            account = {
              Timeout = 120;
              PipelineDepth = 1;
            };
          };
        };
        notmuch.enable = true;
        astroid.enable = true;
        msmtp.enable = true;
      };
    };
  };
  programs.msmtp.enable = true;
  programs.notmuch.enable = true;
  programs.afew = {
    enable = true;
    extraConfig = ''
      [SpamFilter]
      [KillThreadsFilter]
      [ListMailsFilter]
      [ArchiveSentMailsFilter]
      [FolderNameFilter]
      maildir_separator = /
  
      [MailMover]
      folders = shaunsingh0207/Inbox
      rename = true
  
      shaunsingh0207/Inbox = 'NOT tag:Inbox':"shuansingh0207/[Gmail]/All Mail"
    '';
  };
  programs.astroid = {
    enable = true;
    externalEditor = "foot nvim";
    extraConfig = {
      startup.queries.inbox = "tag:Inbox";
      startup.queries.inbox_shaunsingh0207 = "folder:shaunsingh0207/Inbox";
    };
  };
  services.mbsync = {
    enable = true;
    preExec = "${config.xdg.configHome}/mbsync/preExec";
    postExec = "${config.xdg.configHome}/mbsync/postExec";
    frequency = "*:0/30";
  };
  home.file."bin/msmtp" = {
    text = ''
    #!${pkgs.stdenv.shell}
    ${pkgs.libnotify}/bin/notify-send "Sending mail"
    ${pkgs.msmtp}/bin/msmtp --read-envelope-from $@
    '';
    executable = true;
  };
  xdg.configFile."mbsync/preExec" = {
    text = ''
    #!${pkgs.stdenv.shell}
  
    export NOTMUCH_CONFIG=${config.xdg.configHome}/notmuch/notmuchrc
    export NMBGIT=${config.xdg.dataHome}/notmuch/nmbug
  
    ${pkgs.coreutils}/bin/mkdir -p ${config.home.homeDirectory}/.local/share/mail/shuansingh0207
    ${pkgs.afew}/bin/afew -C  ${config.xdg.configHome}/notmuch/notmuchrc -m -v
    '';
    executable = true;
  };
  xdg.configFile."mbsync/postExec" = {
    text = ''
    #!${pkgs.stdenv.shell}
  
    export NOTMUCH_CONFIG=${config.xdg.configHome}/notmuch/notmuchrc
    export NMBGIT=${config.xdg.dataHome}/notmuch/nmbug
  
    ${pkgs.notmuch}/bin/notmuch new
    ${pkgs.afew}/bin/afew -C ${config.xdg.configHome}/notmuch/notmuchrc --tag --new -v
    # Remove inbox (lower-case)
    ${pkgs.notmuch}/bin/notmuch tag -inbox -- tag:inbox
    # Remove Inbox tagged message that are not in an Inbox
    ${pkgs.notmuch}/bin/notmuch tag -Inbox -- not folder:shaunsingh0207/Inbox and tag:Inbox
    ${pkgs.libnotify}/bin/notify-send "Mails synced"
    '';
    executable = true;
  };
  home.file."bin/msync" = {
    text = ''
    #!${pkgs.stdenv.shell}
    ${pkgs.libnotify}/bin/notify-send "Syncing mails"
    systemctl --user start mbsync
    '';
    executable = true;
  };
}

{
  pkgs,
  lib,
  config,
  ...
}: let
  cfg = config.services.xremap;
  userPath = "/run/user/${toString cfg.userId}";
  configFile = pkgs.writeTextFile {
    name = "xremap-config.yml";
    text = pkgs.lib.generators.toYAML {} cfg.config;
  };
in {
  systemd.services.xremap = lib.mkIf (cfg.serviceMode == "system") {
    description = "xremap system service";
    path = [cfg.package];
    wantedBy = ["multi-user.target"];
    serviceConfig = {
      PrivateNetwork = true;
      MemoryDenyWriteExecute = true;
      CapabilityBoundingSet = ["~CAP_SETUID" "~CAP_SETGID" "~CAP_SETPCAP" "~CAP_SYS_ADMIN" "~CAP_SYS_PTRACE" "~CAP_NET_ADMIN" "~CAP_FOWNER" "~CAP_IPC_OWNER" "~CAP_SYS_TIME" "~CAP_KILL" "~CAP_SYS_BOOT" "~CAP_LINUX_IMMUTABLE" "~CAP_IPC_LOCK" "~CAP_SYS_CHROOT" "~CAP_BLOCK_SUSPEND" "~CAP_SYS_PACCT" "~CAP_WAKE_ALARM" "~CAP_AUDIT_WRITE" "~CAP_AUDIT_CONTROL" "~CAP_AUDIT_READ" "CAP_DAC_READ_SEARCH" "CAP_DAC_OVERRIDE"];
      SystemCallArchitectures = ["native"];
      RestrictRealtime = true;
      SystemCallFilter = map (x: "~@${x}") ["clock" "debug" "module" "reboot" "swap" "cpu-emulation" "obsolete" "privileged" "resources"];
      LockPersonality = true;
      UMask = "077";
      IPAddressDeny = ["0.0.0.0/0" "::/0"];
      # ProtectClock adds to DeviceAllow, which does not seem to work with xremap since it tries to enumerate all /dev/input devices
      # ProtectClock = true;
      # DeviceAllow = [ "/dev/input/event24" ];
      ProtectHostname = true;
      # Does not work, the service cannot locate sway socket
      # PrivateUsers = true;
      RestrictAddressFamilies = "AF_UNIX";
      RestrictNamespaces = true;
      # RestrictNamespaces = ["~CLONE_NEWUSER" "~CLONE_NEWIPC" "~CLONE_NEWNET" "~CLONE_NEWNS" "~CLONE_NEWPID"];
      # ProtectClock = true;
      # Need 'tmpfs' here so that the socket may be actually bind-mounted through Bind*Paths
      ProtectHome = "tmpfs";
      # This is needed, otherwise xremap cannot read from sway socket
      BindReadOnlyPaths = lib.mkIf cfg.withSway [userPath];
      # Sway socket gets generated as $XDG_RUNTIME_DIR/sway-ipc.$UID.$SWAY_PID
      # Hacky way to allow sway socket
      # Systemd does not support wildcards :(
      InaccessiblePaths = lib.mkIf cfg.withSway (map (x: "-${userPath}/${x}") ["app" "bus" "dbus-1" ".dbus-proxy" "dconf" "env-vars" ".flatpak" ".flatpak-helper" "gnupg" "pipewire-0" "pipewire-0.lock" "pulse" "systemd" "tmux-${toString cfg.userId}" "wayland-1" "wayland-1.lock"]);
      # Looks like Hyprland socket is hardcoded to be in tmp
      PrivateTmp = cfg.withHypr;
      ProtectKernelLogs = true;
      # Does not work, running as root
      # ProtectProc = true;
      # SystemCallFilter = "~@clock";
      NoNewPrivileges = true;
      ProtectSystem = "strict";
      ProtectKernelTunables = true;
      ProtectKernelModules = true;
      ProtectControlGroups = true;
      RestrictSUIDSGID = true;
      # End of hardening
      ExecStart = ''
        ${cfg.package}/bin/xremap ${
          if cfg.deviceName != ""
          then "--device \"${cfg.deviceName}\""
          else ""
        } ${
          if cfg.watch
          then "--watch"
          else ""
        } ${configFile}
      '';
      Nice = -20;
    };
  };
}

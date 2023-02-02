{ pkgs
, lib
, inputs
, ...
}:
{
  # give myself sudo rights
  users.users.shauryasingh = {
    isNormalUser = true;
    home = "/home/shauryasingh";
    extraGroups = [ 
      "wheel" 
      "networkmanager" 
      "audio" 
      "video"
      "render"
      "docker"
    ];
    shell = pkgs.fish;
  };

  # use systemd-boot
  boot.loader.systemd-boot.enable = true;

  # us locale
  services.timesyncd.enable = true;
  i18n = {
    defaultLocale = lib.mkDefault "en_US.UTF-8";

    extraLocaleSettings = {
      LC_TIME = lib.mkDefault "en_US.UTF-8";
    };

    supportedLocales = lib.mkDefault [
      "en_US.UTF-8/UTF-8"
      "ja_JP.UTF-8/UTF-8"
    ];
  };

  # console
  console =
    let
      normal = [ "181825" "F38BA8" "A6E3A1" "F9E2AF" "89B4FA" "F5C2E7" "94E2D5" "BAC2DE" ];
      bright = [ "1E1E2E" "F38BA8" "A6E3A1" "F9E2AF" "89B4FA" "F5C2E7" "94E2D5" "A6ADC8" ];
    in
    {
      # colors = normal ++ bright;
      keyMap = "us";
    };

  # env variables + essential packages
  environment = {
    variables = {
      # defaults
      EDITOR = "nvim";
      # BROWSER = "nyxt";
      BROWSER = "firefox";
      # enable wayland 
      NIXOS_OZONE_WL = "1";
      XDG_SESSION_TYPE = "wayland";
      SDL_VIDEODRIVER = "wayland";
      CLUTTER_BACKEND = "wayland";
      GDK_BACKEND = "wayland";
      # qt wayland settings
      DISABLE_QT5_COMPAT = "0";
      QT_AUTO_SCREEN_SCALE_FACTOR = "1";
      QT_QPA_PLATFORM = "wayland";
      QT_QPA_PLATFORMTHEME = "qt5ct";
      QT_STYLE_OVERRIDE = "kvantum";
      QT_WAYLAND_DISABLE_WINDOWDECORATION = "1";
      # fix java stuff on wayland 
      _JAVA_AWT_WM_NONREPARENTING = "1";
      # firefox
      MOZ_ENABLE_WAYLAND = "1";
      MOZ_USE_XINPUT2 = "1";
      MOZ_DISABLE_RDD_SANDBOX = "1";
      # beta opengl for firefox
      MESA_GL_VERSION_OVERRIDE = "3.3";   
      MESA_GLES_VERSION_OVERRIDE = "3.1";
      MESA_GLSL_VERSION_OVERRIDE = "330";
    };
  };

  # disk
  services.fstrim.enable = true;

  # bluetooth support
  hardware.bluetooth.enable = true;

  # needed for gtk
  programs.dconf.enable = true;

  # sound
  services.pipewire = {
    enable = true;
    pulse.enable = true;
    alsa.enable = true;
  };
  
  # network
  networking.networkmanager.enable = true;
  systemd.services.NetworkManager-wait-online.enable = false;

  # power
  services.upower.enable = true;

  # xdg directories
  xdg.portal = {
    enable = true;
    wlr.enable = true;
    extraPortals = [
      pkgs.xdg-desktop-portal-wlr
      pkgs.xdg-desktop-portal-gtk
    ];
  };

  # font + fontconfig
  fonts = {
    fonts = with pkgs; [
      # cjk
      sarasa-gothic
      # apple fonts
      sf-mono-liga-bin
      otf-apple
    ];
    fontconfig = {
      enable = true;
      antialias = true;
      hinting = {
        enable = true;
        autohint = true;
        style = "hintfull";
      };
      subpixel.lcdfilter = "default";
      defaultFonts = {
        # emoji = [ "Noto Color Emoji" ];
        monospace = [ "Liga SFMono Nerd Font" ];
        sansSerif = [ "SF Pro Text" ];
        serif = [ "New York Medium" ];
      };
    };
  };
}

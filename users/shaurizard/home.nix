{ config, inputs, inputs', lib, pkgs, system, ... }:

/*
  home-manager configuration
  Useful links:
  - Home Manager Manual: https://nix-community.gitlab.io/home-manager/
  - Appendix A. Configuration Options: https://nix-community.gitlab.io/home-manager/options.html
*/
{
  imports = [
    # Append your custom home-manager modules in this list
    ../../modules/shared/home-manager/wayland/kickoff.nix
  ];

  ### -- colorscheme
  colorscheme = inputs.nix-colors.lib.schemeFromYAML "oxocarbon-dark" (builtins.readFile ../../assets/colorschemes/base16-oxocarbon-dark.yaml);

  ### -- home
  home = {
    packages = builtins.attrValues {
      inherit (pkgs)
        clang
        ffmpeg
        zotero
        transmission-gtk
        font-manager
        wayland-utils
        xdg_utils
        spotify;

      inherit (inputs'.nixpkgs-wayland.packages)
        grim
        slurp
        swaybg
        swayidle
        swaylock
        wf-recorder
        wl-clipboard
        wlogout;
    };

    sessionPath = [
      "${config.xdg.configHome}/scripts"
      "${config.home.homeDirectory}/.local/bin"
    ];

    sessionVariables = {
      EDITOR = "nvim";
      BROWSER = "firefox";
      QT_QPA_PLATFORMTHEME = "qt5ct";
      RUSTUP_HOME = "${config.home.homeDirectory}/.local/share/rustup";
      WLR_NO_HARDWARE_CURSORS = "1";
      NIXOS_OZONE_WL = "1";
      NVD_BACKEND = "direct";
      LIBVA_DRIVER_NAME = "nvidia";
      MOZ_DISABLE_RDD_SANDBOX = "1";
      MOZ_GLX_TEST_EARLY_WL_ROUNDTRIP = "1";
    };
  };

  ### -- tmux
  tmux = {
    enable = true;
    sensibleOnTop = true;
    extraConfig = with config.colorscheme.colors; ''
      set -g status-right-length 100
      set -g status-left-length 100
      set -g status-left " "
      set -g status-right " "
      set -g status-justify left
      set -g status-style fg=black,bg=default
      set -g window-status-current-format "#[fg=#${base00},bg=#${base0C}] #I #[fg=#${base05},bg=#${base01}] [#W] #[fg=#${base03},bg=#${base01}]#{s|$HOME|~|;s|.*/||:pane_current_path} "
      set -g window-status-format "#[fg=#{base00},bg=#{base0F}] #I #[fg=#{base04},bg=#{base01}] [#W] #[fg=#{base03},bg=#{base01}]#{s|$HOME|~|;s|.*/||:pane_current_path} "
    '';
  };

  ### -- sway
  wayland.windowManager.sway = {
    enable = true;
    systemdIntegration = true;
    extraSessionCommands = ''
      export XDG_SESSION_DESKTOP=sway
      export SDL_VIDEODRIVER=wayland
      export QT_QPA_PLATFORM=wayland-egl
      export QT_WAYLAND_DISABLE_WINDOWDECORATION="1"
      export MOZ_ENABLE_WAYLAND=1
      export CLUTTER_BACKEND=wayland
      export ECORE_EVAS_ENGINE=wayland-egl
      export ELM_ENGINE=wayland_egl
      export NO_AT_BRIDGE=1
      export _JAVA_AWT_WM_NONREPARENTING=1
    '';
    wrapperFeatures.gtk = true;
    config = {
      startup = [
        {
          command = "${inputs.eww.packages.${system}.eww-wayland}/bin/eww open bar && ${inputs.eww.packages.${system}.eww-wayland}/bin/eww open bar2";
          always = false;
        }
        {
          command = "${pkgs.pamixer}/bin/pamixer --set-volume 33";
          always = false;
        }
        {
          command = "${pkgs.brightnessctl}/bin/brightnessctl s 66%";
          always = false;
        }
      ];
      window = {
        titlebar = true;
        border = 0;
      };
      input = {
        "keyboard" = {
          xkb_layout = "us";
          xkb_options = "caps:super";
        };
        "type:mouse" = {
          dwt = "disabled";
          accel_profile = "flat";
        };
        "type:touchpad" = {
          tap = "enabled";
          accel_profile = "adaptive";
          scroll_factor = "0.45";
          pointer_accel = "0.27";
        };
      };
      output = {
        "*" = {
          background = "#${config.colorscheme.colors.baseDARK} solid_color";
        };
      };
      bars = lib.mkForce [ ];
      gaps.outer = 18;
      defaultWorkspace = "workspace 1";
      keybindings =
        let
          modifier = "Mod4";
          concatAttrs = lib.fold (x: y: x // y) { };
          tagBinds =
            concatAttrs
              (map
                (i: {
                  "${modifier}+${toString i}" = "exec 'swaymsg workspace ${toString i} && ${inputs.eww.packages.${system}.eww-wayland}/bin/eww update active-tag=${toString i}'";
                  "${modifier}+Shift+${toString i}" = "exec 'swaymsg move container to workspace ${toString i}'";
                })
                (lib.range 0 9));
        in
        tagBinds
        // 
        {
          "${modifier}+Return" = "exec ${pkgs.foot}/bin/foot";
          "${modifier}+d" = "exec ${pkgs.kickoff}/bin/kickoff";
          "${modifier}+p" = "exec ${pkgs.screenshot}/bin/screenshot";
          "${modifier}+Shift+p" = "exec ${pkgs.ocrScript}/bin/wl-ocr";
          "${modifier}+Shift+p" = "exec ${pkgs.grim}/bin/grim -o eDP-1";
          "${modifier}+v" = "exec ${pkgs.volume}/bin/volume -d 5";
          "${modifier}+b" = "exec ${pkgs.volume}/bin/volume -i 5";
          "${modifier}+Shift+v" = "exec ${pkgs.playerctl}/bin/playerctl play-pause";
          "${modifier}+Shift+b" = "exec ${pkgs.volume}/bin/volume -t";
          "${modifier}+n" = "exec ${pkgs.brightness}/bin/brightness set 5%-";
          "${modifier}+m" = "exec ${pkgs.brightness}/bin/brightness set 5%+";
          "${modifier}+Shift+n" = "exec ${pkgs.playerctl}/bin/playerctl previous";
          "${modifier}+Shift+m" = "exec ${pkgs.playerctl}/bin/playerctl next";
          "${modifier}+q" = "kill";
          "${modifier}+r" = ''mode "resize"'';
          "${modifier}+h" = "focus left";
          "${modifier}+j" = "focus down";
          "${modifier}+k" = "focus up";
          "${modifier}+l" = "focus right";
          "${modifier}+Shift+h" = "move left";
          "${modifier}+Shift+j" = "move down";
          "${modifier}+Shift+k" = "move up";
          "${modifier}+Shift+l" = "move right";
          "${modifier}+s" = "layout stacking";
          "${modifier}+w" = "layout tabbed";
          "${modifier}+e" = "layout toggle split";
          "${modifier}+f" = "fullscreen";
          "${modifier}+space" = "floating toggle";
          "${modifier}+Shift+s" = "sticky toggle";
          "${modifier}+Shift+space" = "focus mode_toggle";
          "${modifier}+a" = "focus parent";
          "${modifier}+Shift+c" = "reload";
          "${modifier}+Shift+e" = "exit";
        };
      colors = with config.colorscheme.colors; {
        focused = {
          background = "#${base05}";
          indicator = "#${base05}";
          border = "#${base05}";
          text = "#${base05}";
          childBorder = "#${base05}";
        };
        focusedInactive = {
          background = "#${base01}";
          indicator = "#${base01}";
          border = "#${base01}";
          text = "#${base01}";
          childBorder = "#${base01}";
        };
        unfocused = {
          background = "#${base01}";
          indicator = "#${base01}";
          border = "#${base01}";
          text = "#${base01}";
          childBorder = "#${base01}";
        };
        urgent = {
          background = "#${base0A}";
          indicator = "#${base0A}";
          border = "#${base0A}";
          text = "#${base0A}";
          childBorder = "#${base0A}";
        };
      };
    };
    extraConfig = ''
      default_border none
      default_floating_border none

      # gestures
      bindgesture swipe:3:right workspace prev
      bindgesture swipe:3:left workspace next
    '';
  };
  services.kanshi.systemdTarget = "sway-session.target";

  ### -- launcher
  programs.kickoff = {
    enable = true;
    settings = {
      prompt = "λ  ";
      padding = 54;
      fonts = [ "Liga SFMono Nerd Font" ];
      font_size = 21.0;
      colors = with config.colorscheme.colors; {
        background = "#${baseDARK}FF";
        prompt = "#${base0C}FF";
        text = "#${base04}FF";
        text_query = "#${base06}FF";
        text_selected = "#${base08}FF";
      };
    };
  };

  ### -- display 
  services.kanshi = {
    enable = true;
    profiles = {
      undocked = {
        outputs = [
          {
            criteria = "eDP-1";
            scale = 2.0;
          }
        ];
      };
    };
  };

  ### -- GTK 
  gtk = {
    enable = true;
    font.name = "Liga SFMono Nerd Font";
    iconTheme = {
      package = pkgs.whitesur-icon-theme;
      name = "WhiteSur-dark";
    };
    theme = {
      name = "phocus";
      package = pkgs.phocus-oxocarbon;
    };
  };
  # gtk 4 themes suck
  xdg.configFile."gtk-4.0/gtk.css".text = ''
    @define-color surface-strongest rgb(10, 10, 10);
    @define-color surface-strong rgb(20, 20, 20);
    @define-color surface-moderate rgb(28, 28, 28);
    @define-color surface-weak rgb(34, 34, 34);
    @define-color surface-weakest rgb(40, 40, 40);

    @define-color white-strongest rgb(255, 255, 255);
    @define-color white-strong rgba(255, 255, 255, 0.87);
    @define-color white-moderate rgba(255, 255, 255, 0.34);
    @define-color white-weak rgba(255, 255, 255, 0.14);
    @define-color white-weakest rgba(255, 255, 255, 0.06);

    @define-color black-strongest rgb(0, 0, 0);
    @define-color black-strong rgba(0, 0, 0, 0.87);
    @define-color black-moderate rgba(0, 0, 0, 0.42);
    @define-color black-weak rgba(0, 0, 0, 0.15);
    @define-color black-weakest rgba(0, 0, 0, 0.06);

    @define-color red-tint rgba(218, 88, 88, 0.6);
    @define-color red-normal rgb(218, 88, 88);
    @define-color red-light rgb(227, 109, 109);
    @define-color orange-tint rgba(237, 148, 84, 0.6);
    @define-color orange-normal rgb(237, 148, 84);
    @define-color orange-light rgb(252, 166, 105);
    @define-color yellow-normal rgb(232, 202, 94);
    @define-color yellow-light rgb(250, 221, 117);
    @define-color green-tint rgba(63, 198, 97, 0.6);
    @define-color green-normal rgb(63, 198, 97);
    @define-color green-light rgb(97, 214, 126);
    @define-color cyan-normal rgb(92, 216, 230);
    @define-color cyan-light rgb(126, 234, 246);
    @define-color blue-normal rgb(73, 126, 233);
    @define-color blue-light rgb(93, 141, 238);
    @define-color purple-normal rgb(113, 84, 242);
    @define-color purple-light rgb(128, 102, 245);
    @define-color pink-normal rgb(213, 108, 195);
    @define-color pink-light rgb(223, 129, 207);

    @define-color accent_color @purple-light;
    @define-color accent_bg_color @purple-normal;
    @define-color accent_fg_color @white-strong;

    @define-color destructive_color @red-light;
    @define-color destructive_bg_color @red-tint;
    @define-color destructive_fg_color @white-strong;

    @define-color success_color @green-light;
    @define-color success_bg_color @green-tint;
    @define-color success_fg_color @white-strong;

    @define-color warning_color @orange-light;
    @define-color warning_bg_color @orange-tint;
    @define-color warning_fg_color @white-strong;

    @define-color error_color @red-light;
    @define-color error_bg_color @red-tint;
    @define-color error_fg_color @white-strong;

    @define-color window_bg_color @surface-strong;
    @define-color window_fg_color @white-strong;

    @define-color view_bg_color @surface-strongest;
    @define-color view_fg_color @white-strong;

    @define-color headerbar_bg_color @surface-moderate;
    @define-color headerbar_fg_color @white-strong;
    @define-color headerbar_border_color @surface-moderate;
    @define-color headerbar_backdrop_color transparent;
    @define-color headerbar_shade_color transparent;

    @define-color card_bg_color @white-weakest;
    @define-color card_fg_color @white-strong;
    @define-color card_shade_color @white-weak;

    @define-color dialog_bg_color @surface-weak;
    @define-color dialog_fg_color @white-strong;

    @define-color popover_bg_color @surface-weakest;
    @define-color popover_fg_color @white-strong;

    @define-color shade_color @black-strongest;
    @define-color scrollbar_outline_color @white-weakest;

    @define-color borders transparent;


    window contents {
        background: @surface-strongest;
        box-shadow: none;
    }

    popover contents, popover arrow {
    	background: @surface-weakest;
    }

    .solid-csd {
    	padding: 0;
    }

    headerbar {
        padding: 0.3em;
        padding-top: calc(0.3em + 3px);
    }

    window {
        border-bottom-left-radius: 0px;
        border-bottom-right-radius:0px;
        border-top-left-radius: 0px;
        border-top-right-radius: 0px;
    }
  '';
  home.sessionVariables = {
    GTK_THEME = "phocus";
  };

  ### -- cursor
  home.pointerCursor = {
    name = "phinger-cursors";
    package = pkgs.phinger-cursors;
    size = 32;
  };
  gtk = {
    cursorTheme = {
      name = "phinger-cursors";
      package = pkgs.phinger-cursors;
    };
    gtk3.extraConfig = {
      Settings = ''
        gtk-application-prefer-dark-theme=1
      '';
    };
    gtk4.extraConfig = {
      Settings = ''
        gtk-application-prefer-dark-theme=1
      '';
    };
  };
  home.sessionVariables = {
    XCURSOR_THEME = "phinger-cursors";
    XCURSOR_SIZE = "32";
  };

  ### -- terminal 
  programs.foot = {
    enable = true;
    settings = {
      main = {
        font = "Liga SFMono Nerd Font:size=11";
        pad = "27x27";
        dpi-aware = "no";
        notify = "${pkgs.libnotify}/bin/notify-send -a foot -i foot \${title} \${body}";
      };
      mouse.hide-when-typing = "yes";
      scrollback.lines = 32768;
      url.launch = "${pkgs.xdg-utils}/bin/xdg-open \${url}";
      tweak.grapheme-shaping = "yes";
      cursor.style = "beam";
      colors = with config.colorscheme.colors; {
        background = "${base00}";
        foreground = "${base06}";
        regular0 = "${base00}";
        regular1 = "${base0B}";
        regular2 = "${base0C}";
        regular3 = "${base0D}";
        regular4 = "${base07}";
        regular5 = "${base0F}";
        regular6 = "${base09}";
        regular7 = "${base04}";
        bright0 = "${base03}";
        bright1 = "${base0B}";
        bright2 = "${base0C}";
        bright3 = "${base0D}";
        bright4 = "${base07}";
        bright5 = "${base0F}";
        bright6 = "${base09}";
        bright7 = "${base06}";
      };
    };
  };

  ### -- notifications 
  services.dunst = {
    enable = true;
    package = inputs'.nixpkgs-wayland.packages.dunst.overrideAttrs (old: {
      __contentAddressed = true;
    });
    settings = with config.colorscheme.colors; {
      global = {
        # gen settings
        follow = "mouse";
        width = 300;
        origin = "top-left";
        notification_limit = 0;
        offset = "18x18";
        icon_position = "off";
        # progress
        progress_bar_height = 9;
        progress_bar_frame_width = 0;
        # other gen
        padding = 18;
        horizontal_padding = 18;
        frame_width = 0;
        gap_size = 9;
        font = "Liga SFMono Nerd Font 11";
        format = "<span size='x-large' font_desc='Liga SFMono Nerd Font 9' weight='bold' foreground='#${base04}'>%a</span>\\n%s\\n%b";
        show_indicators = false;
        mouse_left_click = "do_action";
        mouse_middle_click = "close_all";
        mouse_right_click = "close_current";
        ellipsize = "end";
        markup = "full";
      };

      # disable notifs in fullscreen
      fullscreen_delay_everything = { fullscreen = "delay"; };

      # colors
      urgency_low = {
        timeout = 3;
        background = "#${baseBLEND}";
        foreground = "#${base04}";
        highlight = "#${base0E}";
      };
      urgency_normal = {
        timeout = 6;
        background = "#${baseBLEND}";
        foreground = "#${base04}";
        highlight = "#${base08}";
      };
      urgency_critical = {
        timeout = 0;
        background = "#${baseBLEND}";
        foreground = "#${base04}";
        highlight = "#${base0C}";
      };
    };
  };
  services.poweralertd.enable = true;

  ### -- browsing
  programs.firefox = {
    enable = true;
    package =
      if pkgs.stdenv.hostPlatform.isDarwin
      then pkgs.firefox-bin
      else pkgs.firefox;
    profiles =
      let
        userChrome = with config.colorscheme.colors; ''
          :root {
            --srf-primary: #${base00};
            --srf-secondary: #${base01};
            --srf-text: #${base04};
            --srf-accent: #${base0C};
          }
          window,
          #main-window,
          #toolbar-menubar,
          #TabsToolbar,
          #PersonalToolbar,
          #navigator-toolbox,
          #sidebar-box {
            background-color: var(--srf-primary) !important;
            -moz-appearance: none !important;
            background-image: none !important;
            border: none !important;
            box-shadow: none !important;
          }
          ::selection {
            background-color: var(--srf-accent);
            color: var(--srf-primary);
          }
          :root {
            --tabs-border: transparent !important;
          }
          .tab-background {
            border: none !important;
            border-radius: 0!important;
            margin: 0!important;
            margin-left: -1.6px!important;
            padding: 0!important;
          }
          .tab-background[selected='true'] {
            -moz-appearance: none !important;
            background-image: none !important;
            background-color: var(--srf-secondary)!important;
          }
          .tabbrowser-tabs {
            border: none !important;
            opacity: 0 !important;
          }
          .tabbrowser-tab::before, .tabbrowser-tab::after{
            opacity: 0 !important;
            border-left: none !important;
          }
          .titlebar-placeholder {
            border: none !important;
          }
          .tab-line {
            display: none !important;
          }
          #back-button,
          #forward-button,
          #whats-new-menu-button,
          #star-button,
          #pocket-button,
          #save-to-pocket-button
          #pageActionSeparator,
          #pageActionButton,
          #reader-mode-button,
          #urlbar-zoom-button,
          #identity-box,
          #PanelUI-button,
          #tracking-protection-icon-container {
            display: none !important;
          }
          #context-navigation,
          #context-savepage,
          #context-pocket,
          #context-sendpagetodevice,
          #context-selectall,
          #context-viewsource,
          #context-inspect-a11y,
          #context-sendlinktodevice,
          #context-openlinkinusercontext-menu,
          #context-bookmarklink,
          #context-savelink,
          #context-savelinktopocket,
          #context-sendlinktodevice,
          #context-searchselect,
          #context-sendimage,
          #context-print-selection,
          #context_bookmarkTab,
          #context_moveTabOptions,
          #context_sendTabToDevice,
          #context_reopenInContainer,
          #context_selectAllTabs,
          #context_closeTabOptions {
            display: none !important;
          }
          #save-to-pocket-button {
            visibility: hidden !important;
          }
          .titlebar-spacer {
            display: none !important;
          }
          .tabbrowser-tab:not([pinned]) .tab-close-button {
            display: none !important;
          }
          .tabbrowser-tab:not([pinned]) .tab-icon-image {
            display: none !important;
          }
          #navigator-toolbox::after {
            border-bottom: 0px !important;
            border-top: 0px !important;
          }
          #nav-bar {
            background: var(--srf-secondary) !important;
            border: none !important;
            box-shadow: none !important;
            margin-top: 0px !important;
            border-top-width: 0px !important;
            margin-bottom: 0px !important;
            border-bottom-width: 0px !important;
          }
          #history-panel,
          #sidebar-search-container,
          #bookmarksPanel {
            background: var(--srf-primary) !important;
          }
          #search-box {
            -moz-appearance: none !important;
            background: var(--srf-primary) !important;
            border-radius: 6px !important;
          }
          #sidebar-search-container {
            background-color: var(--srf-primary) !important;
          }
          #sidebar-icon {
            display: none !important;
          }
          .sidebar-placesTree {
            color: var(--srf-text) !important;
          }
          #sidebar-switcher-target {
            color: var(--srf-text) !important;
          }
          #sidebar-header {
            background: var(--srf-primary) !important;
          }
          #sidebar-box {
            --sidebar-background-color: var(--srf-primary) !important;
          }
          #sidebar-splitter {
            border: none !important;
            opacity: 1 !important;
            background-color: var(--srf-primary) !important;
          }
          .urlbarView {
            display: none !important;
          }
          #urlbar-input-container {
            background-color: var(--srf-secondary) !important;
            border: 1px solid rgba(0, 0, 0, 0) !important;
          }
          #urlbar-container {
            margin-left: 8px !important;
          }
          #urlbar[focused='true'] > #urlbar-background {
            box-shadow: none !important;
          }
          .urlbarView-url {
            color: var(--srf-text) !important;
          }
  
        '';
        userContent = with config.colorscheme.colors; ''
          :root {
            scrollbar-width: none !important;
          }
          @-moz-document url(about:privatebrowsing) {
            :root {
              scrollbar-width: none !important;
            }
          }
           @-moz-document url("about:newtab"), url("about:home") {
            body {
              background-color: #${base01} !important;
            }
            .search-wrapper .logo-and-wordmark .logo {
              background-image: url("https://raw.githubusercontent.com/NixOS/nixos-artwork/master/logo/nixos-white.png") !important;
              background-size: 100% !important;
              height: 250px !important;
              width: 500px !important;
            }
            .icon-settings,
            .body-wrapper,
            .SnippetBaseContainer,
            .search-handoff-button,
            .search-wrapper .logo-and-wordmark .wordmark,
            .search-wrapper .search-inner-wrapper,
            .search-wrapper input {
              display: none !important;
            }
          }
        '';

        settings = {
          "browser.ctrlTab.recentlyUsedOrder" = false;
          "browser.uidensity" = 1;
          "browser.urlbar.update1" = true;
          "privacy.trackingprotection.enabled" = true;
          "privacy.trackingprotection.socialtracking.enabled" = true;
          "privacy.trackingprotection.socialtracking.annotate.enabled" = true;
          "services.sync.declinedEngines" = "addons,prefs";
          "services.sync.engine.addons" = false;
          "services.sync.engineStatusChanged.addons" = true;
          "services.sync.engine.prefs" = false;
          "services.sync.engineStatusChanged.prefs" = true;
          "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
          "gfx.webrender.all" = true;
          "general.smoothScroll" = true;
        };
      in
      {
        home = {
          id = 0;
          inherit settings;
          inherit userChrome;
          inherit userContent;
          extensions = with config.nur.repos.rycee.firefox-addons; [
            add-custom-search-engine
            amp2html
            betterttv
            reddit-enhancement-suite
            tridactyl
            ublock-origin
            umatrix
          ];
        };
      };
  };

  ### -- apps
  programs.zathura = {
    enable = true;
    options = with config.colorscheme.colors; {
      completion-bg = "#${base02}";
      completion-fg = "#${base0C}";
      completion-highlight-bg = "#${base0C}";
      completion-highlight-fg = "#${base02}";
      default-bg = "#${base00}";
      default-fg = "#${base01}";
      highlight-active-color = "#${base0D}";
      highlight-color = "#${base0A}";
      index-active-bg = "#${base0D}";
      inputbar-bg = "#${base00}";
      inputbar-fg = "#${base05}";
      notification-bg = "#${base0B}";
      notification-error-bg = "#${base08}";
      notification-error-fg = "#${base00}";
      notification-fg = "#${base00}";
      notification-warning-bg = "#${base08}";
      notification-warning-fg = "#${base00}";
      recolor = true;
      recolor-darkcolor = "#${base06}";
      recolor-keephue = true;
      recolor-lightcolor = "#${base00}";
      selection-clipboard = "clipboard";
      statusbar-bg = "#${base01}";
    };
  };
  programs = {
    obs-studio = {
      enable = true;
      plugins = with pkgs.obs-studio-plugins; [
        wlrobs
        obs-pipewire-audio-capture
      ];
    };
    discocss = {
      enable = false;
      discordAlias = true;
      discordPackage = pkgs.discord-canary;
      css = with config.colorscheme.colors; ''
        /* monospaced font */
        * { font-family: "Liga SFMono Nerd Font" !important; }
        /* themeing*/
        .theme-dark {
          --background-primary: #${base00};
          --background-secondary: #${base01};
          --background-tertiary: #${base03};
          --background-secondary-alt: #${base02};
          --channeltextarea-background: #${base01};
          --interactive-muted: #${base0A};
          --background-floating: #${base01};
          --text-normal: #${base06};
          --header-primary: #${base05};
          --interactive-active: #${base0E};
          --background-accent: #${base01};
        }
        .theme-dark .container-1D34oG {
          background-color: #${base00};
        }
        .categoryHeader-O1zU94, .theme-dark .autocomplete-1vrmpx {
          background-color: #${base00};
        }
        .theme-dark .selected-1Tbx07 {
          background-color: #${base02};
        }
        /* minimal looks*/
        [aria-label="Servers sidebar"],
        [class*="chat-"] > [class*="content-"]::before,
        [class*="repliedMessage-"]::before,
        ::-webkit-scrollbar,
        [class*="form-"] [class*="attachWrapper-"],
        [class*="form-"] [class*="buttons-"],
      '';
    };
  };

  ### -- bar
  programs.eww = {
    enable = true;
    package = inputs.eww.packages.${system}.eww-wayland;
    configDir =
      let
        ewwYuck = pkgs.writeText "eww.yuck" ''
            (defwidget bar []
              (centerbox :orientation "v"
                         :halign "center"
                (box :class "segment-top"
                     :valign "start"
                     :orientation "v"
                  (tags))
                (box :class "segment-center"
                     :valign "center"
                     :orientation "v"
                  (time)
                  (date))
                (box :class "segment-bottom"
                     :valign "end"
                     :orientation "v"
                  (menu)
                  (brightness)
                  (volume)
                  (battery)
                  (current-tag))))
      
            (defwidget time []
              (box :class "time"
                   :orientation "v"
                hour min type))
      
            (defwidget date []
              (box :class "date"
                   :orientation "v"
                year month day))
      
            (defwidget menu []
              (button :class "icon"
                      :orientation "v"
                      :onclick "''${EWW_CMD} open --toggle notifications-menu"
                 ""))
      
            (defwidget brightness []
              (button :class "icon"
                      :orientation "v"
                (circular-progress :value brightness-level
                                   :thickness 3)))
      
            (defwidget volume []
              (button :class "icon"
                      :orientation "v"
                (circular-progress :value volume-level
                                   :thickness 3)))
      
            (defwidget battery []
              (button :class "icon"
                      :orientation "v"
                      :onclick ""
                (circular-progress :value "''${EWW_BATTERY['macsmc-battery'].capacity}"
                                   :thickness 3)))
      
            (defwidget current-tag []
              (button :class "current-tag"
                      :orientation "v"
                      :onclick "kickoff & disown"
                "''${active-tag}"))
      
            (defvar active-tag "1")
            (defpoll hour :interval "1m" "date +%I")
            (defpoll min  :interval "1m" "date +%M")
            (defpoll type :interval "1m" "date +%p")
      
            (defpoll day   :interval "10m" "date +%d")
            (defpoll month :interval "1h"  "date +%m")
            (defpoll year  :interval "1h"  "date +%y")
      
            ;; this is updated by the helper script
            (defvar brightness-level 66)
            (defvar volume-level 33)

            ;; sway
            (defwidget tags []
              (box :class "tags"
                   :orientation "v"
                   :halign "center"
                (for tag in tags
                  (box :class {active-tag == tag.tag ? "active" : "inactive"}
                    (button :onclick "swaymsg workspace ''${tag.tag} ; ''${EWW_CMD} update active-tag=''${tag.tag}"
                      "''${tag.label}")))))
      
            (defvar tags '[{ "tag": 1, "label": "一" },
                           { "tag": 2, "label": "二" },
                           { "tag": 3, "label": "三" },
                           { "tag": 4, "label": "四" },
                           { "tag": 5, "label": "五" },
                           { "tag": 6, "label": "六" },
                           { "tag": 7, "label": "七" },
                           { "tag": 8, "label": "八" },
                           { "tag": 9, "label": "九" },
                           { "tag": 0, "label": "" }]')
      
            (defwindow bar
              :monitor 0
              :stacking "bottom"
              :geometry (geometry
                          :height "100%"
                          :anchor "left center")
              :exclusive true
              (bar))
      
            (defwindow bar2
              :monitor 1
              :stacking "bottom"
              :geometry (geometry
                          :height "100%"
                          :anchor "left center")
              :exclusive true
              (bar))
          '';

        ewwScss = pkgs.writeText "eww.scss" (with config.colorscheme.colors; ''
          $baseTR: rgba(13,13,13,0.13);
          $base00: #${baseBLEND};
          $base01: #${base01};
          $base02: #${base02};
          $base03: #${base03};
          $base04: #${base04};
          $base05: #${base05};
          $base06: #${base06};
          $base07: #${base07};
          $base08: #${base08};
          $base09: #${base09};
          $base0A: #${base0A};
          $base0B: #${base0B};
          $base0C: #${base0C};
          $base0D: #${base0D};
          $base0E: #${base0E};
          $base0F: #${base0F};
          $baseIBM: #${baseIBM};
      
          * {
            all: unset;
          }
      
          window {
            font-family: "Liga SFMono Nerd Font";
            font-size: 13px;
            background-color: rgba(0,0,0,0);
            color: $base04;
            & > * {
              margin: 0px 0px 12px 12px;
            }
          }
      
          .tags {
            margin-top: 9px;
          }
      
          .active {
            color: $base06;
            padding: 6px 9px 6px 6px;
            background-color: $baseTR;
            border-left: 3px solid $base0C;
          }
      
          .segment-center {
            margin-top: 18px;
            padding: 9px;
          }
      
          .time {
            color: $base06;
            font-weight: bold;
            margin-bottom: 6px;
          }
      
          .date {
            margin-top: 6px;
          }
      
          .icon {
            background-color: $base01;
            padding: 9px;
            margin: 4.5px 0px;
            border-radius: 3px;
          }
      
          .current-tag {
            color: $base00;
            background-color: $base0E;
            padding: 9px;
            margin: 4.5px 0px;
            border-radius: 3px;
          }
        '');

        ewwConf = pkgs.linkFarm "ewwConf" [
          {
            name = "eww.scss";
            path = ewwScss;
          }
          {
            name = "eww.yuck";
            path = ewwYuck;
          }
        ];
      in
      ewwConf;
  };
  systemd.user.services.eww = {
    Unit = {
      Description = "Eww daemon";
      PartOf = [ "graphical-session.target" ];
    };
    Service = {
      Environment =
        let
          dependencies = with pkgs;
            [
              kickoff
              brightnessctl
              pamixer
              coreutils
              sway
            ];
        in
        "PATH=/run/wrappers/bin:${lib.makeBinPath dependencies}";
      ExecStart = "${config.programs.eww.package}/bin/eww daemon --no-daemonize";
      Restart = "on-failure";
    };
    Install.WantedBy = [ "graphical-session.target" ];
  };

  ### -- sleep
  services.swayidle =
    let
      display = status: "swaymsg 'output * power ${status}'";
    in
    {
      enable = true;
      package = inputs'.nixpkgs-wayland.packages.swayidle;

      events = [
        { event = "before-sleep"; command = display "off"; }
        { event = "before-sleep"; command = "swaylock"; }
        { event = "after-resume"; command = display "on"; }
        { event = "lock"; command = display "off"; }
        { event = "unlock"; command = display "on"; }
      ];

      timeouts = [
        { timeout = 300; command = display "off"; resumeCommand = display "on"; }
        { timeout = 310; command = "swaylock"; }
      ];
    };
}

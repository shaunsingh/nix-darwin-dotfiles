{ config
, lib
, pkgs
, ...
}:
with lib; let
  cfg = config.wayland.windowManager.river;

  systemdIntegration = ''
    dbus-update-activation-environment --systemd WAYLAND_DISPLAY XDG_CURRENT_DESKTOP
    systemctl --user start river-session.target
  '';

  genBind = mode:
    let
      binds = with cfg.config; zipLists (attrNames keybindings.${mode}) (attrValues keybindings.${mode});
    in
    toString (forEach binds
      (x: "riverctl map${optionalString (mode == "pointer") "-pointer"} ${
        if mode == "pointer"
        then "normal"
        else mode
      } ${x.fst} ${x.snd}\n"));

  configFile = pkgs.writeShellScript "init" ''
    ### This file was generated with Nix. Don't modify this file directly.

    ${config.lib.shell.exportAll cfg.extraSessionVariables}

    # NORMAL KEYBINDINGS
    ${genBind "normal"}

    # MOUSE BINDINGS
    ${genBind "pointer"}

    # LOCKED KEYBINDINGS
    ${genBind "locked"}

    riverctl declare-mode passthrough
    # PASSTHROUGH KEYBINDINGS
    ${genBind "passthrough"}

    riverctl background-color 0x${cfg.config.backgroundColor}
    riverctl border-width ${toString cfg.config.border.width}
    riverctl border-color-focused 0x${cfg.config.border.color.focused}
    riverctl border-color-unfocused 0x${cfg.config.border.color.unfocused}
    riverctl border-color-urgent 0x${cfg.config.border.color.urgent}
    riverctl focus-follows-cursor ${
      if cfg.config.focusFollowsCursor
      then "normal"
      else "disabled"
    }
    riverctl set-repeat ${cfg.config.repeatRate}

    ${cfg.extraConfig}

    ${lib.optionalString cfg.systemdIntegration systemdIntegration}
    riverctl default-layout ${cfg.config.layoutGenerator.name}
    exec ${cfg.config.layoutGenerator.name} ${cfg.config.layoutGenerator.arguments}
  '';
in
{
  options.wayland.windowManager.river = {
    enable = mkEnableOption "river wayland compositor";

    package = mkOption {
      type = types.package;
      default = pkgs.river;
      defaultText = literalExpression "${pkgs.river}";
      description = "River package to use.";
    };

    systemdIntegration = mkOption {
      type = types.bool;
      default = true;
      description = ''
        Whether to enable <filename>river-session.target</filename> on
        river startup. This links to <filename>graphical-session.target</filename>
      '';
    };

    xwayland = mkOption {
      type = types.bool;
      default = true;
      description = "Enable XWayland.";
    };

    extraSessionVariables = mkOption {
      type = types.attrs;
      default = { };
      description = "Extra session variables set when running the compositor.";
      example = { MOZ_ENABLE_WAYLAND = "1"; };
    };

    config = {
      backgroundColor = mkOption {
        type = types.str;
        default = "0x002b36";
        description = "Background color in rrggbb format.";
        example = "ffffff";
      };

      border = mkOption {
        type = types.submodule {
          options = {
            color = mkOption {
              type = types.submodule {
                options = {
                  focused = mkOption {
                    type = types.str;
                    default = "93a1a1";
                    description = "Focused border color.";
                  };

                  unfocused = mkOption {
                    type = types.str;
                    default = "586e75";
                    description = "Unfocused border color.";
                  };

                  urgent = mkOption {
                    type = types.str;
                    default = "ff0000";
                    description = "Urgent border color.";
                  };
                };
              };
            };

            width = mkOption {
              type = types.int;
              default = 2;
              description = "Width of the border in pixels.";
            };
          };
        };
      };

      focusFollowsCursor = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to focus views with the mouse cursor.";
      };

      layoutGenerator = mkOption {
        type = types.submodule {
          options = {
            name = mkOption {
              type = types.str;
              default = "rivertile";
              description = "Name of the layout generator executable.";
            };

            arguments = mkOption {
              type = types.str;
              default = "-view-padding 6 -outer-padding 8";
              description = "Arguments passed to the layout generator.";
            };
          };
        };
      };

      repeatRate = mkOption {
        type = types.str;
        default = "50 300";
        description = "The repeat rate of the compositor.";
      };

      keybindings = mkOption {
        type = types.attrs;

        default = {
          normal = { };
          locked = { };
          passthrough = { };
          pointer = { };
        };

        description = "An attribute set that assigns a key press to an action in a mode using a key symbol.";

        example = lib.mkOptionDefault {
          normal = {
            "Alt Q" = "close";
            "Alt Return" = "spawn foot";
          };

          locked = {
            "None XF86AudioRaiseVolume" = "spawn 'pamixer -i 5'";
            "None XF86AudioLowerVolume" = "spawn 'pamixer -d 5'";
          };

          passthrough = {
            "Alt F11" = "enter-mode normal";
          };

          pointer = {
            "Alt BTN_LEFT" = "move-view";
          };
        };
      };
    };

    extraConfig = mkOption {
      type = types.lines;
      default = "";
      description = "Extra lines appended to <filename>$XDG_CONFIG_HOME/river/init</filename>";
    };
  };

  config = mkIf cfg.enable {
    home.packages =
      [ (cfg.package.override { xwaylandSupport = cfg.xwayland; }) ]
      ++ (optional cfg.xwayland pkgs.xwayland);

    systemd.user.targets.river-session = mkIf cfg.systemdIntegration {
      Unit = {
        Description = "River compositor session";
        Documentation = "man:systemd.special(7)";
        BindsTo = [ "graphical-session.target" ];
        Wants = [ "graphical-session-pre.target" ];
        After = [ "graphical-session-pre.target" ];
      };
    };

    xdg.configFile."river/init".source = configFile;
  };
}

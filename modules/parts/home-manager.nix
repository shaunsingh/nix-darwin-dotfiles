{ config, lib, inputs, withSystem, ... }:

let
  inherit lib;
  inherit (lib) types;

  cfg = config.parts.homeConfigurations;
  configurations = __mapAttrs (_: value: value._home) cfg;

  homeOpts = opts@{ config, lib, name, ... }: {
    options = {
      system = lib.mkOption {
        type = types.enum [
          "aarch64-darwin"
          "aarch64-linux"
          "x86_64-darwin"
          "x86_64-linux"
        ];

        description = "System architecture for the configuration.";
      };

      agenix = lib.mkEnableOption "agenix";

      stateVersion = lib.mkOption {
        type = types.str;
        description = "home-manager state version, changing this value DOES NOT update your config.";
      };

      modules = lib.mkOption {
        type = types.listOf types.unspecified;
        description = "List of home-manager modules to include in the configuration.";
      };

      _home = lib.mkOption {
        type = types.unspecified;
        readOnly = true;
        description = "Composed home-manager configuration.";
      };
    };

    config._home = withSystem config.system (ctx:
      inputs.home.lib.homeManagerConfiguration {
        # Default nixpkgs for home.nix
        pkgs = inputs.nixpkgs.legacyPackages.${ctx.system};

        modules = config.modules ++ [
          inputs.nix-index-database.hmModules.nix-index

          # Shared configuration across all users
          ../shared/home-manager

          ({ config, lib, pkgs, ... }: {
            _module.args = ctx.extraModuleArgs;
            nixpkgs = removeAttrs ctx.nixpkgs [ "hostPlatform" ];

            home = {
              username = __elemAt (lib.strings.split "@" name) 0;
              inherit (opts.config) stateVersion;

              homeDirectory = lib.mkMerge [
                (lib.mkIf pkgs.stdenv.isDarwin "/Users/${config.home.username}")
                (lib.mkIf pkgs.stdenv.isLinux "/home/${config.home.username}")
              ];
            };
          })
        ] ++ lib.optionals config.agenix [
          inputs.agenix.homeManagerModules.age

          ({ config, ... }: {
            age.identityPaths = [
              "${config.home.homeDirectory}/.ssh/id_ed25519"
              "/etc/ssh/ssh_host_ed25519_key"
            ];
          })
        ];
      }
    );
  };
in
{
  options.parts.homeConfigurations = lib.mkOption {
    type = types.attrsOf (types.submodule homeOpts);
  };

  config.flake.homeConfigurations = configurations;
}
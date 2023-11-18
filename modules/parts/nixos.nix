{ config, lib, inputs, withSystem, ... }:

let
  inherit lib;
  inherit (lib) types;

  cfg = config.parts.nixosConfigurations;
  configurations = __mapAttrs (_: value: value._nixos) cfg;

  nixosOpts = opts@{ config, name, ... }: {
    options = {
      server = lib.mkEnableOption "Host is a headless server configuration.";

      system = lib.mkOption {
        type = types.enum [ "aarch64-linux" "x86_64-linux" ];
        description = "System architecture for the configuration.";
      };

      stateVersion = lib.mkOption {
        type = types.str;
        description = "NixOS state version, changing this value DOES NOT update your system.";
      };

      wsl = lib.mkEnableOption "nixos-wsl";

      modules = lib.mkOption {
        type = types.listOf types.unspecified;
        description = "List of NixOS modules to include in the configuration.";
      };

      _nixos = lib.mkOption {
        type = types.unspecified;
        readOnly = true;
        description = "Composed NixOS configuration.";
      };
    };

    config._nixos = withSystem config.system (ctx:
      inputs.nixpkgs.lib.nixosSystem {
        modules = config.modules ++ lib.optionals (!config.server) [
          # Shared configuration across all NixOS machines
          ../shared/nixos
        ] ++ [
          ({ pkgs, ... }: {
            inherit (ctx) nix nixpkgs;
            _module.args = ctx.extraModuleArgs;
            networking.hostName = name;
            system.stateVersion = config.stateVersion;
            environment.systemPackages = ctx.basePackagesFor pkgs;
          })
        ] ++ lib.optionals config.wsl [
          inputs.nixos-wsl.nixosModules.wsl

          ({ lib, pkgs, ... }: {
            wsl.wslConf.network.hostname = name;
            system.build.installBootLoader = lib.mkForce "${pkgs.coreutils}/bin/true";
          })
        ];
      }
    );
  };
in
{
  options.parts.nixosConfigurations = lib.mkOption {
    type = types.attrsOf (types.submodule nixosOpts);
  };

  config.flake.nixosConfigurations = configurations;
}
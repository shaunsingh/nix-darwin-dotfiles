{ config, lib, inputs, withSystem, ... }:

let
  inherit lib;
  inherit (lib) types;

  cfg = config.parts.darwinConfigurations;
  configurations = __mapAttrs (_: value: value._darwin) cfg;

  darwinOpts = { config, name, ... }: {
    options = {
      system = lib.mkOption {
        type = types.enum [ "aarch64-darwin" "x86_64-darwin" ];
        description = "System architecture for the configuration.";
      };

      stateVersion = lib.mkOption {
        type = types.int;
        description = "nix-darwin state version, changing this value DOES NOT update your system.";
      };

      modules = lib.mkOption {
        type = types.listOf types.unspecified;
        description = "List of nix-darwin modules to include in the configuration.";
      };

      _darwin = lib.mkOption {
        type = types.unspecified;
        readOnly = true;
        description = "Composed nix-darwin configuration.";
      };
    };

    config._darwin = withSystem config.system (ctx:
      inputs.darwin.lib.darwinSystem {
        inherit inputs;
        inherit (ctx) system;

        modules = config.modules ++ [
          ({ pkgs, ... }: {
            inherit (ctx) nix;
            nixpkgs = removeAttrs ctx.nixpkgs [ "hostPlatform" ];
            _module.args = ctx.extraModuleArgs;
            networking.hostName = name;
            system.stateVersion = config.stateVersion;
            environment.systemPackages = ctx.basePackagesFor pkgs;
          })
        ];
      }
    );
  };
in
{
  options.parts.darwinConfigurations = lib.mkOption {
    type = types.attrsOf (types.submodule darwinOpts);
  };

  config.flake.darwinConfigurations = configurations;
}
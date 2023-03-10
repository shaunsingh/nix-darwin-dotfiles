{
  description = "Nix Environment";

  inputs = {
    ### --- nixpkgs

    # https://hydra.nixos.org/jobset/mobile-nixos/unstable/evals
    # these evals have a cross-compiled stdenv available
    mobile.url = "github:nixos/nixpkgs/e5530aba13caff5a4f41713f1265b754dc2abfd8";

    # other nixpkgs branches
    master.url = "github:nixos/nixpkgs/master";
    unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    stable.url = "github:nixos/nixpkgs/nixos-21.11";
    nur.url = "github:nix-community/NUR";

    # Default Nixpkgs for packages
    nixpkgs.follows = "master";

    ### --- platform support

    nixos-wsl.url = "github:nix-community/NixOS-WSL";
    nix-darwin.url = "github:lnl7/nix-darwin";
    nixos-apple-silicon.url = "github:tpwrules/nixos-apple-silicon";

    ### --- flakes

    # manage home env
    home-manager.url = "github:nix-community/home-manager";

    # secrets
    agenix.url = "github:ryantm/agenix";

    # formatting
    nixpkgs-fmt.url = "github:nix-community/nixpkgs-fmt";

    # overlays
    nixpkgs-wayland.url = "github:nix-community/nixpkgs-wayland";
    rust-overlay.url = "github:oxalica/rust-overlay";
    neovim-nightly-overlay.url = "github:nix-community/neovim-nightly-overlay";

    # desktop
    eww.url = "github:elkowar/eww";

    # theme
    base16 = {
      url = "github:shaunsingh/base16.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    base16-oxocarbon = {
      url = "github:shaunsingh/base16-oxocarbon";
      flake = false;
    };

    # nyxt extensions
    nx-kaomoji-src = {
      url = "github:aartaka/nx-kaomoji";
      flake = false;
    };
    nx-dark-reader-src = {
      url = "github:aartaka/nx-dark-reader";
      flake = false;
    };
    nx-search-engines-src = {
      url = "github:aartaka/nx-search-engines";
      flake = false;
    };
    nx-notmuch-src = {
      url = "github:igoralmeida/nx-notmuch";
      flake = false;
    };

    ### --- de-duplicate flake inputs

    agenix.inputs.nixpkgs.follows = "nixpkgs";
    nixos-wsl.inputs.nixpkgs.follows = "nixpkgs";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
    nixos-apple-silicon.inputs.nixpkgs.follows = "nixpkgs";
    rust-overlay.inputs.nixpkgs.follows = "nixpkgs";
    eww.inputs.nixpkgs.follows = "nixpkgs";
    eww.inputs.rust-overlay.follows = "rust-overlay";
  };

  outputs = {
    self,
    nixpkgs,
    home-manager,
    nur,
    agenix,
    nix-darwin,
    ...
  } @ inputs: let
    ### --- utility
    # get the revision of an input
    versionOf = input: input.rev;

    # genattr for all supported systems of this config
    forAllSystems = nixpkgs.lib.genAttrs [
      "aarch64-linux"
      "i686-linux"
      "x86_64-linux"
      "aarch64-darwin"
      "x86_64-darwin"
    ];

    # builtins.readDir, but recurses into directories.
    readDirRecursive = dir:
      nixpkgs.lib.mapAttrs
      (name: type:
        if type == "directory"
        then readDirRecursive "${dir}/${name}"
        else type)
      (builtins.readDir dir);

    # adapatation of f2k's config that handles recursive reads
    filterNixFiles = k: v: v == "regular" && nixpkgs.lib.hasSuffix ".nix" k;
    importNixFiles = path:
      with nixpkgs.lib;
        (lists.forEach (mapAttrsToList (name: _: path + ("/" + name))
            (filterAttrs filterNixFiles (readDirRecursive path))))
        import;

    ### --- import stuff I should probably upstream
    myNixosModules = importNixFiles ./modules/nixos;
    myDarwinModules = importNixFiles ./modules/darwin;
    myHomeManagerModules = importNixFiles ./modules/home-manager;

    ### --- add overlays
    overlays = with inputs;
      [
        (final: prev: let
          inherit (final) system;
        in
          {
            eww-wayland-git = eww.packages.${system}.eww-wayland;
          }
          // {
            /*
            one can access these branches like so:

            `pkgs.stable.mpd'
            `pkgs.master.linuxPackages_xanmod'
            */
            master = import master {inherit config system;};
            unstable = import unstable {inherit config system;};
            stable = import stable {inherit config system;};
            mobile = import mobile {inherit config system;};
          })

        rust-overlay.overlays.default
        neovim-nightly-overlay.overlay
        nixpkgs-wayland.overlay
      ]
      ++ (importNixFiles ./overlays);

    ### --- configure nixpkgs
    config = {
      allowBroken = true;
      allowUnfree = true;
      allowUnfreePredicate = _: true;
      tarball-ttl = 0;
      contentAddressedByDefault = false;
    };

    ### --- generalized mkConfig between nixOS/macOS
    mkSystemConfig = {
      system,
      modules,
      hm-modules,
      useNur ? true,
      useHomeManager ? true,
      withMail ? false,
      withSway ? false,
      withRiver ? false,
      withBindings ? false,
      isGui ? (withSway || withRiver),
      isWayland ? (withSway || withRiver),
      isDarwin ? nixpkgs.lib.hasSuffix "-darwin" system,
      ...
    }:
      (
        if isDarwin
        then nix-darwin.lib.darwinSystem
        else nixpkgs.lib.nixosSystem
      ) {
        inherit system;
        specialArgs = {inherit nixpkgs overlays inputs withSway withRiver isGui isWayland isDarwin;};
        modules =
          modules
          ++ [
            agenix.nixosModules.default
            ./users/shared
            {
              nix = import ./nix-settings.nix {
                inherit inputs system nixpkgs;
                max-jobs = 8;
              };
              nixpkgs = {inherit config overlays;};
            }
          ]
          ++ (
            if isDarwin
            then
              (myDarwinModules
                ++ [
                  ./users/shared/darwin
                ])
            else
              (myNixosModules
                ++ [
                  ./users/shared/nixos
                ]
                ++ nixpkgs.lib.lists.optionals useHomeManager ([
                    ./users/shaunsingh
                  ]
                  ++ nixpkgs.lib.lists.optionals withBindings [
                    ./users/shaunsingh/bindings.nix
                  ])
                ++ nixpkgs.lib.lists.optionals withSway [./users/shared/nixos/sway]
                ++ nixpkgs.lib.lists.optionals withRiver [./users/shared/nixos/river]
                ++ nixpkgs.lib.lists.optionals isGui [./users/shared/nixos/hardware]
                ++ nixpkgs.lib.lists.optionals isWayland [./users/shared/nixos/wayland])
          )
          ++ nixpkgs.lib.lists.optionals useHomeManager [
            (
              if isDarwin
              then home-manager.darwinModules.home-manager
              else home-manager.nixosModules.home-manager
            )
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.shauryasingh = {
                imports =
                  hm-modules
                  ++ myHomeManagerModules
                  ++ [
                    inputs.base16.hmModule
                    ./home
                    ./home/cli
                  ]
                  ++ nixpkgs.lib.lists.optionals useNur [nur.hmModules.nur]
                  ++ nixpkgs.lib.lists.optionals withMail [./home/mail.nix]
                  ++ nixpkgs.lib.lists.optionals withSway [./home/wm/sway.nix]
                  ++ nixpkgs.lib.lists.optionals withRiver [./home/wm/river.nix]
                  ++ nixpkgs.lib.lists.optionals isGui [
                    ./home/xdg.nix
                    ./home/desktop/gtk.nix
                    ./home/desktop/cursor.nix
                    ./home/desktop/apps
                    ./home/desktop/bars/eww.nix
                    ./home/desktop/browsers/nyxt.nix
                    ./home/desktop/browsers/firefox.nix
                  ]
                  ++ nixpkgs.lib.lists.optionals isWayland [
                    ./home/desktop/kanshi.nix
                    ./home/desktop/swaybg.nix
                    ./home/lockers/waylock.nix
                    ./home/desktop/term/foot.nix
                    ./home/launchers/kickoff.nix
                    ./home/desktop/notifications/dunst.nix
                  ];
              };
              home-manager.extraSpecialArgs = {
                inherit nixpkgs inputs withSway withRiver isGui isWayland isDarwin;
              };
            }
          ];
      };
  in {
    ### --- linux configurations

    nixosConfigurations = {
      # macbook via UTM virtual machine
      nixos-utm-aarch64 = mkSystemConfig {
        system = "aarch64-linux";
        modules = [./hosts/nixos-utm-aarch64];
        hm-modules = [];
      };

      # VMWare guest (windows 11 host)
      nixos-virtualboy-x86_64 = mkSystemConfig {
        system = "x86_64-linux";
        modules = [./hosts/nixos-virtualboy-x86_64];
        hm-modules = [];
      };

      # asahi on M1 Macbook pro
      nixos-asahi-aarch64 = mkSystemConfig {
        system = "aarch64-linux";
        withMail = true;
        withSway = true;
        withBindings = true;
        modules = [
          ./hosts/nixos-asahi-aarch64
          ./users/shared/nixos/hardware
        ];
        hm-modules = [
          ./home/themes/oxocarbon-dark.nix
          ./home/desktop/apps/zathura.nix
          ./home/desktop/services/asahi-battery-threshold.nix
        ];
      };

      # WSL2 on Win11
      nix-wsl-x86_64 = mkSystemConfig {
        system = "x86_64-linux";
        modules = [./hosts/nix-wsl-x86_64];
        hm-modules = [];
      };
    };

    ### --- darwin configurations

    darwinConfigurations = {
      nix-darwin-aarch64 = mkSystemConfig {
        system = "aarch64-darwin";
        modules = [./hosts/nix-darwin-aarch64];
        hm-modules = [
          inputs.base16.hmModule
          ./home/themes/oxocarbon.nix

          ./home/cli

          ./home/desktop/browsers/firefox.nix
          ./home/desktop/term/alacritty.nix
        ];
      };
    };

    ### --- stuff I should probably upstream but don't

    inherit (nixpkgs) overlays;
    nixosModules = myNixosModules;
    darwinModules = myDarwinModules;
    homeManagerModules = myHomeManagerModules;

    ### --- make it easy to bootstrap my config

    devShells = forAllSystems (system: let
      pkgs = nixpkgs.legacyPackages.${system};
    in
      import ./shell.nix {inherit pkgs;});

    ### --- formatting

    formatter =
      forAllSystems (system: inputs.nixpkgs-fmt.defaultPackage.${system});

    ### --- make it easier to `nix build *` linux machines

    utm =
      self.nixosConfigurations.nixos-utm-aarch64.config.system.build.toplevel;
    virtualboy =
      self.nixosConfigurations.nixos-virtualboy-x86_64.config.system.build.toplevel;
    wsl2 =
      self.nixosConfigurations.nix-wsl-x86_64.config.system.build.toplevel;
    asahi =
      self.nixosConfigurations.nixos-asahi-aarch64.config.system.build.toplevel;
  };

  nixConfig = {
    commit-lockfile-summary = "flake: bump inputs";
    substituters = [
      "https://cache.nixos.org?priority=10"
      "https://cache.ngi0.nixos.org/"
      "https://nix-community.cachix.org?priority=5"
      "https://nixpkgs-wayland.cachix.org"
    ];
  };
}

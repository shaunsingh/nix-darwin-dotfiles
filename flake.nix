{
  description = "Shaurya's Nix Environment";

  inputs = {
    # hydra.nixos.org/jobset/mobile-nixos/unstable/evals
    # these evals have a cross-compiled stdenv available
    nixpkgs.url = "github:nixos/nixpkgs/9b97ad7b4330aacda9b2343396eb3df8a853b4fc";
    nur.url = "github:nix-community/NUR";
    # Nix-Darwin
    darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # HM-manager for dotfile/user management
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # Asahi linux
    nixos-apple-silicon = {
      url = "github:tpwrules/nixos-apple-silicon/main";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    asahi-alsa-src = {
      url = "github:AsahiLinux/alsa-ucm-conf-asahi";
      flake = false;
    };
    nixpkgs-wayland = {
      url = "github:nix-community/nixpkgs-wayland";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    phocus-src = {
      url = "github:phocus/gtk";
      flake = false;
    };
    swayfx = {
      url = "github:WillPower3309/swayfx";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # Window Management
    yabai-src = {
      url = "github:FelixKratz/yabai";
      flake = false;
    };
    # SFMono w/ patches
    sf-mono-liga-src = {
      url = "github:shaunsingh/SFMono-Nerd-Font-Ligaturized";
      flake = false;
    };
    # Other apple fonts 
    sf-pro-src = {
      url = "https://devimages-cdn.apple.com/design/resources/download/SF-Pro.dmg";
      flake = false;
    };
    sf-compact-src = {
      url = "https://devimages-cdn.apple.com/design/resources/download/SF-Compact.dmg";
      flake = false;
    };
    ny-src = {
      url = "https://devimages-cdn.apple.com/design/resources/download/NY.dmg";
      flake = false;
    };
    # Terminal emulator
    alacritty-src = {
      url = "github:fee1-dead/alacritty/ligatures_harfbuzz";
      flake = false;
    };
    # Themeing
    base16 = {
      url = "github:shaunsingh/base16.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # IBM-Carbon-Theme (see IBM-design: colors)
    base16-oxocarbon = {
      url = "github:nyoom-engineering/base16-oxocarbon";
      flake = false;
    };
    # Neovim Nightly
    neovim-nightly = {
      url = "github:neovim/neovim?dir=contrib";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # Rust Nightly
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # bar
    sketchybar-src = {
      url = "github:FelixKratz/SketchyBar";
      flake = false;
    };
    eww = {
      url = "github:elkowar/eww";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.rust-overlay.follows = "rust-overlay";
    };
    # Firefox for darwin
    firefox-overlay = {
      url = "github:bandithedoge/nixpkgs-firefox-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # minecraft
    prismmc = {
      url = "github:PrismLauncher/PrismLauncher";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = { self, nixpkgs, darwin, home-manager, ... }@inputs: {
    # asahi m1 macbook running wayland
    nixosConfigurations.shaunsingh-laptop = nixpkgs.lib.nixosSystem {
      system = "aarch64-linux";
      specialArgs = { inherit inputs; };
      modules = [
        ./hosts/mbp-m1-linux
        inputs.nixos-apple-silicon.nixosModules.default
        home-manager.nixosModule
        {
          home-manager = {
            useGlobalPkgs = true;
            useUserPackages = true;
            extraSpecialArgs = {
              inherit (inputs)
                base16-oxocarbon;
            };
            users.shauryasingh = {
              imports = [
                inputs.base16.hmModule
                # env
                ./modules/home.nix

                # hardware
                ./modules/kanshi.nix
                ./modules/backlight.nix

                # themeing 
                ./modules/theme.nix
                ./modules/gtk.nix
                ./modules/cursor.nix
                ./modules/swaybg.nix

                # window managers
                ./modules/sway.nix
                # ./modules/river.nix
                
                # widgets
                # ./modules/swaylock.nix
                ./modules/rofi.nix
                ./modules/eww.nix
                ./modules/dunst.nix

                # terminal
                ./modules/foot.nix
                # ./modules/alacritty.nix

                # browser
                ./modules/nyxt.nix
                ./modules/firefox.nix

                # other
                # ./modules/prismmc.nix
              ];
            };
          };
        }
      ];
    };
    # m1 macbook running macOS
    darwinConfigurations.shaunsingh-laptop = darwin.lib.darwinSystem {
      system = "aarch64-darwin";
      specialArgs = { inherit inputs; };
      modules = [
        ./hosts/mbp-m1-darwin
        home-manager.darwinModule
        {
          home-manager = {
            useGlobalPkgs = true;
            useUserPackages = true;
            extraSpecialArgs = {
              inherit (inputs)
                base16-oxocarbon;
            };
            users.shauryasingh = {
              imports = [
                inputs.base16.hmModule
                ./modules/home.nix
                ./modules/firefox.nix
                ./modules/theme.nix
                ./modules/alacritty.nix
                ./modules/discord.nix
              ];
            };
          };
        }
      ];
    };
  };
}

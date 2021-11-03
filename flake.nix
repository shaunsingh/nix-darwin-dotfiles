{
  description = "Shaurya's Nix Environment";

  nixConfig = {
    # Add binary cache for neovim-nightly/emacsGcc 
    extra-substituters =
      [ "https://cachix.cachix.org" "https://nix-community.cachix.org" ];
    extra-trusted-public-keys = [
      "cachix.cachix.org-1:eWNHQldwUO7G2VkjpnjDbWwy4KQ/HNxht7H4SSoMckM="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };

  inputs = {
    # All packages should follow latest nixpkgs home-manager & flake-utils
    unstable.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    darwin = {
      url = "github:LnL7/nix-darwin/master";
      inputs.nixpkgs.follows = "unstable";
    };
    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "unstable";
    };
    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.nixpkgs.follows = "unstable";
    };
    # Bar
    spacebar = {
      url = "github:shaunsingh/spacebar/master";
      inputs.nixpkgs.follows = "unstable";
    };
    # Editors
    neovim = {
      url = "github:nix-community/neovim-nightly-overlay";
      inputs.nixpkgs.follows = "unstable";
    };
    emacs = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "unstable";
    };
    # "unpin" all inputs for nix-doom-emacs
    doom-emacs = {
      url = "github:hlissner/doom-emacs/develop";
      flake = false;
    };
    doom-snippets = {
      url = "github:hlissner/doom-snippets/";
      flake = false;
    };
    emacs-so-long =  {
        url = "github:hlissner/emacs-so-long";
        flake = false;
    };
    evil-markdown =  {
        url = "github:Somelauw/evil-markdown";
        flake = false;
    };
    evil-org-mode =  {
        url = "github:hlissner/evil-org-mode";
        flake = false;
    };
    evil-quick-diff =  {
        url = "github:rgrinberg/evil-quick-diff";
        flake = false;
    };
    explain-pause-mode =  {
        url = "github:lastquestion/explain-pause-mode";
        flake = false;
    };
    nix-straight =  {
        url = "github:vlaci/nix-straight.el";
        flake = false;
    };
    nose =  {
        url = "github:emacsattic/nose";
        flake = false;
    };
    ob-racket =  {
        url = "github:xchrishawk/ob-racket";
        flake = false;
    };
    org =  {
        url = "github:emacs-straight/org-mode";
        flake = false;
    };
    org-contrib =  {
        url = "git+https://git.sr.ht/~bzg/org-contrib";
        flake = false;
    };
    org-yt =  {
        url = "github:TobiasZawada/org-yt";
        flake = false;
    };
    php-extras =  {
        url = "github:arnested/php-extras";
        flake = false;
    };
    revealjs =  {
        url = "github:hakimel/reveal.js";
        flake = false;
    };
    rotate-text =  {
        url = "github:debug-ito/rotate-text.el";
        flake = false;
    };
    nix-doom-emacs = {
      url = "github:vlaci/nix-doom-emacs/develop";
      inputs.nixpkgs.follows = "unstable";
      inputs.flake-utils.follows = "flake-utils";
      inputs.doom-emacs.follows = "doom-emacs";
      inputs.doom-snippets.follows = "doom-snippets";
      inputs.emacs-overlay.follows = "emacs";
      inputs.emacs-so-long.follows = "emacs-so-long";
      inputs.evil-markdown.follows = "evil-markdown";
      inputs.evil-org-mode.follows = "evil-org-mode";
      inputs.evil-quick-diff.follows = "evil-quick-diff";
      inputs.explain-pause-mode.follows = "explain-pause-mode";
      inputs.nix-straight.follows = "nix-straight";
      inputs.nose.follows = "nose";
      inputs.ob-racket.follows = "ob-racket";
      inputs.org.follows = "org";
      inputs.org-contrib.follows = "org-contrib";
      inputs.org-yt.follows = "org-yt";
      inputs.php-extras.follows = "php-extras";
      inputs.revealjs.follows = "revealjs";
      inputs.rotate-text.follows = "rotate-text";
    };
  };

  outputs = { self, nixpkgs, spacebar, neovim, nix-doom-emacs, emacs, darwin
    , home-manager, ... }@inputs: {
      darwinConfigurations."shaunsingh-laptop" = darwin.lib.darwinSystem {
        system = "aarch64-darwin";
        modules = [
          ./modules/mac.nix
          ./modules/home.nix
          ./modules/pam.nix
          home-manager.darwinModule
          {
            home-manager = {
              useGlobalPkgs = true;
              useUserPackages = true;
            };
            home-manager.users.shauryasingh = { pkgs, ... }: {
              imports = [ nix-doom-emacs.hmModule ];
              programs.doom-emacs = {
                enable = false;
                doomPrivateDir = ./configs/doom;
                emacsPackage = pkgs.emacsGcc;
              };
            };
          }
          ({ pkgs, lib, ... }: {
            services.nix-daemon.enable = true;
            security.pam.enableSudoTouchIdAuth = true;
            nixpkgs = {
              overlays = [ spacebar.overlay neovim.overlay emacs.overlay ];
              config.allowUnfree = true;
            };
            nix = {
              package = pkgs.nixUnstable;
              extraOptions = ''
                system = aarch64-darwin
                extra-platforms = aarch64-darwin x86_64-darwin
                experimental-features = nix-command flakes
                build-users-group = nixbld
              '';
            };
          })
        ];
      };
    };
}

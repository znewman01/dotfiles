{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/release-21.11";
    home-manager.url = "github:nix-community/home-manager/release-21.11";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    nixpkgs-unstable.url = "github:NixOS/nixpkgs/master";
    home-manager-unstable.url = "github:nix-community/home-manager/master";
    home-manager-unstable.inputs.nixpkgs.follows = "nixpkgs-unstable";

    impermanence.url = "github:nix-community/impermanence";

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      flake = false;
    };
    doom-emacs = {
      url = "github:nix-community/nix-doom-emacs";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.emacs-overlay.follows = "emacs-overlay";
    };
    darwin = {
      url = "github:lnl7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };
    doom-emacs-unstable = {
      url = "github:nix-community/nix-doom-emacs";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
      inputs.emacs-overlay.follows = "emacs-overlay";
    };
  };
  outputs = inputs@{ nixpkgs, home-manager, impermanence, doom-emacs, darwin
    , nixpkgs-unstable, home-manager-unstable, doom-emacs-unstable, ... }:
    let
      stable = { inherit nixpkgs home-manager impermanence doom-emacs; };
      unstable = {
        inherit darwin;
        nixpkgs = nixpkgs-unstable;
        home-manager = home-manager-unstable;
        doom-emacs = doom-emacs-unstable;
      };
    in {
      colmena = {
        meta.nixpkgs = import nixpkgs { system = "x86_64-linux"; };
        defaults = { ... }: {
          imports = [ home-manager.nixosModule impermanence.nixosModule ];
        };
      } // import ./machines { inherit inputs; };

      darwinConfigurations."zjn-mac" = darwin.lib.darwinSystem {
        system = "aarch64-darwin";
        modules = [
          home-manager-unstable.darwinModules.home-manager
          ./machines/zjn-mac
        ];
        specialArgs = { inputs = unstable; };
      };

    };
}

{
  inputs = {
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/master";
    nixpkgs.url = "github:numtide/nixpkgs-unfree";
    nixpkgs.inputs.nixpkgs.follows = "nixpkgs-unstable";

    home-manager.url = "github:nix-community/home-manager/master";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

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
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  nixConfig = {
    extra-substituters = [ "https://nixpkgs-unfree.cachix.org" ];
    extra-trusted-public-keys = [
      "nixpkgs-unfree.cachix.org-1:hqvoInulhbV4nJ9yJOEr+4wxhDV4xq2d1DK7S6Nj6rs="
    ];
  };
  outputs =
    inputs@{ nixpkgs, home-manager, impermanence, doom-emacs, darwin, ... }: {
      nixosConfigurations = {
        zjn-work = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            home-manager.nixosModule
            impermanence.nixosModule
            ./machines/zjn-work
          ];
          specialArgs = inputs;
        };
      };

      darwinConfigurations."zjn-mac" = darwin.lib.darwinSystem {
        system = "aarch64-darwin";
        modules =
          [ home-manager.darwinModules.home-manager ./machines/zjn-mac ];
        specialArgs = inputs;
      };

    };
}

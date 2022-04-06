{
  inputs = {
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/master";
    nixpkgs.url = "github:numtide/nixpkgs-unfree";
    nixpkgs.inputs.nixpkgs.follows = "nixpkgs-unstable";

    flake-utils.url = "github:numtide/flake-utils";

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
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };
  };

  outputs = inputs@{ nixpkgs, darwin, flake-utils, home-manager, impermanence
    , doom-emacs, ... }:
    let
      useSystemNixpkgs = ({ ... }: {
        nix.registry.nixpkgs.flake = nixpkgs;
        nix.nixPath = [ "nixpkgs=${nixpkgs}" ];
      });
    in {
      nixosConfigurations = {
        zjn-work = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            home-manager.nixosModule
            impermanence.nixosModule
            ./machines/zjn-work
            useSystemNixpkgs
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

      devShells = nixpkgs.lib.genAttrs flake-utils.lib.defaultSystems (system:
        let pkgs = nixpkgs.legacyPackages.${system};
        in {
          default = pkgs.mkShell {
            buildInputs = with pkgs; [ nixfmt git-crypt terraform ];
          };
        });
    };
}

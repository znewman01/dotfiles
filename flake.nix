{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/master";

    nixpkgs-unfree.url = "github:numtide/nixpkgs-unfree";
    nixpkgs-unfree.inputs.nixpkgs.follows = "nixpkgs";

    flake-utils.url = "github:numtide/flake-utils";

    home-manager.url = "github:nix-community/home-manager/master";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    impermanence.url = "github:nix-community/impermanence";

    kolide-launcher.url = "github:znewman01/kolide-launcher";
    kolide-launcher.inputs.nixpkgs.follows = "nixpkgs";

    emacs-overlay = {
      url =
        "github:nix-community/emacs-overlay/70e241d5b189982dabc1fe55829475c5c483c89d";
      flake = false;
    };
    doom-emacs-upstream = {
      url =
        "github:doomemacs/doomemacs/c44bc81a05f3758ceaa28921dd9c830b9c571e61";
      flake = false;
    };
    doom-emacs = {
      url = "github:nix-community/nix-doom-emacs";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.doom-emacs.follows = "doom-emacs-upstream";
      inputs.emacs-overlay.follows = "emacs-overlay";
    };
    darwin = {
      url = "github:lnl7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ nixpkgs, darwin, flake-utils, home-manager, impermanence
    , doom-emacs, kolide-launcher, ... }:
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
            kolide-launcher.nixosModules.x86_64-linux.default
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

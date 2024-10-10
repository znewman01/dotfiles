{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/master";

    flake-utils.url = "github:numtide/flake-utils";

    home-manager.url = "github:nix-community/home-manager/master";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    impermanence.url = "github:nix-community/impermanence";

    emacs-overlay = { url = "github:nix-community/emacs-overlay"; };

    darwin = {
      url = "github:lnl7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ nixpkgs, darwin, flake-utils, home-manager, impermanence
    , emacs-overlay, ... }:
    let
      systemOutputs = flake-utils.lib.eachDefaultSystem (system:
        let pkgs = nixpkgs.legacyPackages.${system};
        in rec {
          devShells.default =
            pkgs.mkShell { buildInputs = with pkgs; [ nixfmt git-crypt ]; };
          legacyPackages = import nixpkgs {
            inherit system;
            config.allowUnfree = true;
            overlays = [ emacs-overlay.overlay ];
          };
        });
    in systemOutputs // rec {
      homeManagerModules = (import ./modules).homeManagerModules
        ++ [ impermanence.nixosModules.home-manager.impermanence ];
      modules = (import ./modules).modules
        ++ [ home-manager.nixosModule impermanence.nixosModule ];
      nixosConfigurations = {
        zjn-work = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          pkgs = systemOutputs.legacyPackages."x86_64-linux";
          modules = [
            ./machines/zjn-work
            ({ ... }: { home-manager.users.zjn.imports = homeManagerModules; })
          ] ++ modules;
          specialArgs = inputs;
        };
        zjn-x1prime = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          pkgs = systemOutputs.legacyPackages."x86_64-linux";
          modules = [
            ({ ... }: { home-manager.users.zjn.imports = homeManagerModules; })
            ./machines/zjn-x1prime
          ] ++ modules;
          specialArgs = inputs;
        };
      };
      darwinConfigurations."zjn-air" = darwin.lib.darwinSystem {
        system = "aarch64-darwin";
        pkgs = systemOutputs.legacyPackages."aarch64-darwin";
        modules = [
          home-manager.darwinModules.home-manager
          ./machines/zjn-air
          ({ ... }: {
            home-manager.users.zjn.imports =
              (import ./modules).homeManagerModules;
          })
        ] ++ (import ./modules).modules;
        specialArgs = inputs;
      };
    };
}

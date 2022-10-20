{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/master";

    flake-utils.url = "github:numtide/flake-utils";

    home-manager.url = "github:nix-community/home-manager/master";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    impermanence.url = "github:nix-community/impermanence";

    kolide-launcher.url = "github:znewman01/kolide-launcher";
    kolide-launcher.inputs.nixpkgs.follows = "nixpkgs";

    doom-emacs = {
      url = "github:znewman01/nix-doom-emacs/reuse-straight";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    darwin = {
      url = "github:lnl7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ nixpkgs, darwin, flake-utils, home-manager, impermanence
    , doom-emacs, kolide-launcher, ... }:
    let
      systemOutputs = flake-utils.lib.eachDefaultSystem (system:
        let pkgs = nixpkgs.legacyPackages.${system};
        in rec {
          devShells.default = pkgs.mkShell {
            buildInputs = with pkgs; [ nixfmt git-crypt terraform ];
          };
          packages = import ./packages { inherit pkgs; };
          legacyPackages = import nixpkgs {
            inherit system;
            config.allowUnfree = true;
            overlays = [ (final: prev: packages) ];
          };
        });
    in systemOutputs // rec {
      nixosConfigurations = {
        zjn-work = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          pkgs = systemOutputs.legacyPackages."x86_64-linux";
          modules = [
            home-manager.nixosModule
            impermanence.nixosModule
            kolide-launcher.nixosModules.x86_64-linux.default
            ./machines/zjn-work
          ];
          specialArgs = inputs;
        };
      };
      darwinConfigurations."zjn-mac" = darwin.lib.darwinSystem rec {
        system = "aarch64-darwin";
        pkgs = systemOutputs.legacyPackages."aarch64-darwin";
        modules = [
          home-manager.darwinModules.home-manager
          ./machines/zjn-mac
          ./common/darwin.nix
          ./desktop/darwin.nix
            ({ ... }: {
              home-manager.users.zjn = {
                imports = [
                  ./machines/zjn-mac/home.nix
                  ./desktop/darwin-home.nix
                  ./common/home-darwin.nix
                  ./code
                  ./work/home.nix
                  doom-emacs.hmModule
                ];
              };
            })
        ];
        specialArgs = inputs;
      };
    };
}

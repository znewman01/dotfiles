{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/master";

    flake-utils.url = "github:numtide/flake-utils";

    home-manager.url = "github:nix-community/home-manager/master";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    impermanence.url = "github:nix-community/impermanence";

    emacs-overlay = { url = "github:nix-community/emacs-overlay"; };
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

  outputs = inputs@{ nixpkgs, darwin, flake-utils, home-manager, impermanence
    , doom-emacs, emacs-overlay, ... }:
    let
      systemOutputs = flake-utils.lib.eachDefaultSystem (system:
        let pkgs = nixpkgs.legacyPackages.${system};
        in rec {
          devShells.default = pkgs.mkShell {
            buildInputs = with pkgs; [ nixfmt git-crypt terraform ];
          };
          packages = import ./packages { inherit (pkgs) callPackage; };
          legacyPackages = import nixpkgs {
            inherit system;
            config.allowUnfree = true;
            overlays = [ (final: prev: packages) emacs-overlay.overlay ];
          };
        });
    in systemOutputs // rec {
      modules = import ./modules;
      nixosConfigurations = {
        zjn-work = let flakeModules = modules;
        in nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          pkgs = systemOutputs.legacyPackages."x86_64-linux";
          modules = [
            home-manager.nixosModule
            impermanence.nixosModule
            flakeModules.kolide
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
          ./desktop
          ./desktop/chat
          ({ ... }: {
            home-manager.users.zjn = {
              imports = [
                ./machines/zjn-mac/home.nix
                ./desktop/home.nix
                ./desktop/chat/home.nix
                ./common/home.nix
                modules.code
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

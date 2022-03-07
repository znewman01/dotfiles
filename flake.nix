{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/release-21.11";
    home-manager.url = "github:nix-community/home-manager/release-21.11";
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
  outputs = inputs@{ nixpkgs, home-manager, impermanence, doom-emacs, darwin, ... }: {
    colmena = {
      meta.nixpkgs = import nixpkgs { system = "x86_64-linux"; };
      defaults = { ... }: {
        imports = [ home-manager.nixosModule impermanence.nixosModule ];
      };
    } // import ./machines { inherit inputs; };
    
    darwinConfigurations."zjn-mac" = darwin.lib.darwinSystem {
      system = "aarch64-darwin";
      modules = [ ./machines/zjn-mac ];
    };

  };
}

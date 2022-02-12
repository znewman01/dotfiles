{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/release-21.11";
    home-manager.url = "github:nix-community/home-manager/release-21.11";
    impermanence.url = "github:nix-community/impermanence";
  };
  outputs = { nixpkgs, home-manager, impermanence, ... }: {
    colmena = {
      meta.nixpkgs = import nixpkgs { system = "x86_64-linux"; };
      defaults = { ... }: {
        imports = [ home-manager.nixosModule impermanence.nixosModule ];
      };
    } // import ./machines { };
  };
}

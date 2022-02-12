{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/release-21.11";
    home-manager.url = "github:nix-community/home-manager/release-21.11";
  };
  outputs = { nixpkgs, home-manager, ... }: {
    colmena = {
      meta.nixpkgs = import nixpkgs { system = "x86_64-linux"; };
      defaults = { ... }: { imports = [ (import "${home-manager}/nixos") ]; };
    } // import ./machines { inherit home-manager; };
  };
}

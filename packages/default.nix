{ callPackage }:

{
  chainctl = callPackage /chainctl.nix { };
  osquery = callPackage /osquery.nix { };
  kolide = callPackage /kolide { };
}

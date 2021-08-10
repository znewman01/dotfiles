{ config, ... }:

{
  code = (import ./shells/code.nix) { config = config; };
}

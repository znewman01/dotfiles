{ pkgs, lib, ... }:

{
  launchd.user.agents.syncthing = lib.optionalAttrs pkgs.stdenv.isDarwin {
    command = "syncthing serve --no-browser --no-restart";
    path = [ pkgs.syncthing ];
    serviceConfig.KeepAlive = true;
  };
}

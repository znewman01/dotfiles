{ config, pkgs, ... }:

{
  environment.systemPackages = [ pkgs.syncthing ];
  launchd.user.agents.syncthing = {
    command = "syncthing serve --no-browser --no-restart";
    path = [ pkgs.syncthing ];
    serviceConfig = {
      KeepAlive = true;
    };
  };
}

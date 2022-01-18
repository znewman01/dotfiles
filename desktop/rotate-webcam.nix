{ config, lib, pkgs, ... }:

{
   boot.extraModulePackages = [
       config.boot.kernelPackages.v4l2loopback.out
   ];
   boot.kernelModules = [ "v4l2loopback" ];
   boot.extraModprobeConfig = ''
     options v4l2loopback exclusive_caps=1
   '';

   systemd.services.rotate-webcam = {
     enable = true;
     path = [ pkgs.nix pkgs.ffmpeg ];
     script = ''
       ffmpeg -f v4l2 -i /dev/video1 -vf "transpose=3,format=yuv420p" -f v4l2 /dev/video0
     '';
     wantedBy = [ "multi-user.target" ];
   };
}

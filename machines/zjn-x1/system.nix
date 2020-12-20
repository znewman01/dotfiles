{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    sof-firmware
  ];
  hardware.pulseaudio.extraConfig = ''
      load-module module-alsa-sink device=;w:0,0 channels=4
      load-module module-alsa-source device=hw:0,6 channels=4
  '';
}

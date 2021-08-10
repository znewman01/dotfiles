# Do not modify this file!  It was generated by ‘nixos-generate-config’
# and may be overwritten by future invocations.  Please make changes
# to /etc/nixos/configuration.nix instead.
{ config, lib, pkgs, modulesPath, ... }:

{
  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

  networking.hostId = "2f4cf0b0";

  boot.initrd.availableKernelModules =
    [ "xhci_pci" "ahci" "nvme" "usbhid" "usb_storage" "sd_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-amd" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" = {
    device = "tank/local/root";
    fsType = "zfs";
  };

  fileSystems."/nix" = {
    device = "tank/local/nix";
    fsType = "zfs";
  };

  fileSystems."/persist" = {
    device = "tank/safe/persist";
    fsType = "zfs";
    neededForBoot = true;
  };

  fileSystems."/cache" = {
    device = "tank/local/cache";
    fsType = "zfs";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/0737-2C96";
    fsType = "vfat";
  };

  swapDevices = [ ];
}

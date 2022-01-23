{ config, pkgs, modulesPath, lib, ... }: {
  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

  boot.initrd.availableKernelModules =
    [ "xhci_pci" "ahci" "usbhid" "usb_storage" "sd_mod" "nvme" ];

  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ ];
  boot.extraModulePackages = [ ];

  networking.hostId = "babecafe";

  fileSystems."/" = {
    device = "tank/local/root";
    fsType = "zfs";
  };

  fileSystems."/nix" = {
    device = "tank/local/nix";
    fsType = "zfs";
  };

  fileSystems."/cache" = {
    device = "tank/local/cache";
    fsType = "zfs";
  };

  fileSystems."/persist" = {
    device = "tank/safe/persist";
    fsType = "zfs";
  };

  fileSystems."/boot" = {
    device = "/dev/nvme0n1p2";
    fsType = "vfat";
  };

  swapDevices = [ ];
}

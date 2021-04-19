#!/bin/sh
set -x
set -e

if [ ! "$USER" = "root" ]; then
  echo "Must be run as root!"
  exit 1
fi

# Need internet BEFORE this script. but if we downloaded it...
#   $ su
#   # wpa_supplicant -B -i <INTERFACE> -c <(wpa_passphrase 'SSID' 'key')

# Manual
# 1. partition (can script parts maybe?)
DISK=/dev/nvme0n1  # TODO: should prompt
parted $DISK -- mklabel GPT
parted $DISK -- mkpart primary 512MiB 100%
parted $DISK -- mkpart ESP fat32 1MiB 512MiB
parted $DISK -- set 2 esp on
PART_PREFIX=/dev/nvme0n1p  # TODO: prompt (maybe ls $DISK/*)
# 2. format
mkfs.fat -F 32 -n boot ${PART_PREFIX}2
zpool create tank ${PART_PREFIX}1
zfs create -p -o mountpoint=legacy tank/safe/persist
zfs create -p -o mountpoint=legacy tank/local/root
zfs create -p -o mountpoint=legacy tank/local/cache
zfs create -p -o mountpoint=legacy tank/local/nix
# 3. mount
mount -t zfs tank/local/root /mnt
mkdir /mnt/{boot,cache,persist,nix}
mount -t zfs tank/local/nix /mnt/nix
mount -t zfs tank/local/cache /mnt/cache
mount -t zfs tank/safe/persist /mnt/persist
mount ${PART_PREFIX}2 /mnt/boot
# 4. configure locally
REPO=/mnt/persist/zjn/git/dotfiles
mkdir -p $(dirname $REPO)
nix-env -iA nixos.git
git clone https://github.com/znewman01/dotfiles $REPO
HOSTNAME=zjn-x1prime  # TODO: prompt
cat $REPO/configuration.nix.template \
  | sed -e "s'{{HOSTNAME}}'${HOSTNAME}'g" \
  > $REPO/configuration.nix
cat $REPO/home.nix.template \
  | sed -e "s'{{HOSTNAME}}'${HOSTNAME}'g" \
  > $REPO/home.nix
# comment out code line in home-common.nix!
sed -i 's/\(code = (import\)/# \1/' $REPO/home-common.nix
# 5. Setup install files
mkdir -p /mnt/etc/nixos
ln -s $REPO/configuration.nix /mnt/etc/nixos/
nixos-generate-config --root /mnt --show-hardware-config \
  > /mnt/persist/hardware-configuration.nix
ln -s /mnt/persist/hardware-configuration.nix /etc/nixos/
# 6. do the thing
nixos-install

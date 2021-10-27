#!/bin/sh
set -x
set -e

if [ ! "$USER" = "root" ]; then
  echo "Must be run as root!"
  exit 1
fi


read -p "What disk to format (e.g. /dev/sda, /dev/nvme0n1)? " DISK
ls "$DISK*"
read -p "What prefix should we use for partitions of $DISK? " PART_PREFIX
read -p "What hostname (usually `zjn-...`)? " HOSTNAME

# Need internet BEFORE this script. but if we downloaded it...
#   $ su
#   # wpa_supplicant -B -i <INTERFACE> -c <(wpa_passphrase 'SSID' 'key')

# Manual
# 1. partition (can script parts maybe?)
parted $DISK -- mklabel GPT
parted $DISK -- mkpart primary 512MiB 100%
parted $DISK -- mkpart ESP fat32 1MiB 512MiB
parted $DISK -- set 2 esp on
# 2. format
mkfs.fat -F 32 -n boot ${PART_PREFIX}2
zpool create -f tank ${PART_PREFIX}1
zfs set com.sun:auto-snapshot=true tank
zfs set compression=lz4 tank
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
rm -f /etc/nixos/configuration.nix
ln -s $REPO/configuration.nix /etc/nixos/
nixos-generate-config --root /mnt --show-hardware-config \
  > /mnt/persist/hardware-configuration.nix
ln -s /mnt/persist/hardware-configuration.nix /etc/nixos/
ln -s /mnt/persist/hardware-configuration.nix /mnt/etc/nixos/
# 6. do the thing
nixos-rebuild build
nixos-install

# TODO:
# - permissions on /persist/zjn, /cache/zjn
mkdir -p /cache/zjn
mkdir -p /persist/ssh
mkdir -p /persist/nm-system-connections
mkdir -p /persist/bors
mkdir -p /persist/wireguard
chown -R zjn:users /cache/zjn /persist/zjn

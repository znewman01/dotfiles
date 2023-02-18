#!/usr/bin/env bash
set -x
set -e

if [ ! "$USER" = "root" ]; then
  echo "Must be run as root!"
  exit 1
fi


echo "What disk to format?"
IFS= mapfile -t all_disks < <(find /dev/disk/by-id -type l)
select opt in "${all_disks[@]}"; do
  DISK="$opt"
  break
done

# 1. partition (can script parts maybe?)
parted $DISK -- mklabel GPT
parted $DISK -- mkpart primary 512MiB 100%
parted $DISK -- mkpart ESP fat32 1MiB 512MiB
parted $DISK -- set 2 esp on
# 2. format
mkfs.fat -F 32 -n boot ${DISK}-part2
zpool create -f \
  -O acltype=posixacl \
  -O xattr=sa \
  -O compression=lz4 \
  -O com.sun:auto-snapshot=true \
  -R /mnt \
  tank \
  ${DISK}-part1
zfs create -p -o canmount=off -o mountpoint=none -o encryption=on -o keylocation=prompt -o keyformat=passphrase tank/encrypt
zfs create -p -o mountpoint=legacy tank/encrypt/safe/persist
zfs create -p -o mountpoint=legacy tank/encrypt/local/root
zfs create -p -o mountpoint=legacy tank/encrypt/local/cache
zfs create -p -o mountpoint=legacy tank/encrypt/local/nix
# 3. mount
mount -t zfs tank/encrypt/local/root /mnt
mkdir /mnt/{boot,cache,persist,nix}
mount -t zfs tank/encrypt/local/nix /mnt/nix
mount -t zfs tank/encrypt/local/cache /mnt/cache
mount -t zfs tank/encrypt/safe/persist /mnt/persist
mount ${DISK}-part2 /mnt/boot
# 4. configure locally
REPO=/mnt/persist/zjn/git/dotfiles
mkdir -p $(dirname $REPO)
nix-env -iA nixos.git
git clone https://github.com/znewman01/dotfiles $REPO

echo "Go to $REPO!"

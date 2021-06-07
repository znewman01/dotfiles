# dotfiles

## Manual steps

- Preconfigure: copy an existing setup to `machines/$HOSTNAME`
- Install NixOS: https://nixos.org/nixos/manual/index.html
  - I recommend setting up [erase-on-boot](https://grahamc.com/blog/erase-your-darlings).
  - {root,nix,cache,persist} datasets seems to make sense
  - If using ZFS, set autosnapshot where applicable.
  - You may need to configure WiFi:

    ```sh
    ifconfig wlp3s0 down
    ifconfig wlp3s0 up
    iwconfig wlp3s0 essid ...
    sleep 10 && ping 8.8.8.8  # repeat as needed
    ```
  - Clone this repo on the new machine; symlink configuration.nix in and run `nixos-rebuild boot`
  - on reboot:
    - log in as `zjn`
    - deactivate root password (`sudo passwd -l root`). then proceed

0. Clone this repo: `mkdir -p ~/git && git clone https://github.com/znewman01/dotfiles.git /mnt/persist/zjn/git/dotfiles`
   - syncthing: http://localhost:8384 on both machines
1. (Comment out GitHub code.). Run `./scripts/install.sh`.
2. Run `./scripts/post-install.sh`
   - Firefox
   - MIT certs (broken)
   - email
   - GitHub (generate yourself an SSH key, share with GitHub; broken/removed)
   - CSAIL WiFi

You're probably going to have to restart Firefox, Emacs a couple of times each
until things work. Such is life.

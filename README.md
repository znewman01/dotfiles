# dotfiles

## TODO

- [ ] TRIM on your SSD
- [ ] xmobar dropbox script should be scoped to just xmobar world
- [ ] work through nix-pills
- [ ] different xmobar configs

## Manual steps

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
  - make sure to add `zjn` user, hostname, git/vim/wget in configuration.nix
  - on reboot:
    - log in as root,
    - set `passwd zjn`
    - log in as `zjn`
    - deactivate root password (`passwd -l root`). then proceed

0. Clone this repo: `mkdir -p ~/git && git clone https://github.com/znewman01/dotfiles.git ~/git/dotfiles`
1. Run `cd ~/git/dotfiles && sudo ./scripts/install_root.sh && sudo reboot`
2. (Comment out GitHub code.). Run `./scripts/install.sh`.
3. Run `./scripts/post-install.sh`
   - Firefox
   - MIT certs (broken)
   - email
   - GitHub (generate yourself an SSH key, share with GitHub; broken/removed)
   - CSAIL WiFi

You're probably going to have to restart Firefox, Emacs a couple of times each
until things work. Such is life.

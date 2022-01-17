# dotfiles

## Manual steps

- Preconfigure: copy an existing setup to `machines/$HOSTNAME`
- Install NixOS: https://nixos.org/nixos/manual/index.html
  - format flash drive: `dd if=$PATH_TO_ISO.iso of=/dev/sda`
  - boot with that flash drive
  - You may need to configure WiFi:

    ```sh
    ifconfig wlp3s0 down
    ifconfig wlp3s0 up
    iwconfig wlp3s0 essid ...
    sleep 10 && ping 8.8.8.8  # repeat as needed
    ```
  - run `scripts/actual_install.sh` from this repo (no dependencies)
  - on reboot:
    - log in as `zjn`
    - deactivate root password (`sudo passwd -l root`). then proceed

- Set stuff up
  - Syncthing: http://localhost:8384 on both machines
  - Pass (might need to run repeatedly)
  - Firefox
  - [MIT certs](https://ist.mit.edu/certificates)
  - email (`mkdir ~/Maildir/{gmail,csail,mit,fastmail,chainguard} && mbsync -a` then `mu init --my-address=... --my-address=...`)
  - GitHub (MIT and public) SSH keys
  - CSAIL WiFi

You're probably going to have to restart Firefox, Emacs a couple of times each
until things work. Such is life.

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
  - email (`mkdir ~/Maildir/{gmail,csail,mit,fastmail,chainguard} && mbsync -a` then `mu init --my-address=... --my-address=...`)
  - GitHub SSH keys, reenable `code.nix`
  - Tailscale

You're probably going to have to restart Firefox, Emacs a couple of times each
until things work. Such is life.

## Alternate: auto steps

- Preconfigure
  - copy an existing setup to `machines/$HOSTNAME` (but comment out import of `code.nix`!)
  - copy a block in `ops/network.nix` for the new machine
- Install 
  - `cd autoinstall && ln -s ../../machines/$HOSTNAME/system.nix configuration.nix && ./build`
  - `dd if=autoinstall.iso of=/dev/sda`
  - boot with that flash drive, watch it install itself
- Post-install
  1. tailscale (which involves logging into google)
  2. syncthing
     - <https://localhost:8384>
     - add introducer device, accept on it
  3. keys (from a working device)
     - create and fetch ssh-keys
       ```bash
       ssh $NEWDEVICE -- 'ssh-keygen -t ed25519 -f "$HOME/.ssh/id_ed25519" -N ""'
       ssh $NEWDEVICE -- cp ~/.ssh/id_ed25519.pub ~/Sync/keys/${NEWDEVICE}.pub
       cp ~/Sync/keys/${NEWDEVICE}.pub net/${NEWDEVICE}.pub
       sed -i "/\]/i\  ./${NEWDEVICE}.pub" net/keys.nix
         ```
     - gpg keys
       ```bash
       ssh $NEWDEVICE -- 'find "$HOME/.gnupg/" -type f -exec chmod 600 {} \;'
       ssh $NEWDEVICE -- 'find "$HOME/.gnupg/" -type d -exec chmod 700 {} \;'
       ssh $NEWDEVICE  # do the GPG stuff in `install.sh`
       FINGERPRINT=$(ssh $NEWDEVICE -- gpg --list-keys | grep "Key fingerprint" | head -n 1 | cut -d= -f2 | sed 's/ //g')
       ssh $NEWDEVICE -- gpg --edit-key $FINGERPRINT  # type in 'passwd<RET>' and then enter password
       ssh $NEWDEVICE "gpg --output $HOME/Sync/keys/${NEWDEVICE}.gpg --export z@znewman.net"
       gpg --import $HOME/Sync/keys/${NEWDEVICE}.gpg
       echo "${FINGERPRINT}:6" | gpg --import-ownertrust
       echo $FINGERPRINT >> $HOME/.password-store/.gpg-id
       ```
  4. pass (on a working device)
     ```bash
     # this will take a while; in the meantime import owner trust on other devices including $NEWDEVICE
     while true; do xargs pass init < $HOME/.password-store/.gpg-id; echo trying again; sleep 1; done
     ```
  5. once `pass show firefox` works log in to Firefox
  6. email (if applicable)
     - `mkdir ~/Maildir/{gmail,csail,mit,fastmail,chainguard} && mbsync -a`
     - `mu init --my-address=... --my-address=...`
  7. GitHub SSH keys, pull this repo
  8. update yourself
     - reenable `code.nix`
     - make sure you have a `hardware-configuration.nix`
       - `nixos-generate-config --show-hardware-config`
       - compare with existing



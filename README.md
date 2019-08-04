# dotfiles

## TODO

- [x] Firefox configuration (plugins, sync)
  - [x] Install Tridactyl beta automatically
  - [x] & Tridactyl config:
          :bind J tabnext
          :bind K tabprev
  - [x] Set default search engine: DDG
  - [x] Set home page
  - [x] get search configured correctly
- [x] Passwords, SSH keys (pass?)
  - [x] use pass more: xmonad-pass
- [x] Better terminal
- [x] Email
- [x] Git config
- [x] My git projects
- [x] Sound
- [x] Emacs + Spacemacs
  - [x] services.emacs.enable
- [ ] TRIM on your SSD
- [x] CUPS
- [x] Separate install.sh into root,non-root parts
- [x] WiFi
- [x] $EDITOR
- [x] Need to fix fonts
- [x] Xmonad
  - [x] less ugly border
  - [x] dmenu for running programs
  - [x] xmobar
  - [ ] xmobar dropbox script should be scoped to just xmobar world
  - [x] scratchpad
  - [ ] dunst
- [x] batteries
- [x] get copy/paste working
- [x] better factoring for shared variables (and reorganize generally)
- [x] brightness
  - [x] services.redshift
- [x] screen saver/locker
- [ ] rofi calculator (requires writing (probably simple) derivation)
- [ ] emacs configs into git
- [-] org files into git? NO
- [ ] disk encryption
- [-] get everything working on mac. NO
- [ ] look into fixing home-manager mbsync experience
  - [ ] channels
  - [ ] add test for mbsyncrc extra config at top
- [x] anki
- [ ] work through nix-pills
- [x] direnv
- [x] webcam
- [ ] better configuration for multiple machines
  - [ ] hostnames
  - [x] e.g. autorandr
  - [x] github code cloning
- [ ] backups
- [ ] org from source

## Manual steps

0. Clone this repo: `git clone https://github.com/znewman01/dotfiles.git`
1. Run `cd dotfiles`. Edit networking.hostName. `./install_root.sh && reboot`
2. (Comment out GitHub code.). Run `./install.sh` (in X!). Wait a while. Reboot.
3. Run `/.post-install.sh`

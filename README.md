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
- [x] Better terminal
- [x] Email
- [x] Git config
- [x] My git projects
- [x] Sound
- [x] Emacs + Spacemacs
  - [x] services.emacs.enable
- [ ] TRIM on your SSD
- [ ] CUPS
- [ ] Separate install.sh into root,non-root parts
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
- [ ] better factoring for shared variables (and reorganize generally)
- [x] brightness
  - [x] services.redshift
- [x] screen saver/locker
- [ ] rofi calculator
- [ ] emacs configs into git
- [ ] org files into git?
- [ ] disk encryption
- [ ] get everything working on mac
- [ ] look into fixing home-manager mbsync experience
- [x] anki
- [ ] work through nix-pills
- [x] direnv
- [ ] webcam
- [ ] (workstation) screen: `xrandr --output HDMI-5 --left-of DP-3 --rotate right`

## Manual steps

0. Clone this repo: `git clone https://git.github.com/znewman01/dotfiles.git`
1. Run `cd dotfiles`. Edit networking.hostName. `./install_root.sh && reboot`
2. (Comment out GitHub code. TODO: make this not a thing). Run `./install.sh` (in X!). Wait a while. Reboot.
3. Run `/.post-install.sh`

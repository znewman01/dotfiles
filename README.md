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

## Manual steps

1. Run `./install.sh`
2. Firefox:
   - Log in to Firefox sync (need email nearby)
   - Log in to LastPass
   - Tridactyl: ":installnative", ":restart"
   - firefox "about:preferences#search"
   - firefox https://ca.mit.edu/ca/
3. Dropbox
   - this is weird, you may need to wait a while for it to update itself
   - `dropbox status` will give you a URL

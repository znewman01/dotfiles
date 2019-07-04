# dotfiles

## TODO

- [-] Firefox configuration (plugins, sync)
  - [ ] Install Tridactyl beta automatically
  - [ ] & Tridactyl config:
          :bind J tabnext
          :bind K tabprev
  - [ ] Set default search engine: DDG
  - [ ] Set home page
  - [ ] get search configured correctly
- [ ] Passwords, SSH keys (pass?)
- [x] Better terminal
- [ ] Email
- [x] Git config
- [ ] My git projects
- [x] Sound
- [ ] Emacs + Spacemacs
  - [ ] services.emacs.enable
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
- [x] batteries
- [ ] get copy/paste working
- [ ] better factoring for shared variables
- [ ] brightness
  - [ ] services.redshift
- [ ] screen saver/locker
- [ ] rofi calculator

## Manual steps

1. Run `./install.sh`
2. Firefox:
   - Log in to Firefox sync (need email nearby)
   - Log in to LastPass
   - Tridactyl: ":installnative", ":restart"
   - firefox "about:preferences#search"
3. Dropbox
   - this is weird, you may need to wait a while for it to update itself
   - `dropbox status` will give you a URL

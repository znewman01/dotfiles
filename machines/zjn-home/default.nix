{ config, pkgs, ... }:

{
  imports = [ ];

  systemd.user.tmpfiles.rules = [
    "L %h/git      - - - - /persist/zjn/git"
    "L %h/.gnupg   - - - - /persist/zjn/gnupg"
    "L %h/.ssh     - - - - /persist/zjn/ssh"
    "L %h/Maildir  - - - - /persist/zjn/Maildir"
    "L %h/.dropbox-hm - - - - /persist/zjn/dropbox-hm"
    "L %h/.gist    - - - - /persist/zjn/gist"
  ];
}

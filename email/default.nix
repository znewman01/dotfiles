{ config, lib, pkgs, ... }:

with lib;

let
  passCmd = entry: "${pkgs.pass}/bin/pass ${entry} 2> /dev/null";
  unstable = import (builtins.fetchGit {
    # Descriptive name to make the store path easier to identify
    name = "nixos-unstable-2021-09-30";
    url = "https://github.com/nixos/nixpkgs/";
    # git ls-remote https://github.com/nixos/nixpkgs nixos-unstable
    ref = "refs/heads/nixos-unstable";
    rev = "c21ba4f7bb4a3d621eb1d187e6b5e816bb85380c";
  }) { };
in {

  home.packages = [ unstable.mu ];

  home.file."bin/mbsync" = {
    executable = true;
    text = ''
      #!/usr/bin/env nix-shell
      #!nix-shell -i bash -p isync
      set -euo pipefail
      GOAL=earlyemail
      BEE="https://www.beeminder.com/api/v1"
      BEE_AUTH="auth_token=$(pass show beeminder-auth-token)"
      if [ $(date '+%H%M') -lt 1000 ]; then
        echo "uh oh! too early in the day :'("
        curl -s -X POST \
          --data "''${BEE_AUTH}&value=''${LI_GAMES}" \
          "''${BEE}/users/znewman01/goals/''${GOAL}/datapoints.json"
      fi
      mbsync $@
    '';
  };

  # concatStringsSep trick is a half-hearted attempt to prevent email harvesting.
  # remember to mu init --my-address=zjn@chainguard.dev --my-address=z@znewman.net --my-address=zjn@csail.mit.edu --my-address=znewman01@gmail.com --my-address=zjn@mit.edu --my-address='/.*@z.znewman.net/'
  accounts.email = {
    maildirBasePath = "${config.home.homeDirectory}/Maildir";

    accounts = {

      mit = {
        address = concatStringsSep "@" [ "zjn" "mit.edu" ];
        userName = concatStringsSep "@" [ "zjn" "mit.edu" ];
        passwordCommand = passCmd "mit";

        folders = {
          inbox = "Inbox";
          drafts = "Drafts";
          sent = "Sent";
          trash = "Deleted";
        };

        imap = {
          host = "imap.exchange.mit.edu";
          port = 993;
          tls.enable = true;
        };

        # mbsync = {
        #   enable = true;
        #   remove = "maildir";
        #   create = "both";
        #   expunge = "both";
        #   flatten = "..";
        #   patterns = [ "Archive" "Drafts" "INBOX" ];
        #   extraConfig.account = {
        #     AuthMechs = "PLAIN";
        #   };
        #   extraConfig.channel = {
        #     "#" = ''
        #       # TODO: move these after https://github.com/rycee/home-manager/issues/747
        #       Master :mit-remote:
        #       Slave :mit-local:

        #       Channel mit-sent
        #       Master ":mit-remote:Sent Items"
        #       Slave ":mit-local:Sent"

        #       Channel mit-junk
        #       Master ":mit-remote:Junk E-Mail"
        #       Slave ":mit-local:Junk"

        #       Channel mit-trash
        #       Master ":mit-remote:Deleted Items"
        #       Slave ":mit-local:Deleted"

        #       Channel mit-rest
        #       #'';
        #   };

        # };
      };

      fastmail = {
        realName = "Zachary Newman";
        userName = concatStringsSep "@" [ "z" "znewman.net" ];
        address = concatStringsSep "@" [ "z" "znewman.net" ];
        maildir.path = "fastmail";
        passwordCommand = passCmd "fastmail-app";
        primary = true;

        folders = {
          inbox = "INBOX";
          drafts = "Drafts";
          sent = "Sent";
          trash = "Trash";
        };

        imap = {
          host = "imap.fastmail.com";
          port = 993;
          tls.enable = true;
        };

        # mbsync = {
        #   enable = true;
        #   create = "both";
        #   expunge = "both";
        #   remove = "maildir";
        #   flatten = "..";
        #   patterns = [ "Archive" "Drafts" "INBOX" "Sent" "Spam" "Trash" ];
        # };
      };

      csail = {
        realName = "Zachary Newman";
        userName = concatStringsSep "@" [ "zjn" "csail.mit.edu" ];
        address = concatStringsSep "@" [ "zjn" "csail.mit.edu" ];
        maildir.path = "fastmail";
        passwordCommand = passCmd "csail-imap";

        folders = {
          inbox = "INBOX";
          drafts = "Drafts";
          sent = "Sent";
          trash = "Trash";
        };

        imap = {
          host = "imap.csail.mit.edu";
          port = "143";
          tls.enable = true;
        };
      };

      gmail = {
        realName = "Zachary Newman";
        address = concatStringsSep "@" [ "znewman01" "gmail.com" ];
        userName = concatStringsSep "@" [ "znewman01" "gmail.com" ];
        maildir.path = "gmail";
        flavor = "gmail.com";
        passwordCommand = passCmd "gmail-imap";

        folders = {
          inbox = "Inbox";
          drafts = "[Gmail]/Drafts";
          sent = "[Gmail]/SentMail";
          trash = "[Gmail]/Trash";
        };

        imap.port = 993;

        # mbsync = {
        #   enable = true;
        #   remove = "maildir";
        #   create = "both";
        #   expunge = "both";
        #   patterns = [ "Inbox" "\"[Gmail]/Drafts\"" "\"[Gmail]/Spam\"" "\"[Gmail]/Trash\"" ];
        #   extraConfig.account = {
        #     AuthMechs = "LOGIN";
        #   };
        #   extraConfig.channel = {
        #     "#" = ''
        #       Giant hack ahead!
        #       # TODO: move these after https://github.com/rycee/home-manager/issues/747
        #       Master :gmail-remote:
        #       Slave :gmail-local:

        #       Channel gmail-inbox
        #       Master :gmail-remote:Inbox
        #       Slave :gmail-local:Inbox

        #       Channel gmail-sent
        #       Master ":gmail-remote:[Gmail]/Sent Mail"
        #       Slave ":gmail-local:[Gmail]/SentMail"

        #       Channel gmail-all
        #       Master ":gmail-remote:[Gmail]/All Mail"
        #       Slave ":gmail-local:[Gmail]/AllMail"

        #       Channel gmail-rest
        #       # <- needed to escape quoting'';
        #   };
        # };
      };

      chainguard = {
        realName = "Zachary Newman";
        address = concatStringsSep "@" [ "zjn" "chainguard.dev" ];
        userName = concatStringsSep "@" [ "zjn" "chainguard.dev" ];
        maildir.path = "chainguard";
        flavor = "gmail.com";
        passwordCommand = passCmd "chainguard-imap";

        folders = {
          inbox = "Inbox";
          drafts = "[Gmail]/Drafts";
          sent = "[Gmail]/SentMail";
          trash = "[Gmail]/Trash";
        };

        imap.port = 993;
      };
    };

  };
  # programs.mbsync = {
  # enable = true;
  # extraConfig = ''
  #   CopyArrivalDate yes
  #   Create Both
  #   Expunge Both

  # '';
  # TODO: this is all borked. Groups just need channel names, and don't take
  # comma-separated.
  #
  # groups = {
  #   mit = {
  #     mit = [ "mit-rest" "mit-sent" "mit-junk" "mit-trash" ];
  #   };
  #   gmail = {
  #     gmail = [ "gmail-rest" "gmail-inbox" "gmail-sent" "gmail-all" ];
  #   };
  # };
  # };

  home.file.".mbsyncrc".text = import ./mbsyncrc.nix { pkgs = pkgs; };

}

{ config, lib, pkgs, ... }:

with lib;

let
  passBin = "${pkgs.pass}/bin/pass";
in
{

  home.packages = with pkgs; [
    mu
    isync
  ];

  # concatStringsSep trick is a half-hearted attempt to prevent email harvesting.
  accounts.email = {
    maildirBasePath = "${config.home.homeDirectory}/Maildir";

    accounts = {

      mit = {
        address = concatStringsSep "@" [ "zjn" "mit.edu" ];
        userName = concatStringsSep "@" [ "zjn" "mit.edu" ];
        passwordCommand = "${passBin} show mit";

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
        passwordCommand = "${passBin} show fastmail";
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

      gmail = {
        realName = "Zachary Newman";
        address = concatStringsSep "@" [ "znewman01" "gmail.com" ];
        userName = concatStringsSep "@" [ "znewman01" "gmail.com" ];
        maildir.path = "gmail";
        flavor = "gmail.com";
        passwordCommand = "${passBin} show gmail-imap";

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
  home.file.".mbsyncrc".source = ./mbsyncrc;
  services.mbsync = {
    enable = true;
    postExec = ("${pkgs.emacs}/bin/emacsclient -e" +
                " \"(progn (require 'mu4e) (mu4e-update-index))\"");
  };

}

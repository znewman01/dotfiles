{ pkgs, ... }:

let passBin = "${pkgs.pass}/bin/pass";
in ''
  CopyArrivalDate yes
  Create Both
  Expunge Both
  Remove Near
  SyncState *


  IMAPAccount fastmail
  CertificateFile /etc/ssl/certs/ca-certificates.crt
  Host imap.fastmail.com
  PassCmd "${passBin} show fastmail-app 2> /dev/null"
  Port 993
  SSLType IMAPS
  User z@znewman.net

  IMAPStore fastmail-remote
  Account fastmail

  MaildirStore fastmail-local
  Flatten ..
  Inbox /home/zjn/Maildir/fastmail/INBOX
  Path /home/zjn/Maildir/fastmail/
  SubFolders Verbatim

  Channel fastmail
  Create Both
  Expunge Both
  Far :fastmail-remote:
  Patterns *
  Remove Near
  Near :fastmail-local:


  IMAPAccount csail
  AuthMechs CRAM-MD5 DIGEST-MD5
  CertificateFile /etc/ssl/certs/ca-certificates.crt
  Host imap.csail.mit.edu
  PassCmd "${passBin} show csail-imap 2> /dev/null"
  Port 143
  SSLType STARTTLS
  # Port 993
  # SSLType IMAPS
  User zjn

  IMAPStore csail-remote
  Account csail

  MaildirStore csail-local
  Flatten ..
  Inbox /home/zjn/Maildir/csail/INBOX
  Path /home/zjn/Maildir/csail/
  SubFolders Verbatim

  Channel csail
  Create Both
  Expunge Near
  Far :csail-remote:
  Patterns *
  Remove Near
  Near :csail-local:


  IMAPAccount gmail
  AuthMechs LOGIN
  CertificateFile /etc/ssl/certs/ca-certificates.crt
  Host imap.gmail.com
  PassCmd "${passBin} show gmail-imap 2> /dev/null"
  Port 993
  SSLType IMAPS
  User znewman01@gmail.com

  IMAPStore gmail-remote
  Account gmail

  MaildirStore gmail-local
  Inbox /home/zjn/Maildir/gmail/Inbox
  Path /home/zjn/Maildir/gmail/
  SubFolders Verbatim

  Channel gmail-inbox
  Far ":gmail-remote:INBOX"
  Near ":gmail-local:Inbox"

  Channel gmail-sent
  Far ":gmail-remote:[Gmail]/Sent Mail"
  Near ":gmail-local:[Gmail]/SentMail"

  Channel gmail-all
  Far ":gmail-remote:[Gmail]/All Mail"
  Near ":gmail-local:[Gmail]/AllMail"

  Channel gmail-rest
  Far :gmail-remote:
  Near :gmail-local:
  Patterns "[Gmail]/Drafts" "[Gmail]/Spam" "[Gmail]/Trash"


  IMAPAccount mit
  AuthMechs PLAIN
  CertificateFile /etc/ssl/certs/ca-certificates.crt
  Host imap.exchange.mit.edu
  PassCmd "${passBin} show mit 2> /dev/null"
  Port 993
  SSLType IMAPS
  User zjn@mit.edu

  IMAPStore mit-remote
  Account mit

  MaildirStore mit-local
  Flatten ..
  Inbox /home/zjn/Maildir/mit/Inbox
  Path /home/zjn/Maildir/mit/
  SubFolders Verbatim

  Channel mit-sent
  Far ":mit-remote:Sent Items"
  Near ":mit-local:Sent"

  Channel mit-junk
  Far ":mit-remote:Junk E-Mail"
  Near ":mit-local:Junk"

  Channel mit-trash
  Far ":mit-remote:Deleted Items"
  Near ":mit-local:Deleted"

  Channel mit-inbox
  Far ":mit-remote:INBOX"
  Near ":mit-local:Inbox"

  Channel mit-rest
  Far :mit-remote:
  Near :mit-local:
  Patterns "Archive" "Drafts"


  Group gmail
  Channel gmail-inbox
  Channel gmail-rest
  Channel gmail-sent
  Channel gmail-all

  Group mit
  Channel mit-inbox
  Channel mit-rest
  Channel mit-sent
  Channel mit-junk
  Channel mit-trash
''

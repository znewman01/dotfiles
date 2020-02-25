{ pkgs, ... }:

let
  passBin = "${pkgs.pass}/bin/pass";
in
''
CopyArrivalDate yes
Create Both
Expunge Both
Remove Slave
SyncState *


IMAPAccount fastmail
CertificateFile /etc/ssl/certs/ca-certificates.crt
Host imap.fastmail.com
PassCmd "${passBin} show fastmail-app"
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
Master :fastmail-remote:
Patterns Archive Drafts INBOX Sent Spam Trash
Remove Slave
Slave :fastmail-local:


IMAPAccount csail
AuthMechs CRAM-MD5 DIGEST-MD5
CertificateFile /etc/ssl/certs/ca-certificates.crt
Host imap.csail.mit.edu
PassCmd "${passBin} show csail-imap"
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
Expunge Both
Master :csail-remote:
Patterns Archive Drafts INBOX Sent Spam Trash
Remove Slave
Slave :csail-local:


IMAPAccount gmail
AuthMechs LOGIN
CertificateFile /etc/ssl/certs/ca-certificates.crt
Host imap.gmail.com
PassCmd "${passBin} show gmail-imap"
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
Master ":gmail-remote:Inbox"
Slave ":gmail-local:Inbox"

Channel gmail-sent
Master ":gmail-remote:[Gmail]/Sent Mail"
Slave ":gmail-local:[Gmail]/SentMail"

Channel gmail-all
Master ":gmail-remote:[Gmail]/All Mail"
Slave ":gmail-local:[Gmail]/AllMail"

Channel gmail-rest
Master :gmail-remote:
Slave :gmail-local:
Patterns "[Gmail]/Drafts" "[Gmail]/Spam" "[Gmail]/Trash"


IMAPAccount mit
AuthMechs PLAIN
CertificateFile /etc/ssl/certs/ca-certificates.crt
Host imap.exchange.mit.edu
PassCmd "${passBin} show mit"
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
Master ":mit-remote:Sent Items"
Slave ":mit-local:Sent"

Channel mit-junk
Master ":mit-remote:Junk E-Mail"
Slave ":mit-local:Junk"

Channel mit-trash
Master ":mit-remote:Deleted Items"
Slave ":mit-local:Deleted"

Channel mit-inbox
Master ":mit-remote:Inbox"
Slave ":mit-local:Inbox"

Channel mit-rest
Master :mit-remote:
Slave :mit-local:
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

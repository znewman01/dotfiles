#!/bin/sh
set -e

if [ ! gpg --list-keys | grep "($HOSTNAME)"; then
    tmpdir=$(mktemp -d)
    chmod 0700 $HOME/.gnupg
    cat > $tmpdir/keydetails <<EOF
        %echo Generating a basic OpenPGP key
        Key-Type: RSA
        Key-Length: 3072
        Subkey-Type: RSA
        Subkey-Length: 2048
        Name-Real: Zachary Newman
        Name-Comment: $HOSTNAME
        Name-Email: z@znewman.net
        Expire-Date: 0
        %no-ask-passphrase
        %no-protection
        %pubring $HOME/.gnupg/pubring.kbx
        %secring $HOME/.gnupg/trustdb.gpg
        %commit
        %echo done
    EOF
    gpg2 --verbose --batch --gen-key keydetails
    echo -e "5\ny\n" |  gpg2 --command-fd 0 --expert --edit-key z@znewman.net trust;
    # TODO: gpg --edit-key $KEYID # then type "passwd<RET>"
fi

if [ ! -f "$HOME/.ssh/id_ed25519" ]; then
    ssh-keygen -t ed25519 -f "$HOME/.ssh/id_ed25519" -N ""
fi

find "$HOME/.gnupg/" -type f -exec chmod 600 {} \;
find "$HOME/.gnupg/" -type d -exec chmod 700 {} \;

read -p "Hook up sync thing, wait for it to finish, hit enter."

# TODO: setup/copy ssh pubkey here
gpg --output "$HOME/Sync/keys/${HOSTNAME}.gpg" --export z@znewman.net
cp "$HOME/.ssh/id_ed25519.pub" "$HOME/Sync/keys/${HOSTNAME}.pub"

FINGERPRINT=$(gpg --list-keys | grep "Key fingerprint" | head -n 1 | cut -d= -f2 | sed 's/ //g')

read -p "On a viable machine, run:
    $ gpg --import $HOME/Sync/keys/${HOSTNAME}.gpg
    $ echo '${FINGERPRINT}:6' | gpg --import-ownertrust
    $ echo $FINGERPRINT >> $HOME/.password-store/.gpg-id
    $ xargs pass init < $HOME/.password-store/.gpg-id

Verify that you can now read passwords on this machine. Then hit [enter]. "

# gpg --list-keys | grep -oP "(?<=fingerprint =) ([A-F0-9]{4}\s*)+" | tr -d ' ' | sed 's/\(.*\)/\1:6/g'

#! /usr/bin/env nix-shell
#! nix-shell -i bash -p "python3.withPackages(ps: [ps.selenium])" geckodriver xdotool nssTools
# Needs to be bash for process subtitution
set -e

autorandr -c

FFKEYS="xdotool search --onlyvisible --class Firefox"

read -p "Set up Firefox account?

Requires:
- all Firefox windows closed
- \"$ pass -c firefox\" to work

[y/N]? " FIREFOX


if [ "$FIREFOX" = "y" ]; then
    echo "Setting up Firefox sync."
    python3 browser_auto/firefox_setup.py

    # home-manager handles Firefox profile files so need "-b"
    home-manager switch -b "bak"

    # For Tridactyl
    curl -fsSl https://raw.githubusercontent.com/tridactyl/tridactyl/master/native/install.sh -o /tmp/trinativeinstall.sh && bash /tmp/trinativeinstall.sh master

    # Selenium can't really mess with preferences.
    echo "Setting default search engine to DDG."
    firefox "about:preferences#search" > /dev/null 2>&1 &
    sleep 2
    $FFKEYS \
        windowfocus --sync \
        key --clearmodifiers --delay 200 Tab Tab d ctrl+q
    sleep 1
    pkill firefox

fi

read -p "Set up MIT certificates? [y/N] " MIT

if [ "$MIT" = "y" ]; then
    echo "Setting up MIT certificate."
    python3 browser_auto/mit_cert.py
    CERTDIR=$(dirname $(find "$HOME/.mozilla" -name "cert9.db"))
    curl -o /tmp/mitca.crt https://ca.mit.edu/mitca.crt
    certutil -A -n "MIT" -t "TC,," -i /tmp/mitca.crt -d "$CERTDIR"
    rm /tmp/mitca.crt
fi

read -p "Set up GitHub/MIT GitHub access? [y/N] " GITHUB

if [ "$GITHUB" = "y" ]; then
    ssh-keygen -N "" -f $HOME/.ssh/id_rsa
    python3 browser_auto/github_add_ssh_key.py

    # TODO: do this better?
    git checkout home.nix
    home-manager switch
fi





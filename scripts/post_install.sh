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
    firefox 'https://accounts.firefox.com/signin?context=fx_desktop_v3&entrypoint=fxa_app_menu&action=email&service=sync' &
    read -p "Log into Firefox Sync"
    curl -fsSl https://raw.githubusercontent.com/tridactyl/tridactyl/master/native/install.sh -o /tmp/trinativeinstall.sh && bash /tmp/trinativeinstall.sh master

    # Selenium can't really mess with preferences.
    echo "Setting default search engine to DDG."
    firefox "about:preferences#search" > /dev/null 2>&1 &
    sleep 2
    $FFKEYS \
        windowfocus --sync \
        key --clearmodifiers --delay 200 Tab Tab d
fi

read -p "Set up MIT certificates? BROKEN RIGHT NOW [y/N] " MIT

if [ "$MIT" = "y" ]; then
    echo "Setting up MIT certificate."
    python3 browser_auto/mit_cert.py
    CERTDIR=$(dirname $(find "$HOME/.mozilla" -name "cert9.db"))
    curl -s -o /tmp/mitca.crt https://ca.mit.edu/mitca.crt
    certutil -A -n "MIT" -t "TC,," -i /tmp/mitca.crt -d "$CERTDIR"
    rm /tmp/mitca.crt

fi


read -p "Initialize email (might take a while, multiple attempts)? [y/N] " EMAIL

if [ "$EMAIL" = "y" ]; then
    for store in fastmail mit gmail csail; do
        mkdir -p $HOME/Maildir/$store
        mbsync $store
    done

    mu index
fi

read -p "Set up CSAIL WiFi? [y/N] " WIFI

if [ "$WIFI" = "y" ]; then
    ./net/CSAILPrivate.nmconnection.sh | sudo tee /etc/NetworkManager/system-connections/CSAILPrivate.nmconnection > /dev/null
    echo "You may need to reload your network connection."
fi

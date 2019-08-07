#!/bin/sh
cat << EOF
[connection]
id=CSAILPrivate
uuid=85818a95-3811-41e1-9356-2c64375d3cd4
type=wifi
permissions=user:zjn:;
timestamp=1565190188

[wifi]
mac-address-blacklist=
mode=infrastructure
seen-bssids=8A:15:14:5F:8C:30;8A:15:54:AC:FA:FF;
ssid=CSAILPrivate

[wifi-security]
key-mgmt=wpa-eap

[802-1x]
anonymous-identity=anonymous
ca-cert=${HOME}/dotfiles/net/AddTrust_External_Root.pem
eap=ttls;
identity=zjn
password=$(pass show csail)
phase2-auth=pap

[ipv4]
dns-search=
method=auto

[ipv6]
addr-gen-mode=stable-privacy
dns-search=
ip6-privacy=0
method=auto
EOF

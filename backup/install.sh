#!/bin/sh
set -e

unset PATH
for p in $buildInputs; do
    export PATH=$p/bin${PATH:+:}$PATH
done

mkdir -p $out/bin

for file in $src; do
    dest="${out}/bin/$(basename "$file" | cut -c 34-)" # cut out hash prefix
    sed \
        -e "s'{{RESTIC_BIN_PATH}}'${restic}/bin/restic'g" \
        -e "s'{{JQ_BIN_PATH}}'${jq}/bin/jq'g" \
        -e "s'{{PASS_BIN_PATH}}'${pass}/bin/pass'g" \
        "$file" > "$dest"
    chmod +x "$dest"
done


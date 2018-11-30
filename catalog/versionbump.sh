#/bin/bash

# set -x

version="0.2.2.1"

bump () {
    f="$1"
    if [[ -f "$f" ]]
    then
        perl -p -i~ \
             -e 's/\b[0-9]+[.][0-9]+[.][0-9]+[.][0-9]\b/'"$version"'/' \
             "$f"
        perl -p -i~ \
             -e 's/"[0-9]+[.][0-9]+[.][0-9]+[.][0-9]"/"'"$version"'"/' \
             "$f"
    fi
}

bump catalog.cabal
bump catalog-servant/catalog-servant.cabal
bump src/Catalog/Options.hs
bump data/assets/html/edit.html
bump data/assets/javascript/rpc-servant.js

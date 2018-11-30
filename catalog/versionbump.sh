#/bin/bash

# set -x

version="0.2.2.1"
dat=$(date "+%Y-%m-%d")

bump () {
    f="$1"
    if [[ -f "$f" ]]
    then
        # set the version, separator is word start and word end
        perl -p -i~ \
             -e 's/\b[0-9]+[.][0-9]+[.][0-9]+[.][0-9]\b/'"$version"'/' \
             "$f"
        # set the version, in quoted string format "0.0.0.0"
        perl -p -i~ \
             -e 's/"[0-9]+[.][0-9]+[.][0-9]+[.][0-9]"/"'"$version"'"/' \
             "$f"
        # set the date, in quoted string format "2000-01-01"
        perl -p -i~ \
             -e 's/\b[0-9]{4}-[0-9]{2}-[0-9]{2}\b/'"$dat"'/' \
             "$f"
        # set the date, in quoted string format "2000-01-01"
        perl -p -i~ \
             -e 's/"[0-9]{4}-[0-9]{2}-[0-9]{2}"/"'"$dat"'"/' \
             "$f"
    fi
}

# bump tmp.xxx

bump catalog.cabal
bump catalog-servant/catalog-servant.cabal
bump src/Catalog/Options.hs
bump data/assets/html/edit.html
bump data/assets/javascript/rpc-servant.js

set -x

git commit -a -m "version bump to $version"
git tag "catalog-$version-$dat"

stack install

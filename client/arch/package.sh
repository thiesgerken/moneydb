#!/usr/bin/bash
# this file is meant to be run in the repository root and dist/moneydb-client must be present.

rm -Rf .tmp/moneydb-client
mkdir -p .tmp/moneydb-client
cp dist/moneydb-client .tmp/moneydb-client

commit=$(git rev-parse --short HEAD)
checksum=$(md5sum .tmp/moneydb-client/moneydb-client)

sed -e "s/COMMIT/$commit/g" -e "s/CHECKSUM/${checksum%% *}/g" client/arch/PKGBUILD > .tmp/moneydb-client/PKGBUILD

(cd .tmp/moneydb-client && makepkg)
cp .tmp/moneydb-client/moneydb-client-2.1.0-1-x86_64.pkg.tar.xz dist && rm -Rf .tmp/moneydb-client

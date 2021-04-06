#!/usr/bin/bash
# this file is meant to be run in the repository root and dist/moneydb-server must be present.

rm -Rf .tmp/moneydb-server
mkdir -p .tmp/moneydb-server
mkdir -p .tmp/moneydb-server/DEBIAN
cp server/debian/deb-control.txt .tmp/moneydb-server/DEBIAN/control
cp server/debian/deb-conffiles.txt .tmp/moneydb-server/DEBIAN/conffiles

cp server/debian/deb-preinst.sh .tmp/moneydb-server/DEBIAN/preinst
chmod 755 .tmp/moneydb-server/DEBIAN/preinst

cp server/debian/deb-postinst.sh .tmp/moneydb-server/DEBIAN/postinst
chmod 755 .tmp/moneydb-server/DEBIAN/postinst

cp server/debian/deb-prerm.sh .tmp/moneydb-server/DEBIAN/prerm
chmod 755 .tmp/moneydb-server/DEBIAN/prerm

mkdir -p .tmp/moneydb-server/usr/bin
cp dist/moneydb-server .tmp/moneydb-server/usr/bin
chmod 755 .tmp/moneydb-server/usr/bin/moneydb-server

mkdir -p .tmp/moneydb-server/etc/moneydb
cp server/moneydb.cfg .tmp/moneydb-server/etc/moneydb/moneydb.cfg

mkdir -p .tmp/moneydb-server/etc/systemd/system
cp server/debian/moneydb-server.service .tmp/moneydb-server/etc/systemd/system/
cp server/debian/moneydb-backup.service  .tmp/moneydb-server/etc/systemd/system/
cp server/debian/moneydb-backup.timer  .tmp/moneydb-server/etc/systemd/system/

mkdir -p .tmp/moneydb-server/var/backups
cp server/debian/backup.sh .tmp/moneydb-server/var/backups/backup-moneydb.sh

mkdir -p .tmp/moneydb-server/usr/lib/sysusers.d
cp server/debian/sysusers.d .tmp/moneydb-server/usr/lib/sysusers.d/moneydb.conf

(cd .tmp && dpkg-deb --build moneydb-server)
cp .tmp/moneydb-server.deb dist && rm -Rf .tmp/moneydb-server

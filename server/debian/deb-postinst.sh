#!/bin/sh

chmod 755 /usr/bin/moneydb-server
chown -R root:root /etc/moneydb

chmod 644 /etc/systemd/system/moneydb-server.service /etc/systemd/system/moneydb-backup.service /etc/systemd/system/moneydb-backup.timer
chown root:root /etc/systemd/system/moneydb-server.service /etc/systemd/system/moneydb-backup.service /etc/systemd/system/moneydb-backup.timer

chmod 755 /var/backups/backup-moneydb.sh
chown root:root /var/backups/backup-moneydb.sh

systemctl daemon-reload
systemd-sysusers

systemctl enable moneydb-server.service && systemctl start moneydb-server.service
systemctl enable moneydb-backup.timer && systemctl start moneydb-backup.timer

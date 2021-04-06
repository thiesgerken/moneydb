#!/bin/bash
mkdir -p /var/backups/moneydb

if [ -x /usr/bin/moneydb-server ]; then
  DATE=$(date +%Y-%m-%d-%H%M%S)
  UF=/var/backups/moneydb/$DATE.json.xz
  sudo -u moneydb bash -c "moneydb-server -c /etc/moneydb/moneydb.cfg --export" | xz -z > $UF
  chmod 777 $UF
  echo "Exported database to $UF"
fi

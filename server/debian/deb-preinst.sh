#!/bin/bash

if [ -x /var/backups/backup-moneydb.sh ]; then
  /var/backups/backup-moneydb.sh
fi

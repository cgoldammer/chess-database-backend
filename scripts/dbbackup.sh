#!/usr/bin/env bash
set -e

APP=$1
DB_NAME=$2

BUCKET_NAME=chessinsightsbackup

TIMESTAMP=$(date +%F_%T | tr ':' '-')
TEMP_FILE=$(mktemp tmp.XXXXXXXXXX)
S3_FILE="s3://$BUCKET_NAME/$APP/$APP-backup-$TIMESTAMP"

pg_dump -Fc --no-acl -h localhost -U postgres $DB_NAME > ~/backup/$TEMP_FILE
s3cmd put ~/backup/$TEMP_FILE $S3_FILE --encrypt

# Delete backups older than 60 days
find ~/backup -maxdepth 1 -mtime +60 -type f -delete

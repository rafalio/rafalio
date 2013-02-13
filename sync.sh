#!/bin/bash

rsync --progress --stats --compress --rsh=/usr/bin/ssh --recursive --times --perms --links --delete --checksum _site/* radicality_rafalio@ssh.phx.nearlyfreespeech.net:/home/public


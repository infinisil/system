#!/usr/bin/env bash

#set -x xtrace
set -e
set -o pipefail

channel=Pyropuncher
key=AIzaSyBcjoRQhzYG7O8tTr2dwzgbkSlk0-vhAic
idDir=/home/infinisil/ids

if [ ! -d $idDir ]; then
	mkdir -p $idDir
fi
touch $idDir/$channel

uploadsId=$(curl -s https://www.googleapis.com/youtube/v3/channels\?key\=$key\&forUsername\=$channel\&part\=contentDetails | jq -r ".items[0].contentDetails.relatedPlaylists.uploads")

videoIds=$(curl -s https://www.googleapis.com/youtube/v3/playlistItems\?key\=$key\&playlistId\=$uploadsId\&part\=contentDetails\&maxResults\=5 | jq -r ".items[].contentDetails.videoId" | tr " " "\n")
videoIds=$(tac <(echo $videoIds))

oldIds=$(cat $idDir/$channel)

newIds=$(diff --changed-group-format='%>' --unchanged-group-format='' <(echo $oldIds) <(echo $videoIds) || true)

for id in $newIds; do
	echo Downloading $id
	youtube-dl -x -f m4a --add-metadata --embed-thumbnail --xattrs -o "/youtube/%(title)s.%(ext).s" $id
	beet import -s /youtube/*
	rm /youtube/*
	echo $id >> $idDir/$channel
done




#!/bin/sh
set -e # Fail if any command fails
set -x xtrace # Print and trace commands

SETS=(
	"main/test/a"
	"main/test/b"
)

for item in "${SETS[@]}"
do
	echo $item
done


exit

CLIENTPORT=23456
NAME=$(uname -n)
LOCALPOOL=main
REMOTEPOOL=main
LOCALSET=global
REMOTESET=global
LOCAL=$LOCALPOOL/$LOCALSET
REMOTE=$REMOTEPOOL/$REMOTESET

case $NAME in
nixos)
  SSH="ssh root@infinisil.io"
  ;;
dobby)
  SSH="ssh -p $CLIENTPORT localhost"
  TEMP=$REMOTE
  REMOTE=$LOCAL
  LOCAL=$TEMP
  ;;
esac

# Don't sync if there are no local changes
if [ -z "$(zfs diff $LOCAL@last $LOCAL)" ]; then
  echo No local changes!
  if [ "$1" == "-f" ]; then
	  echo Syncing local to remote anyways..
  else
	  echo Use the -f flag to sync local to remote anyways
	  exit
  fi
fi

# Don't sync without the -f flag if there are both local and remote changes
if [ ! -z "$($SSH zfs diff $REMOTE@last $REMOTE)" ]; then
  echo Remote has changes!
  if [ "$1" == "-f" ]; then
	  echo Using local version..
  else
	  echo Use the -f flag to use the local version
	  exit 1
  fi
fi

echo Syncing local version to remote..

if [ ! -z "$(zfs list -Ho name -t snapshot | grep $LOCAL@next)" ]; then
echo Local snapshot $LOCAL@next exists, destroying..
zfs destroy $LOCAL@next
fi

zfs snapshot $LOCAL@next
zfs send -Rvi $LOCAL@last $LOCAL@next | $SSH zfs receive -Fv $REMOTE
zfs destroy $LOCAL@last
zfs rename $LOCAL@next last
$SSH "zfs destroy $REMOTE@last;zfs rename $REMOTE@next last"

echo Synced successfully!

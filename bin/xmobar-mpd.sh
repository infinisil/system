#!/bin/sh

MPD="$(mpc | grep -)"

if [ -z "$MPD" ]; then
	echo ""
else
	#PCNT="$(mpc | grep -o '([0-9]*%)')"
	if [[ $(mpc | grep -o '\[paused\]') ]]; then
		SYMB=""
	else
		SYMB=""
	fi
	echo "$SYMB $MPD | "
fi

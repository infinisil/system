#! /usr/bin/env nix-shell
#! nix-shell -i bash -p bc
bc <<< "scale=1; $(cat /sys/class/power_supply/BAT0/current_now)/1000000"

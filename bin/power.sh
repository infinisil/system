#! /usr/bin/env bash
bc <<< "scale=1; $(cat /sys/class/power_supply/BAT0/current_now)/1000000"

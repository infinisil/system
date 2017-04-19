#!/usr/bin/env bash

DIR=/sys/class/backlight/intel_backlight

echo "$(( $(cat $DIR/brightness) + ($(cat $DIR/max_brightness) - $(cat $DIR/brightness)) / 5 ))" > $DIR/brightness

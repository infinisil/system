#!/usr/bin/env bash

DIR=/sys/class/backlight/intel_backlight

echo "$(( $(cat $DIR/brightness) * 4 / 5 ))" > $DIR/brightness

#!/usr/bin/env bash
set -o xtrace
INFO=$(xwininfo -frame -root)
WIN_GEO=$(echo $INFO | grep -oEe 'geometry [0-9]+x[0-9]+' | grep -oEe '[0-9]+x[0-9]+')
WIN_XY=$(echo $INFO | grep -oEe 'Corners:\s+\+[0-9]+\+[0-9]+' | grep -oEe '[0-9]+\+[0-9]+' | sed -e 's/\+/,/' )

ffmpeg -f x11grab -y -r 30 -s $WIN_GEO -i :0.0+$WIN_XY -vcodec ffv1 -qscale 0 output.avi

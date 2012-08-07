#!/bin/sh

# Load custom colorscheme
xrdb ~/.Xresources

# Active Num Lock
numlockx on

# Xinerama
nscreens=`xrandr | grep "\bconnected" | wc -l`
if [ $nscreens == "2" ]; then two_screens; fi

# Background color and image
xsetroot -solid black
xloadimage -onroot -border black .xconfig/Background.jpg

# Launch xmonad
xmonad

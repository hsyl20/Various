#!/bin/sh

# Load custom colorscheme
xrdb ~/.Xresources

# Active Num Lock
numlockx on

# Set keymap
setxkbmap fr oss_latin9

# Set right window key as compose key
setxkbmap -option compose:rwin

# Xinerama
nscreens=`xrandr | grep "\bconnected" | wc -l`
if [ $nscreens == "2" ]; then two_screens; fi

# Background color and image
xsetroot -solid black
#xloadimage -onroot -center -border black .xconfig/Background.jpg

# Launch xmonad
xmonad

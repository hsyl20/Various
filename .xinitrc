#!/bin/sh

# Load custom colorscheme
xrdb ~/.Xresources

# Active Num Lock
numlockx on

xsetroot -solid black
xloadimage -onroot -center .xconfig/Background.jpg

# Launch xmonad
xmonad

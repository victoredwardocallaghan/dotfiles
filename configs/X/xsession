#!/bin/sh

#
# ~/.xsession
#
# Executed by xdm/gdm/kdm at login
#

/bin/bash --login -i ~/.xinitrc

# Load resources
 
xrdb -merge .Xresources

# Set up an icon tray
 
trayer --edge top --align right --SetDockType true --SetPartialStrut true \
 --expand true --width 10 --transparent true --tint 0x191970 --height 12 &

# Set the background color<
 
#xsetroot -solid midnightblue
 
# Fire up apps
 
#xscreensaver -no-splash &

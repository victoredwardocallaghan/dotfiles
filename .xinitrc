#!/bin/sh
#
# ~/.xinitrc
#

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi


## Generate custom keymap table
## xmodmap -pke > ~/.Xmodmap
#
#if [ -f $HOME/.Xmodmap ]; then
#    /usr/bin/xmodmap $HOME/.Xmodmap
#fi


# Make sure this is before the 'exec' command or it won't be sourced.
[ -f /etc/xprofile ] && source /etc/xprofile
[ -f ~/.xprofile ] && source ~/.xprofile


[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources


# setup an icon tray
#trayer --edge top --align right --SetDockType true --SetPartialStrut true \
# --expand true --widthtype request --transparent true --tint 0x191970 --height 9 &


## $ systemctl --user enable mpd
## $ systemctl --user start mpd
#run systemd as user instance
systemd --user &


# Setup fast terminal lunch
urxvtd -q -f -o


# Fast startup
thunar --daemon &


# Executed by startx (run your window manager from here)
# exec gnome-session
# exec startkde
# exec startxfce4
# ...or the Window Manager of your choice
#exec ck-lunch-session xmonad

# bring up window manager
exec xmonad

###############################


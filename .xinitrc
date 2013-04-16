#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

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

[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

# exec gnome-session
# exec startkde
# exec startxfce4
# ...or the Window Manager of your choice
#exec ck-lunch-session xmonad

###############################

# setup screen locking
xscreensaver -no-splash &

# screen power save tweaks
xset +dpms
xset dpms 0 0 300

# human eyes saving tweaks
redshift &

# mouse speed tweaks
xset mouse 7 1

# Setup X Compose for entering Unicode characters
# e.g., “Compose+'+a*” for ‘α’.
setxkbmap -option compose:rctrl
# This remaps Alt_R to AltGr, granting access to ¬ (AltGr+\) for making ¬_¬ smileys
# and giving me simpler key combos for commonly-used glyphs like µ (AltGr+m) and © (AltGr+c).
setxkbmap -option lv3:ralt_switch

## !! NOTE WELL !!: Setup thinkpad keyboard layout for T series
# Find current by: setxkbmap -query
# setxkbmap us
#setxkbmap -model thinkpad60

# setup media key binding
xbindkeys &

# setup an icon tray
#trayer --edge top --align right --SetDockType true --SetPartialStrut true \
# --expand true --widthtype request --transparent true --tint 0x191970 --height 9 &

## $ systemctl --user enable mpd
## $ systemctl --user start mpd
#run systemd as user instance
systemd --user &

# setup fast terminal lunch
urxvtd -q -f -o

# bring up window manager
exec xmonad

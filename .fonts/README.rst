
INSTALL
=======

Just download and put into your ~/.fonts dir and run ``fc-cache -vf``

If you're using urxvt add something like this to your ~/.Xresources or ~/.Xdefaults::

  !!  Inconsolata
  URxvt*font            : xft:Inconsolata for Powerline:regular:size=12
  URxvt*imFont          : xft:Inconsolata for Powerline:regular:size=12
  URxvt*boldFont        : xft:Inconsolata for Powerline:bold:size=12
  URxvt*italicFont      : xft:Inconsolata for Powerline:italic:size=12
  URxvt*boldItalicFont  : xft:Inconsolata for Powerline:bold:italic:size=12

  !! Monaco
  URxvt*font:                 xft:Monaco for Powerline:regular:size=8
  URxvt*imFont:               xft:Monaco for Powerline:regular:size=8
  URxvt*boldFont:             xft:Monaco for Powerline:bold:size=8
  URxvt*italicFont:           xft:Monaco for Powerline:italic:size=8
  URxvt*boldItalicFont:       xft:Monaco for Powerline:bold:italic:size=8

  !! Anonymous Pro
  URxvt*font            : xft:Anonymous Pro for Powerline:regular:size=12
  URxvt*imFont          : xft:Anonymous Pro for Powerline:regular:size=12
  URxvt*boldFont        : xft:Anonymous Pro for Powerline:bold:size=12
  URxvt*italicFont      : xft:Anonymous Pro for Powerline:italic:size=12
  URxvt*boldItalicFont  : xft:Anonymous Pro for Powerline:bold:italic:size=12

Make sure to run ``xrdb -merge ~/.Xresources`` afterwards and close all urxvt terminals.



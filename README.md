# dotfiles

========

World of configuration files managed by [`biegunka`][biegunka].

[![Build Status](https://drone.io/github.com/victoredwardocallaghan/dotfiles/status.png)](https://drone.io/github.com/victoredwardocallaghan/dotfiles/latest)


## What is it?

This is [`biegunka`][biegunka]-powered repository containing my configuration
files. It has nice mechanism for maintain many working environments with
different configurations.


## How I'm using it?

`dotfiles` is a simple `cabal` package. So, at first, it should be compiled to
something executable:

```
$> cabal install biegunka/dotfiles.cabal
```


=======

My config files.

Requirements;
 * ghc and clang installed.
 * ctags
 * cabal install ghc-mod lushtags

 * rtorrent
 * mutt - email client
 * abook - address book for mutt
 * dwb - web browser
 * lynx - terminal web browser
 * xmobar - status bar
 * xmonad - window manager
 * urxvt - terminal emulator
 * trayer - ensures space for status bar
 * colordiff
 * htop
 * tree
 * tmux - terminal multiplexer
 * xscreensaver
 * xset
 * xbindkeys

 FIXME:
 Automate this in plugin: cd .vim/bundle/vimproc && make -f make_unix.mak
 ==> Run 'pkgfile --update' to initialize the database

[biegunka]: https://github.com/biegunka

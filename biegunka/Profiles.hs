{-# LANGUAGE DataKinds     #-}
module Profiles where
import           Control.Lens
import           Data.Default          (def)

import           Biegunka
import           Biegunka.Source.Git

dotfiles :: Script Actions () -> Script Sources ()
dotfiles as = git' "git@github.com:victoredwardocallaghan/dotfiles" "projects/edward/dotfiles" $ def & actions .~ as

profile_vim :: Script Profiles ()
profile_vim = do
  profile "vim/pathogen/meta" $ do
    git "git@github.com:tpope/vim-pathogen.git" "projects/misc/vim-pathogen" $
      copy "autoload/pathogen.vim" ".vim/autoload/pathogen.vim"
  profile "vim/pathogen/modules" $ do
    git "git@github.com:Shougo/vimproc.git" ".vim/bundle/vimproc" $
      shell "make -f make_unix.mak"
    git_ "git@github.com:Shougo/neocomplcache.git" ".vim/bundle/neocomplcache"
    git_ "git@github.com:Shougo/unite.vim.git" ".vim/bundle/unite"
    git_ "git@github.com:airblade/vim-gitgutter.git" ".vim/bundle/gitgutter"
    git_ "git@github.com:dahu/Insertlessly.git" ".vim/bundle/Insertlessly"
    git_ "git@github.com:godlygeek/tabular.git" ".vim/bundle/tabular"
    git_ "git@github.com:scrooloose/syntastic.git" ".vim/bundle/syntastic"
    git_ "git@github.com:spolu/dwm.vim.git" ".vim/bundle/dwm"
    git_ "git@github.com:supki/vim-perds.git" ".vim/bundle/perds"
    git_ "git@github.com:tpope/vim-commentary.git" ".vim/bundle/commentary"
    git_ "git@github.com:tpope/vim-markdown.git" ".vim/bundle/markdown"
    git_ "git@github.com:tpope/vim-surround.git" ".vim/bundle/surround"
    git_ "git@github.com:ujihisa/neco-ghc.git" ".vim/bundle/neco-ghc"
    git_ "git@github.com:jvoorhis/coq.vim.git" ".vim/bundle/coq"
    git_ "git@github.com:trefis/coquille.git" ".vim/bundle/coquille"
    git_ "git@github.com:def-lkb/vimbufsync.git" ".vim/bundle/bufsync"
  profile "vim/rc" $
    dotfiles $ copy "configs/vim/vimrc" ".vimrc"
  profile "vim/syntax" $
    dotfiles $ copy "configs/vim/syntax/haskell.vim" ".vim/after/syntax/haskell.vim"
  profile "vim/colorschemes" $
    dotfiles $ copy "configs/vim/colors/neverland-darker.vim" ".vim/colors/neverland-darker.vim"

profile_xmonad :: Script Profiles ()
profile_xmonad = do
  profile "xmonad/xmonad.hs" $
    dotfiles $
      substitute "configs/xmonad/xmonad.hs.template" ".xmonad/xmonad.hs"
  profile "xmonad/xmobar" $ do
    dotfiles $ do
      copy "configs/xmonad/xmobar-top.hs" ".xmonad/xmobar-top.hs"
      copy "configs/xmonad/xmobar.hs" ".xmonad/xmobar.hs"
      copy "configs/xmonad/xmobarrc" ".xmobarrc"

profile_git :: Script Profiles ()
profile_git = profile "git" $ do
  dotfiles $ do
    substitute "configs/git/gitconfig.template" ".gitconfig"
    copy "configs/git/gitignore" ".gitignore"
  git_ "git@github.com:nvie/gitflow.git" "projects/misc/gitflow"
    -- install?
  git_ "git@github.com:arc90/git-sweep.git" "projects/misc/git-sweep"
    -- install?

profile_x :: Script Profiles ()
profile_x = profile "X" $ do
  dotfiles $ do
    substitute "configs/X/Xresources.template" ".Xresources"
    copy "configs/X/Xresources.colours" ".Xresources.colours"
    copy "configs/X/XCompose" ".XCompose"
    copy "configs/X/inputrc" ".inputrc"
    copy "configs/X/xprofile" ".xprofile"
    copy "configs/X/xsession" ".xsession"
    copy "configs/X/xinitrc" ".xinitrc"
    copy "configs/X/xbindkeysrc" ".xbindkeysrc"
    copy "configs/X/Xmodmap" ".Xmodmap"

profile_ghc :: Script Profiles ()
profile_ghc = profile "ghc" $ do
  dotfiles $
    copy "configs/ghc/ghci" ".ghci"
  git_ "git@github.com:eagletmt/ghcmod-vim.git" ".vim/bundle/ghcmod-vim"
  git_ "git@github.com:bitc/vim-hdevtools.git" ".vim/bundle/hdevtools"

profile_irssi :: Script Profiles ()
profile_irssi = profile "irssi" $
  dotfiles $ do
    copy "configs/irssi/bleeding.theme" ".irssi/bleeding.theme"
    copy "configs/irssi/config" ".irssi/config"

profile_mpd :: Script Profiles ()
profile_mpd = profile "mpd" $
  dotfiles $ do
    copy "configs/mpd/mpdconf" ".mpdconf"
    copy "configs/mpd/ncmpcpp/config" ".ncmpcpp/config"

profile_mutt :: Script Profiles ()
profile_mutt = profile "mutt" $
  dotfiles $
    copy "configs/mutt" ".mutt"

profile_pentadactyl :: Script Profiles ()
profile_pentadactyl = profile "pentadactyl" $
  dotfiles $ do
    substitute "configs/pentadactyl/colors/pemees.penta.template" ".pentadactyl/colors/pemees.penta"
    copy "configs/pentadactyl/pentadactylrc" ".pentadactylrc"
    copy "configs/pentadactyl/plugins/buftabs.js" ".pentadactyl/plugins/buftabs.js"

profile_ackrc :: Script Profiles ()
profile_ackrc = profile "ack" $
  dotfiles $
    copy "configs/ack/ackrc" ".ackrc"

profile_apvlv :: Script Profiles ()
profile_apvlv = profile "apvlv" $
  dotfiles $
    copy "configs/apvlv/apvlvrc" ".apvlvrc"

profile_rtorrent :: Script Profiles ()
profile_rtorrent = profile "rtorrent" $
  dotfiles $
    copy "configs/rtorrent/rtorrentrc" ".rtorrentrc"

profile_shell :: Script Profiles ()
profile_shell = do
  profile "shell/bash" $
    dotfiles $ do
      copy "configs/shell/bash/bashrc" ".bashrc"
      copy "configs/shell/bash/bash_profile" ".bash_profile"
      copy "configs/shell/bash/bash_logout" ".bash_logout"
      copy "configs/shell/bash/bash_alias" ".bash_alias"
      copy "configs/shell/bash/bash_functions" ".bash_functions"
  profile "shell/zsh" $
    dotfiles $
      copy "configs/shell/zsh/zshrc" ".zshrc"

profile_icons :: Script Profiles ()
profile_icons = profile "icons" $
  dotfiles $
    copy "configs/icons" ".icons"

profile_gtk :: Script Profiles ()
profile_gtk = profile "gtk" $
  dotfiles $ do
    copy "configs/gtk/gtkrc-2.0" ".gtkrc-2.0"
    copy "configs/gtk/gtkrc.mine" ".gtkrc.mine"

profile_mocp :: Script Profiles ()
profile_mocp = profile "mocp" $
  dotfiles $
    copy "configs/moc/config" ".moc/config"

profile_tmux :: Script Profiles ()
profile_tmux = profile "tmux" $
  dotfiles $
    copy "configs/tmux/conf" ".tmux.conf"

profile_vifm :: Script Profiles ()
profile_vifm = profile "vifm" $
  dotfiles $ do
    copy "configs/vifm/colors/neverland" ".vifm/colors/neverland"
    copy "configs/vifm/vifmrc" ".vifm/vifmrc"

profile_zathura :: Script Profiles ()
profile_zathura = profile "zathura" $
  dotfiles $
    copy "configs/zathura/zathurarc" ".config/zathura/zathurarc"

profile_misc :: Script Profiles ()
profile_misc = do
  profile "misc/hpasteit" $
    git_ "git@github.com:parcs/hpasteit.git" "projects/misc/hpasteit"

profile_urxvt :: Script Profiles ()
profile_urxvt = profile "urxvt" $
  dotfiles $
    copy "configs/urxvt" ".urxvt"

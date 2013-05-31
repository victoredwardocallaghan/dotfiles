module Environment.T530 where

import Data.Default (def)

import Environment.Base
import Profiles


settings :: Template
settings = def
  { git = def
    { set_user = True
    , user_name = "Edward O'Callaghan"
    , user_email = "victoredwardocallaghan@gmail.com"
    }
  , pentadactyl = def
    { font_size = 8
    }
  , xmonad = def
    { font = "xft:Anonymous Pro for Powerline"
    }
  , x = def
    { user = "m"
    , xft_dpi = 96
    , xft_font_size = 12
    , xft_font = "xft:Anonymous Pro for Powerline:regular:antialias=true"
    , xft_bold_font = "xft:Anonymous Pro for Powerline:bold"
    , xft_italic_font = "xft:Anonymous Pro for Powerline:italic"
    , xft_boldItalic_font = "xft:Anonymous Pro for Powerline:bold:italic"
    , xft_im_font = "xft:Anonymous Pro for Powerline:regular"
    }
  }

profiles = sequence_
  [ profile_ackrc
  , profile_ghc
  , profile_git
  , profile_gtk
  , profile_misc
  , profile_mpd
  , profile_pentadactyl
  , profile_shell
  , profile_tmux
  , profile_vifm
  , profile_vim
  , profile_x
  , profile_xmonad
  , profile_zathura
  ]


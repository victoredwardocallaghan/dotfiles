{-# LANGUAGE DeriveDataTypeable #-}
module Environment.Template where

import Data.Data (Data)
import Data.Typeable (Typeable)

import Data.Default

data Template = Template
  { git :: Git
  , pentadactyl :: Pentadactyl
  , xmonad :: Xmonad
  , xmobar :: Xmobar
  , urxvt :: Urxvt
  , x :: X
  } deriving (Data, Typeable)

instance Default Template where
  def = Template
    { git = def
    , pentadactyl = def
    , xmonad = def
    , xmobar = def
    , urxvt = def
    , x = def
    }

data Git = Git
  { set_user :: Bool
  , user_email :: String
  , user_name :: String
  } deriving (Data, Typeable)

data Pentadactyl = Pentadactyl
  { font_size :: Int
  } deriving (Data, Typeable)

data Xmonad = Xmonad
  { font :: String
  } deriving (Data, Typeable)

data Xmobar = Xmobar
  { background, position :: String
  , battery :: Maybe String
  } deriving (Data, Typeable)

data Urxvt = Urxvt
  { tabbedex, background_, browser :: String
  } deriving (Data, Typeable)

data X = X
  { user :: String
  , xft_dpi :: Int
  , xft_font_size :: Int
  , xft_font :: String
  , xft_bold_font :: String
  , xft_italic_font :: String
  , xft_boldItalic_font :: String
  , xft_im_font :: String
  } deriving (Data, Typeable)

instance Default Git where
  def = Git
    { set_user = False
    , user_name = def
    , user_email = def
    }

instance Default Pentadactyl where
  def = Pentadactyl
    { font_size = def
    }

instance Default Xmonad where
  def = Xmonad
    { font = def
    }

instance Default Xmobar where
  def = Xmobar
    { background = def
    , position = def
    , battery = Just "\"\""
    }

instance Default Urxvt where
  def = Urxvt
    { tabbedex = def
    , background_ = def
    , browser = def
    }

instance Default X where
  def = X
    { user = def
    , xft_dpi = def
    , xft_font_size = def
    , xft_font = def
    , xft_bold_font = def
    , xft_italic_font = def
    , xft_boldItalic_font = def
    , xft_im_font = def
    }

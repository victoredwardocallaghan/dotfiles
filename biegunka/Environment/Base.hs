{-# LANGUAGE DeriveDataTypeable #-}
module Environment.Base where

import Data.Data (Data)
import Data.Typeable (Typeable)

import Data.Default

data Template = Template
  { git :: Git
  , pentadactyl :: Pentadactyl
  , xresource_scratchpad :: XresourceScratchpad
  , xresource_shiva :: XresourceShiva
  , xmonad :: Xmonad
  , x :: X
  } deriving (Data, Typeable)

instance Default Template where
  def = Template
    { git = def
    , pentadactyl = def
    , xresource_scratchpad = def
    , xresource_shiva = def
    , xmonad = def
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

data X = X
  { user :: String
  , xft_dpi :: Int
  , font_size :: Int
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
    { terminus_font = def
    }

instance Default X where
  def = X
    { user = def
    , xft_dpi = def
    }

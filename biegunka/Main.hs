import Biegunka
import Biegunka.Source.Git
import Control.Lens


main :: IO ()
main = do
  biegunka (set root "~") script (execute id)
 where
  script = do
    profile "dotfiles" $ do
      git "git@github.com:victoredwardocallaghan/dotfiles" ".dotfiles" $ do
        link "configs/xmonad.hs" ".xmonad/xmonad.hs"
        link "configs/vimrc"     ".vimrc"

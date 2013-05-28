-------------------------------------------------------------------------------
-- ~/.xmonad/xmonad.hs                                                       --
-------------------------------------------------------------------------------
-- Author: Edward O'Callaghan <victoredwardocallaghan[at]gmail.com>          --
-- http://github.com/victoredwardocallaghan                                  --
-------------------------------------------------------------------------------
-- versions used atoc (on ArchLinux):                                        --
-- |  ghc                           -> 7.6.3-1                               --
-- |  haskell-haskeline             -> 0.7.0.3-3                             --
-- |  haskell-mtl                   -> 2.1.2-3                               --
-- |  haskell-parsec                -> 3.1.3-3                               --
-- |  haskell-stm                   -> 2.4.2-2                               --
-- |  haskell-terminfo              -> 0.3.2.5-3                             --
-- |  haskell-utf8-string           -> 0.3.7-5                               --
-- |  haskell-x11                   -> 1.6.1.1-3                             --
-- |  haskell-x11-xft               -> 0.3.1-9                               --
-- |  xmonad                        -> 0.11-6                                --
-- |  xmonad-contrib                -> 0.11-7                                --
-- |  xmobar                        -> 0.17-1                                --
-------------------------------------------------------------------------------


-- IMPORTS {{{

import Control.Monad (liftM)
import XMonad hiding ( (|||) )
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import Data.Ratio
import System.IO
import System.Exit

-- <actions>
import XMonad.Actions.PerWorkspaceKeys
--import XMonad.Actions.CycleWS (nextWS,prevWS,toggleWS,shiftToNext,shiftToPrev)
import XMonad.Actions.CycleWS
import XMonad.Actions.DwmPromote
import XMonad.Actions.RotSlaves (rotAllUp,rotAllDown,rotSlavesDown,rotSlavesUp)
import XMonad.Actions.GridSelect
import XMonad.Actions.WindowGo
import XMonad.Actions.SwapWorkspaces
import XMonad.Actions.FloatKeys (keysMoveWindow,keysResizeWindow)
import XMonad.Actions.WithAll
import XMonad.Actions.Submap
import XMonad.Actions.UpdatePointer
import qualified XMonad.Actions.Submap as SM
import qualified XMonad.Actions.Search as S

-- <hooks>
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageHelpers
--import XMonad.Hooks.ManageDocks (avoidStruts,avoidStrutsOn,ToggleStruts(..))
import XMonad.Hooks.ManageDocks
--import XMonad.Hooks.EwmhDesktops (ewmhDesktopsStartup)
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.FadeInactive

-- <utilities>
import XMonad.Util.Cursor
import XMonad.Util.Run(spawnPipe)
--import XMonad.Util.Scratchpad (scratchpadManageHook,scratchpadSpawnActionCustom)
import XMonad.Util.Scratchpad
import XMonad.Util.SpawnOnce
import XMonad.Util.EZConfig(additionalKeys)

-- <prompts>
import XMonad.Prompt
import qualified XMonad.Prompt as P
import XMonad.Prompt.XMonad
import XMonad.Prompt.Shell
import XMonad.Prompt.Email
import XMonad.Prompt.AppendFile
import XMonad.Prompt.Man (manPrompt)
import XMonad.Prompt.AppLauncher as AL
import XMonad.Prompt.AppendFile (appendFilePrompt)
import XMonad.Prompt.Window (windowPromptBring,windowPromptGoto)

-- <layouts>
import XMonad.Layout.OneBig
import XMonad.Layout.TwoPane
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.Grid
import XMonad.Layout.Magnifier
import XMonad.Layout.TabBarDecoration
--import XMonad.Layout.HintedTile
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.Spiral
import XMonad.Layout.IM

-- <layout helpers>
import XMonad.Layout.Minimize
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Combo
import XMonad.Layout.LimitWindows
import XMonad.Layout.NoBorders
import XMonad.Layout.Gaps
import XMonad.Layout.Reflect
import XMonad.Layout.Master
import XMonad.Layout.CenteredMaster
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Named
import XMonad.Layout.WindowNavigation

-- end of IMPORTS }}}


-- VARIABLES {{{

------------------------------------------------------------------------
-- *********************************************************************
-- ** Customisation application variables **
--
myFileManager = "thunar"
myPDFReader = "mupdf"
myBrowser = "/usr/bin/dwb"
myMail = "/usr/bin/urxvt -name Mail -e mutt"
myChat = "/usr/bin/urxvt -name Chat -e irssi"
myNews = "/usr/bin/urxvt -name News -e snownews"
myTorrent = "/usr/bin/urxvt -name Downloads -e rtorrent"
myMedia = "/usr/bin/urxvt -name Media -e ncmpcpp -s media-library"
--myEditor = "/usr/bin/urxvt -e rvim"
myEditor = "/usr/bin/gvim "
myScreenLock = "/usr/bin/xscreensaver-command -lock"

myFocusedBoarderColor = "orange"
myFont = "xft:Anonymous Pro for Powerline"
myHome = "/home/edward"


------------------------------------------------------------------------
-- Terminal
-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal = "/usr/bin/urxvtc"

-- end of VARIABLES }}}


-- WORKSPACES/STATUSBAR {{{

------------------------------------------------------------------------
-- Workspaces
-- The default number of workspaces (virtual screens) and their names.
--
myWorkspaces = ["α:web", "β:irc" ,"γ:code", "δ:mail", "ε:media", "ζ:documents", "η:files", "θ", "ι"]


------------------------------------------------------------------------
-- Window rules

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook :: ManageHook
myManageHook = scratchpadManageHookDefault <+> manageDocks
               <+> fullscreenManageHook <+> myFloatHook
               <+> manageHook defaultConfig
  where fullscreenManageHook = composeOne [ isFullscreen -?> doFullFloat ]

myFloatHook = composeAll
    [ className =? "XMathematica"          --> doFloat
    , className =? "Xzgv"                  --> doFloat
    , className =? "Firefox"               --> moveToWeb
    , className =? "Chromium"              --> moveToWeb
    , className =? "Steam"                 --> moveToMisc
    , className =? "Gvim"                  --> moveToCode
    , className =? "Apvlv"                 --> moveToDocum
    , className =? "Thunar"                --> moveToFiles
    , className =? "Vlc"                   --> moveToMedia
    , appName   =? "Downloads"             --> moveToFiles
    , appName   =? "News"                  --> moveToMail
    , appName   =? "Mail"                  --> moveToMail
    , appName   =? "Media"                 --> moveToMedia
    , appName   =? "Chat"                  --> moveToIM
    , className =? "Pidgin"                --> moveToIM
    , classNotRole ("Pidgin", "")          --> doFloat
    , className =? "Skype"                 --> moveToIM
    , classNotRole ("Skype", "MainWindow") --> doFloat
    , className =? "Gajim"                 --> moveToIM
    , classNotRole ("Gajim", "roster")     --> doFloat
    , manageDocks]
  where
    moveToWeb   = doF $ W.shift "α:web"
    moveToIM    = doF $ W.shift "β:irc"
    moveToCode  = doF $ W.shift "γ:code"
    moveToMail  = doF $ W.shift "δ:mail"
    moveToMedia = doF $ W.shift "ε:media"
    moveToDocum = doF $ W.shift "ζ:documents"
    moveToFiles = doF $ W.shift "η:files"
    moveToMisc  = doF $ W.shift "θ"

    classNotRole :: (String, String) -> Query Bool
    classNotRole (c,r) = className =? c <&&> role /=? r

    role = stringProperty "WM_WINDOW_ROLE"


------------------------------------------------------------------------
-- Status bars and logging
-- Perform an arbitrary action on each internal state change or X event.
-- See the 'DynamicLog' extension for examples.
--
-- To emulate dwm's status bar
--
-- > logHook = dynamicLogDzen
--
myLogHook xmproc = do
  ewmhDesktopsLogHook
  dynamicLogWithPP $ xmoPP xmproc
  updatePointer (Relative (1/20) (1/20))
  fadeInactiveLogHook fadeAmount
   where fadeAmount = 0.8


------------------------------------------------------------------------
-- Config for XMobar
--
xmoPP :: Handle -> PP
xmoPP xmproc = xmobarPP {
      ppTitle = xmobarColor xmobarTitleColor "" . shorten 100
    , ppCurrent = xmobarColor xmobarCurrentWorkspaceColor ""
    , ppSep = "  "
    , ppUrgent = xmobarColor "#FFFFAF" "" . wrap "[ " " ]"
    , ppWsSep = "  "
    , ppLayout = \x -> case x of
                      "Mirror ResizableTall"   -> "MTiled"
                      "ResizableTall"          -> "Tiled"
                      "Tabbed Bottom Simplest" -> "Tabbed"
                      "Tabbed Simplest"        -> "Tabbed"
                      _                        -> x
    , ppOutput = hPutStrLn xmproc
    }

-- end of WORKSPACES/STATUSBAR }}}


-- LAYOUTS {{{

------------------------------------------------------------------------
-- Layouts
-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
--myLayout = avoidStrutsOn[U]
myLayout = avoidStruts
             $ windowNavigation
             $ onWorkspace "β:irc" chatLayout
             $ onWorkspace "ζ:documents" docLayout
             collectiveLayouts
  where
     collectiveLayouts = tiled
           ||| tabs
           ||| Full
           ||| Tall 1 (3/100) (1/2)
           ||| Mirror (Tall 1 (3/100) (1/2))
           ||| noBorders (fullscreenFull Full)
           ||| magnify Grid
           ||| spiral (4/3)
     -- default tiling algorithm partitions the screen into two panes
     tiled   = ResizableTall nmaster delta ratio []
     -- The default number of windows in the master pane
     nmaster = 1
     -- Default proportion of screen occupied by master pane
     ratio   = 8 % 13
     -- Percent of screen to increment by when resizing panes
     delta   = 3 % 100
     -- tabbed layout
     tabs = tabbed shrinkText tabConfig
     -- magnification in grid
     magnify = magnifiercz (13%10)

     -- <define layouts>
     myFull = named "*" (noBorders (fullscreenFull Full))
     myTall = named "@" (smartBorders (Tall 1 (3/100) (1/2)))
     --myTile = named "+" (lessBorders (OnlyFloat) (withBorder 1 (limitWindows 5 (ResizableTall 1 0.03 0.5 []))))

     -- <layouts per workspace>
     chatLayout = withIM (1%7) (ClassName "Pidgin") Grid
     docLayout  = myTall ||| myFull

-- Learn from this:..
--myLayouts = avoidStruts $ windowNavigation  $
----            gaps [(U,15)] $ minimize        $
--            minimize                        $
--            mkToggle (single NBFULL)        $
--            mkToggle (single REFLECTX)      $
--            mkToggle (single REFLECTY)      $
--            mkToggle (single NOBORDERS)     $
--            onWorkspace "1" favorLayout     $
--            onWorkspace "4" inetLayouts     $
--            onWorkspace "5" fotoLayouts     $
--            (collectiveLayouts)
--  where
--    collectiveLayouts = myOneB ||| myTile ||| myFull
--
--    -- <define layouts>
--    myFull = named "*" (smartBorders (noBorders Full))
--    myOneB = named "@" (smartBorders (withBorder 1 (limitWindows 10 (OneBig 0.75 0.65))))
--    myTile = named "+" (lessBorders (OnlyFloat) (withBorder 1 (limitWindows 5 (ResizableTall 1 0.03 0.5 []))))
--    myUniq = named "=" (toggleLayouts (lessBorders (OnlyFloat) (noBorders (topRightMaster (mastered 0.01 0.4 $ (tabbedAlways shrinkText myTabTheme)))))
--                                      (lessBorders (OnlyFloat) (noBorders (mastered 0.01 0.4 $ tabbedAlways shrinkText myTabTheme))))
--
--    -- <layouts per workspace>
--    favorLayout = myUniq
--    inetLayouts = myOneB ||| myFull
--    fotoLayouts = myFull ||| myTile

-- end of LAYOUTS }}}


-- COLORS, FONTS, AND PROMPTS {{{

-- <colors>
-- colorBlack          = "#000000"
-- colorBlackAlt       = "#040404"
-- colorGray           = "#444444"
-- colorGrayAlt        = "#282828"
-- colorDarkGray       = "#161616"
-- colorWhite          = "#cfbfad"
-- colorWhiteAlt       = "#8c8b8e"
-- colorDarkWhite      = "#606060"
-- colorCream          = "#a9a6af"
-- colorDarkCream      = "#5f656b"
-- colorMagenta        = "#a488d9"
-- colorMagentaAlt     = "#7965ac"
-- colorDarkMagenta    = "#8e82a2"
-- colorBlue           = "#98a7b6"
-- colorBlueAlt        = "#598691"
-- colorDarkBlue       = "#464a4a"
-- colorNormalBorder   = colorDarkWhite
-- colorFocusedBorder  = colorMagenta

------------------------------------------------------------------------
-- Colors and borders
-- Currently based on the ir_black theme.
--
myNormalBorderColor  = "#7c7c7c"
myFocusedBorderColor = "#ffb6b0"


------------------------------------------------------------------------
-- Theme Configuration for Tabbed
-- Colors for text and backgrounds of each tab when in "Tabbed" layout.
--
tabConfig :: Theme
tabConfig = defaultTheme { activeBorderColor = myNormalBorderColor
                         , activeTextColor = "#CEFFAC"
                         , activeColor = "#000000"
                         , inactiveBorderColor = "#7C7C7C"
                         , inactiveTextColor = "#EEEEEE"
                         , inactiveColor = "#000000"
                         , fontName = myFont ++ ":size=9"
                         , decoHeight = 17
                         }

-- Color of current window title in xmobar.
xmobarTitleColor = "#FFB6B0"

-- Color of current workspace in xmobar.
xmobarCurrentWorkspaceColor = "#CEFFAC"

-- Width of the window border in pixels.
myBorderWidth = 1


------------------------------------------------------------------------
-- Configuration for Prompt
--
myXPConfig :: XPConfig
myXPConfig = defaultXPConfig { font               = myFont ++ ":size=11"
                              , bgColor           = "orange"
                              , fgColor           = "black"
                              , fgHLight          = "black"
                              , bgHLight          = "darkslategray4"
                              , borderColor       = "black"
                              , promptBorderWidth = 1
                              , position          = Bottom
                              , height            = 20
                              , defaultText       = []
                              }

-- end of COLORS, FONTS, AND PROMPTS }}}


-- KEY-BINDINGS {{{

------------------------------------------------------------------------
-- Key bindings
--
-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask :: KeyMask
myModMask = mod4Mask

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
  ----------------------------------------------------------------------
  -- Custom key bindings
  --

  -- Start a terminal.  Terminal to start is specified by myTerminal variable.
  [ ((modMask .|. shiftMask, xK_Return),
     spawn $ XMonad.terminal conf)

  -- Start Man page prompt.
  , ((modMask, xK_F1),
     manPrompt defaultXPConfig)

  -- Start Search Engine prompt.
  , ((modMask, xK_s),
     SM.submap $ searchEngineMap $ S.promptSearch defaultXPConfig)

  -- Start Email prompt.
  , ((modMask .|. controlMask, xK_e),
     emailPrompt defaultXPConfig addresses)
  --
  -- Start Append file prompt for Note Taking.
  , ((modMask .|. controlMask, xK_n), do
     spawn ("date>>" ++ myHome ++ "/NOTES")
     appendFilePrompt defaultXPConfig (myHome ++ "/NOTES"))

  -- Lock the screen using xscreensaver.
  , ((modMask .|. controlMask, xK_l),
     spawn "xscreensaver-command -lock")

  -- Launch dmenu.
  -- Use this to launch programs without a key binding.
  , ((modMask, xK_p),
     shellPrompt myXPConfig)

  -- Take a screenshot in select mode.
  -- After pressing this key binding, click a window, or draw a rectangle with
  -- the mouse.
  , ((modMask .|. shiftMask, xK_p),
     spawn "select-screenshot")

  -- Take full screenshot in multi-head mode.
  -- That is, take a screenshot of everything you see.
  , ((modMask .|. controlMask .|. shiftMask, xK_p),
     spawn "screenshot")

  --------------------------------------------------------------------
  -- Multimedia keys
  --

  -- Mute volume.
  , ((modMask .|. controlMask, xK_m),
     spawn "amixer -q set Master toggle")

  -- Decrease volume.
  , ((modMask .|. controlMask, xK_j),
     spawn "amixer -q set Master 10%-")

  -- Increase volume.
  , ((modMask .|. controlMask, xK_k),
     spawn "amixer -q set Master 10%+")

  -- Audio previous.
  , ((0, 0x1008FF16),
     spawn "")

  -- Play/pause.
  , ((0, 0x1008FF14),
     spawn "")

  -- Audio next.
  , ((0, 0x1008FF17),
     spawn "")

  -- Eject CD tray.
  , ((0, 0x1008FF2C),
     spawn "eject -T")

  --------------------------------------------------------------------
  -- Custom application key bindings
  --

  -- Start web browser.
  , ((modMask .|. controlMask, xK_w),
     spawn myBrowser)

  -- Start email client.
  , ((modMask .|. controlMask, xK_z),
     spawn myMail)

  -- Start rss news feed client.
  , ((modMask .|. controlMask, xK_r),
     spawn myNews)

  -- Start torrent client.
  , ((modMask .|. controlMask, xK_t),
     spawn myTorrent)

  -- Start media library.
  , ((modMask .|. controlMask, xK_m),
     spawn myMedia)

  -- Start chat client.
  , ((modMask .|. controlMask, xK_i),
     spawn myChat)

  -- Start text editor.
  , ((modMask .|. controlMask, xK_e),
     AL.launchApp defaultXPConfig myEditor)

  -- Start PDF reader.
  , ((modMask .|. controlMask, xK_d),
     AL.launchApp defaultXPConfig myPDFReader)

  -- Start Gtk File Manager.
  , ((modMask .|. controlMask, xK_f),
     AL.launchApp defaultXPConfig myFileManager)

  --------------------------------------------------------------------
  -- "Standard" xmonad key bindings
  --

  --------------------------------------------------------------------
  -- Focus
  --

  -- Close focused window.
  , ((modMask .|. shiftMask, xK_c),
     kill)

  -- Move to urgent window.
  , ((modMask, xK_BackSpace),
     focusUrgent)

  -- Move focus to the Left Workspace.
  , ((modMask, xK_Left),
     prevNonEmptyWS)

  -- Move focus to the Right Workspace.
  , ((modMask, xK_Right),
     nextNonEmptyWS)

  -- Move focus to the Next Screen.
  , ((modMask, xK_Down),
     nextScreen)

  -- Move focus to the next window.
  , ((modMask, xK_Tab),
     windows W.focusDown)

  -- Move focus to the next window.
  , ((modMask, xK_j),
     windows W.focusDown)

  -- Move focus to the previous window.
  , ((modMask, xK_k),
     windows W.focusUp  )

  -- Move focus to the master window.
  , ((modMask, xK_m),
     windows W.focusMaster  )

  -- Swap the focused window and the master window.
  , ((modMask, xK_Return),
     windows W.swapMaster)

  -- Swap the focused window with the next window.
  , ((modMask .|. shiftMask, xK_j),
     windows W.swapDown  )

  -- Swap the focused window with the previous window.
  , ((modMask .|. shiftMask, xK_k),
     windows W.swapUp    )

  -- Push window back into tiling.
  , ((modMask, xK_t),
     withFocused $ windows . W.sink)

  --------------------------------------------------------------------
  -- Resize and Shift
  --

  -- Clear urgent window status.
  , ((modMask .|. shiftMask, xK_BackSpace),
     clearUrgents)

  -- Cycle through the available layout algorithms.
  , ((modMask, xK_space),
     sendMessage NextLayout)

  --  Reset the layouts on the current workspace to default.
  , ((modMask .|. shiftMask, xK_space),
     setLayout $ XMonad.layoutHook conf)

  -- Resize viewed windows to the correct size.
  , ((modMask, xK_n),
     refresh)

  -- Shift to the Left Workspace.
  , ((modMask .|. shiftMask, xK_Left),
     shiftToPrev >> prevWS)

  -- Shift to the Right Workspace.
  , ((modMask .|. shiftMask, xK_Right),
     shiftToNext >> nextWS)

  -- Shift to the Next Screen.
  , ((modMask .|. shiftMask, xK_Down),
     shiftNextScreen >> nextScreen)

  -- Modify Magnifier layout.
  , ((modMask, xK_w),
     sendMessage MagnifyMore)

  , ((modMask, xK_e),
     sendMessage MagnifyLess)

  -- Shrink the master area.
  , ((modMask, xK_h),
     sendMessage Shrink)

  -- Expand the master area.
  , ((modMask, xK_l),
     sendMessage Expand)

  -- Increment the number of windows in the master area.
  , ((modMask, xK_comma),
     sendMessage (IncMasterN 1))

  -- Decrement the number of windows in the master area.
  , ((modMask, xK_period),
     sendMessage (IncMasterN (-1)))

  -- Toggle the status bar gap.
  -- TODO: update this binding with avoidStruts, ((modMask, xK_b),

  --------------------------------------------------------------------
  -- Quit, or Restart
  --

  -- Quit xmonad.
  , ((modMask .|. shiftMask, xK_q),
     io (exitWith ExitSuccess))

  -- Restart xmonad.
  , ((modMask, xK_q),
     restart "xmonad" True)
  ]
  ++

  -- mod-[1..9], Switch to workspace N
  -- mod-shift-[1..9], Move client to workspace N
  [ ((m .|. modMask, k), windows $ f i)
      | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
      , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
  ]
  ++

  -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
  -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
  [ ((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
      | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
      , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
  ]

  where
      nextNonEmptyWS = moveTo Next (WSIs (liftM (not .) isVisible))
      prevNonEmptyWS = moveTo Prev (WSIs (liftM (not .) isVisible))
      -- TODO: pull in email address book
      addresses = ["example@gmail.com"]

isVisible :: X (WindowSpace -> Bool)
isVisible = do
  vs <- gets (map (W.tag . W.workspace) . W.visible . windowset)
  return (\w -> (W.tag w) `elem` vs)

-- Search Engine list
searchEngineMap method = M.fromList $
      [ ((0, xK_a), method S.alpha)
      , ((0, xK_g), method S.google)
      , ((0, xK_h), method S.hoogle)
      , ((0, xK_s), method S.scholar)
      , ((0, xK_w), method S.wikipedia)
      , ((0, xK_y), method S.youtube)
      , ((0, xK_z), method S.amazon)
      ]


------------------------------------------------------------------------
-- Mouse bindings
--
-- Focus rules
-- True if your focus should follow your mouse cursor.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
  [
    -- mod-button1, Set the window to floating mode and move by dragging
    ((modMask, button1),
     (\w -> focus w >> mouseMoveWindow w))

    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2),
       (\w -> focus w >> windows W.swapMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3),
       (\w -> focus w >> mouseResizeWindow w))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    -- cycle focus
    , ((modMask, button4), (\_ -> windows W.focusUp))
    , ((modMask, button5), (\_ -> windows W.focusDown))
    -- cycle through workspaces
    , ((controlMask .|. modMask, button5), nextNonEmptyWS)
    , ((controlMask .|. modMask, button4), prevNonEmptyWS)
  ]
  where
    nextNonEmptyWS = \_ -> moveTo Next (WSIs (liftM (not .) isVisible))
    prevNonEmptyWS = \_ -> moveTo Prev (WSIs (liftM (not .) isVisible))


-- end of KEY-BINDINGS }}}


-- MAIN CONFIGURATION {{{

------------------------------------------------------------------------
-- Startup hook
-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- Start XMonad as "LG3D" so Java works with "non-repainting" WM :/
myStartupHook = ewmhDesktopsStartup >> setWMName "LG3D"

------------------------------------------------------------------------
-- Run xmonad with all the defaults we set up.
--
main = do
  --xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmonad/xmobarrc"
  xmproc <- spawnPipe "/usr/bin/xmobar"
  xmonad $ withUrgencyHook NoUrgencyHook $ defaults xmproc


------------------------------------------------------------------------
-- Combine it all together
-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
defaults pipe = defaultConfig {
    -- simple stuff
    terminal           = myTerminal,
    focusFollowsMouse  = myFocusFollowsMouse,
    borderWidth        = myBorderWidth,
    modMask            = myModMask,
    workspaces         = myWorkspaces,
    normalBorderColor  = myNormalBorderColor,
    focusedBorderColor = myFocusedBorderColor,

    -- key bindings
    keys               = myKeys,
    mouseBindings      = myMouseBindings,

    -- hooks, layouts
    layoutHook         = smartBorders $ myLayout,
    manageHook         = manageDocks <+> myManageHook,
    logHook            = myLogHook pipe,
    startupHook        = myStartupHook
--    handleEventHook    = fullscreenEventHook
}

-- end of MAIN-CONFIGURATION }}}

-- vim:foldmethod=marker foldmarker={{{,}}} sw=2 sts=2 ts=2 tw=0 et ai nowrap

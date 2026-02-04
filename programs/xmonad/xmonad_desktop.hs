
import Data.Maybe (fromJust)
import System.IO
import System.Exit
import XMonad
import XMonad.Config
import qualified XMonad.StackSet as W
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.StatusBar
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.BinarySpacePartition
import XMonad.Util.Run(spawnPipe,runProcessWithInput,safeSpawn)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Themes
import XMonad.Util.Spotify
import XMonad.Util.Loggers
import Graphics.X11.ExtraTypes.XF86
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import qualified DBus            as D
import qualified DBus.Client     as D
import XMonad.Actions.SpawnOn
import qualified Codec.Binary.UTF8.String as UTF8
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.Shell
import XMonad.Prompt.Theme

import XMonad.Actions.CycleWS
import XMonad.Layout.IndependentScreens
import XMonad.Util.NamedScratchpad

------------------------------------------------------------------------

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
-- myTerminal = "/run/current-system/sw/bin/konsole"
myTerminal = "kitty"

-- Suspend the system
mySuspend = "systemctl suspend"

-- The command to take a selective screenshot, where you select
-- what you'd like to capture on the screen.
mySelectScreenshot = "select-screenshot"

-- The command to take a fullscreen screenshot.
myScreenshot = "/usr/bin/deepin-screenshot"

-- The command to use as a launcher, to launch commands that don't have
-- preset keybindings.
myLauncher = "rofi -show run"

myMusic = "spotify"

-- Use dolphin for files
myFS = "caja --no-desktop $HOME"

-- Use firefox for browser
myBrowser = "firefox"

-- Use emacs as server
myIDE = "emacsclient -create-frame"

-- Email client
myEmail = "thunderbird"

myWallPapers = "/home/doyougnu/sync/wallpapers"

------------------------------------------------------------------------
-- Workspaces
-- The default number of workspaces (virtual screens) and their names.
--
myWorkspaces = ["ƛ","\xf269","\xf025","\xf27b","\xf014"]

-- f88݉5
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

myManageHook = manageDocks <+> composeAll (concat $
     [ [ className   =? c --> doIgnore                | c <- myIgnore]
     , [ title       =? t --> doFloat                 | t <- myOtherFloats]
     , [ className   =? c --> doF (W.shift "2:Web")   | c <- webApps]
     , [ className   =? c --> doF (W.shift "3:Music") | c <- musicApps]
     , [ className   =? c --> doF (W.shift "4:Comms") | c <- ircApps]
     ])
  where
    myFloats      = []
    myOtherFloats = []
    myIgnore      = [] -- ["plasmashell", "plasmashell"]
    webApps       = ["Navigator", "Firefox"] -- open on desktop 2
    musicApps     = ["Spotify"]              -- open on desktop 3
    ircApps       = ["slack", "Slack"]       -- open on desktop 4

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
-- myLayout = avoidStruts (
--     tabbed shrinkText myTheme |||
--     ThreeColMid 1 (3/100) (1/2) |||
--     Tall 1 (3/100) (1/2) |||
--     Mirror (Tall 1 (3/100) (1/2)) |||
--     Full |||
--     spiral (6/7)) |||
--     noBorders (fullscreenFull Full))

-- myLayout = avoidStruts $
--     emptyBSP |||
--     tabbed shrinkText (theme robertTheme) |||
--     spiral (6/7) |||
--     Tall 1 (3/100) (1/2) |||
--     Mirror (Tall 1 (3/100) (1/2)) |||
--     Full |||
--     noBorders (fullscreenFull Full)

myLayout = fullscreenFull $ avoidStruts $ smartBorders ( tiled ||| Mirror tiled ||| Full )
  where
    tiled = Tall nmaster delta ratio
    nmaster = 1
    ratio = 3/5
    delta = 3/100

------------------------------------------------------------------------
-- Colors and borders
-- Currently based on the ir_black theme.

-- myNormalBorderColor  = "#7c7c7c"
-- myFocusedBorderColor = "#ffb6b0"
myNormalBorderColor  = ""
myFocusedBorderColor = ""

-- Colors for text and backgrounds of each tab when in "Tabbed" layout.
-- myTheme = defaultTheme { activeBorderColor   = "#7C7C7C"
--                        , activeTextColor     = "#CEFFAC"
--                        , activeColor         = "#000000"
--                        , inactiveBorderColor = "#7C7C7C"
--                        , inactiveTextColor   = "#EEEEEE"
--                        , inactiveColor       = "#000000"
--                        }

-- Color of current window title in xmobar.
-- xmobarTitleColor = "#FFB6B0"

-- Color of current workspace in xmobar.
-- xmobarCurrentWorkspaceColor = "#CEFFAC"

-- Width of the window border in pixels.
myBorderWidth = 1

------------------------------------------------------------------------
-- Key bindings
--
-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask = mod4Mask

myNumRow = [ xK_b
           , xK_y
           , xK_o
           , xK_u
           ]

myKeys conf@XConfig {XMonad.modMask = modMask} = M.fromList $
  ----------------------------------------------------------------------
  -- Custom key bindings
  --

  -- Start a terminal.
  [ ((modMask, xK_a),
     spawn $ XMonad.terminal conf)

  -- spawn an eshell
  , ((modMask .|. controlMask, xK_Return),
     safeSpawn "emacsclient" ["-c", "-e", "(eshell)"])

  -- Lock the screen using command specified by myScreensaver.
  , ((modMask .|. controlMask, xK_l),
     spawn mySuspend)

  -- Spawn the launcher using command specified by myLauncher.
  -- Use this to launch programs without a key binding.
  , ((modMask, xK_period),
     spawn myLauncher)

  -- On Mod d open the browser
  , ((modMask, xK_e),
     spawn myBrowser)

  -- On Mod g open the browser
  , ((modMask, xK_comma),
     spawn "google-chrome-stable")

  -- On Mod s open emacs
  , ((modMask, xK_i),
     spawn myIDE)

  -- On Mod f open the browser
  , ((modMask .|. controlMask, xK_o),
     spawn myEmail)

  -- On Mod d open the file bro
  -- , ((modMask, xK_u),
  --    spawn myFS)

  -- change themes with mod + ctrl + t
  , ((modMask .|. controlMask, xK_t), themePrompt def)

  -- lookup stuff in a dictionary easily and pipe to notification
  , ((modMask, xK_r), lookupPrompt)

  -- close last notification
  , ((modMask .|. mod1Mask, xK_w),
    safeSpawn "dunstctl" ["close"])

  -- Take a selective screenshot using the command specified by mySelectScreenshot.
  -- , ((modMask .|. shiftMask, xK_p),
  --    spawn mySelectScreenshot)

  -- Take a full screenshot using the command specified by myScreenshot.
  -- , ((modMask .|. controlMask .|. shiftMask, xK_p),
  --    spawn myScreenshot)

  -- Mute volume.
  , ((0, xF86XK_AudioMute),
     spawn "wpctl set-mute @DEFAULT_SINK@ toggle")
  --
  -- Decrease volume.
  , ((0, xF86XK_AudioLowerVolume),
     spawn "wpctl set-volume @DEFAULT_SINK@ .1-")

  -- Increase volume.
  , ((0, xF86XK_AudioRaiseVolume),
     spawn "wpctl set-volume @DEFAULT_SINK@ .1+")
  --
  -- -- Mute volume.
  , ((modMask .|. controlMask, xK_t),
     spawn "wpctl set-mute @DEFAULT_SINK@ toggle")

  -- -- Decrease volume.
  , ((modMask .|. controlMask, xK_h),
     spawn "wpctl set-volume @DEFAULT_SINK@ .1-")

  -- -- Increase volume.
  , ((modMask .|. controlMask, xK_s),
     spawn "wpctl set-volume @DEFAULT_SINK@ .1+")


  , ((modMask .|. controlMask, xK_m), audioPlayPause)

  -- spotify previous
  , ((modMask .|. controlMask, xK_r), audioPrev)

  -- spotify next
  , ((modMask .|. controlMask, xK_f), audioNext)

  --  Toggle the bottom xmobar
  , ((modMask .|. controlMask, xK_space),
     sendMessage $ ToggleStrut D)

  --------------------------------------------------------------------
  -- "Standard" xmonad key bindings
  --

  -- Close focused window.
  , ((modMask, xK_j),
     kill)

  -- Cycle through the available layout algorithms.
  , ((modMask, xK_space),
     sendMessage NextLayout)

  --  Reset the layouts on the current workspace to default.
  , ((modMask .|. mod1Mask, xK_space),
     setLayout $ XMonad.layoutHook conf)

  -- Resize viewed windows to the correct size.
  -- , ((modMask, xK_h),
  --    refresh)

  -- Move focus to the next window.
  , ((modMask, xK_Tab),
     windows W.focusDown)

  -- Move focus to the next window.
  , ((modMask, xK_s),
     windows W.focusDown)

  -- Move focus to the previous window.
  , ((modMask, xK_t),
     windows W.focusUp)

  -- Move focus to the master window.
  , ((modMask, xK_w),
     windows W.focusMaster  )

  -- Swap the focused window and the master window.
  , ((modMask, xK_Return),
     windows W.swapMaster)

  -- Swap the focused window with the next window.
  , ((modMask .|. shiftMask, xK_s),
     windows W.swapDown  )

  -- Swap the focused window with the previous window.
  , ((modMask .|. shiftMask, xK_t),
     windows W.swapUp    )

  -- Shrink the master area.
  , ((modMask, xK_h),
     sendMessage Shrink)

  -- Expand the master area.
  , ((modMask, xK_n),
     sendMessage Expand)

  -- Push window back into tiling.
  , ((modMask, xK_y),
     withFocused $ windows . W.sink)

  -- Increment the number of windows in the master area.
  -- , ((modMask, xK_comma),
  --    sendMessage (IncMasterN 1))

  -- Decrement the number of windows in the master area.
  -- , ((modMask, xK_period),
  --    sendMessage (IncMasterN (-1)))

  -- Toggle the status bar gap.
  -- TODO: update this binding with avoidStruts, ((modMask, xK_b),

  -- Quit xmonad.
  , ((modMask .|. shiftMask, xK_apostrophe),
     io exitSuccess)

  -- Restart xmonad.
  , ((modMask, xK_g),
     restart "xmonad" True)
  ]
  ++

  -- mod-[1..9], Switch to workspace N
  -- mod-ctrl-[1..9], Move client to workspace N
  [((m .|. modMask, k), windows $ f i)
      | (i, k) <- zip (XMonad.workspaces conf) myNumRow
      , (f, m) <- [(W.greedyView, 0), (W.shift, controlMask)]]
  ++
  -- mod-{w,e,r} %! Switch to physical/Xinerama screens 1, 2, or 3
  -- mod-shift-{w,e,r} %! Move client to screen 1, 2, or 3
  [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
      | (key, sc) <- zip [xK_w, xK_d] [0..]      , (f, m) <- [(W.view, 0), (W.shift, controlMask)]]

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
     \w -> focus w >> mouseMoveWindow w)

    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2),
       \w -> focus w >> windows W.swapMaster)

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3),
       \w -> focus w >> mouseResizeWindow w)

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
  ]


------------------------------------------------------------------------
-- Status bars and logging
-- Perform an arbitrary action on each internal state change or X event.
-- See the 'DynamicLog' extension for examples.
--

-- -x 2 says display this on monitor number 2
mySBTop = statusBarProp "xmobar -x 2 /home/doyougnu/.config/xmobar/xmobar.hs" (pure myXmobarPP)
mySBBtm = statusBarProp "xmobar -x 2 /home/doyougnu/.config/xmobar/xmobar_bottom_desktop.hs" (pure myXmobarPP)

myXmobarPP :: PP
myXmobarPP = def
    { ppSep             = magenta " • "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2
    , ppHidden          = white . wrap " " ""
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppOrder           = \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras          = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused   = wrap (white    "[") (white    "]") . magenta . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue    . ppWindow

    -- | Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 15

    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta  = xmobarColor "#ff79c6" ""
    blue     = xmobarColor "#bd93f9" ""
    white    = xmobarColor "#f8f8f2" ""
    yellow   = xmobarColor "#f1fa8c" ""
    red      = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""

------------------------------------------------------------------------
-- Startup hook
-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = do
  setWMName "LG3D"
  safeSpawn "feh" ["--randomize", "--bg-scale", myWallPapers, "--bg-center", myWallPapers]
  return ()


------------------------------------------------------------------------
-- custom stuff
lookupInDict :: String -> X ()
lookupInDict arg = do
  res <- runProcessWithInput "sdcv" [] arg
  safeSpawn "dunstify" ["-t", "5000000", res] -- set an extremely long timeout,
                                              -- then use `dunstctl close` to
                                              -- close

lookupPrompt :: X ()
lookupPrompt = inputPrompt greenXPConfig "λ" ?+ lookupInDict

------------------------------------------------------------------------
------------------------------------------------------------------------
-- Run xmonad with all the defaults we set up.
--
-- ON NIXOS, do this to recompile: nix-shell -p 'xmonad-with-packages.override { packages = p: with p; [ xmonad-extras xmonad-contrib xmonad dbus xmonad-spotify ]; }'

main = do

  xmonad
    $ fullscreenSupport
    $ withSB mySBTop
    $ withSB mySBBtm
    $ ewmh
    $ docks
    $ defaults


------------------------------------------------------------------------
-- Combine it all together
-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
defaults = def {
    -- simple stuff
   terminal           = myTerminal,
   focusFollowsMouse  = myFocusFollowsMouse,
   modMask            = myModMask,
   workspaces         = myWorkspaces,
   normalBorderColor  = myNormalBorderColor,
   focusedBorderColor = myFocusedBorderColor,

   -- key bindings
   keys               = myKeys,
   mouseBindings      = myMouseBindings,

   -- hooks, layouts
   layoutHook         = noBorders myLayout,
   manageHook         = manageDocks <+> myManageHook,
   startupHook        = myStartupHook
}

-- xmonad config used by Vic Fryzel
-- Author: Vic Fryzel
-- http://github.com/vicfryzel/xmonad-config

import System.IO
import System.Exit
import System.Environment
import System.FilePath.Posix
import XMonad
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.BinarySpacePartition
import XMonad.Util.Run(spawnPipe,runProcessWithInput,safeSpawn,unsafeSpawn)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Themes
import XMonad.Util.Spotify
import Graphics.X11.ExtraTypes.XF86
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import qualified DBus            as D
import qualified DBus.Client     as D
import XMonad.Actions.SpawnOn
import qualified Codec.Binary.UTF8.String as UTF8
import qualified XMonad.Util.Brightness as Bright
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.Pass -- TODO
import XMonad.Prompt.Shell
import XMonad.Prompt.Theme


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
myLauncher = "dmenu_run"

myMusic = "spotify"

-- Use dolphin for files
myFS = "caja --no-desktop $HOME"

-- Use firefox for browser
myBrowser = "firefox"

-- Use emacs as server
myIDE = "emacsclient --create-frame"

myIDESameBuf = "emacsclient --create-frame --tty"

myWallPapers = "/home/doyougnu/sync/wallpapers"

------------------------------------------------------------------------
-- Workspaces
-- The default number of workspaces (virtual screens) and their names.
--
myWorkspaces = ["1:Space","2:Web","3:Music","4:Comms","5:Dump"] ++ map show [6..9]


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
-- myManageHook = composeAll
--   [ className =? "Gimp"           --> doFloat
--    , resource  =? "gpicview"       --> doFloat
--    , className =? "Desktop — Plasma" --> doIgnore
--    , className =? "Desktop"          --> doIgnore
--    , className =? "MPlayer"        --> doFloat
--    , className =? "Slack"          --> doShift "4:Comms"
--    , className =? "spotify"        --> doShift "3:Music"
--    , isFullscreen --> (doF W.focusDown <+> doFullFloat)]

myManageHook = manageDocks <+> composeAll (concat $
     [ [ className   =? c --> doFloat           | c <- myIgnore]
     , [ title       =? t --> doFloat           | t <- myOtherFloats]
     , [ className   =? c --> doF (W.shift "2:Web")   | c <- webApps]
     , [ className   =? c --> doF (W.shift "3:Music") | c <- musicApps]
     , [ className   =? c --> doF (W.shift "4:Comms") | c <- ircApps]
     ])
  where
    myFloats      = []
    myOtherFloats = []
    myIgnore      = ["plasmashell", "plasmashell"]
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
--     noBorders (fullscreenFull Full)

myLayout = avoidStruts $
    emptyBSP |||
    tabbed shrinkText (theme robertTheme) |||
    spiral (6/7) |||
    Tall 1 (3/100) (1/2) |||
    Mirror (Tall 1 (3/100) (1/2)) |||
    Full |||
    noBorders (fullscreenFull Full)


------------------------------------------------------------------------
-- Colors and borders
-- Currently based on the ir_black theme.
--
myNormalBorderColor  = "#7c7c7c"
myFocusedBorderColor = "#ffb6b0"

-- Colors for text and backgrounds of each tab when in "Tabbed" layout.
-- myTheme = defaultTheme {
--     activeBorderColor = "#7C7C7C",
--     activeTextColor = "#CEFFAC",
--     activeColor = "#000000",
--     inactiveBorderColor = "#7C7C7C",
--     inactiveTextColor = "#EEEEEE",
--     inactiveColor = "#000000"
-- }

-- Color of current window title in xmobar.
-- xmobarTitleColor = "#FFB6B0"

-- Color of current workspace in xmobar.
-- xmobarCurrentWorkspaceColor = "#CEFFAC"

-- Width of the window border in pixels.
myBorderWidth = 2

------------------------------------------------------------------------
-- password stuff
-- data OnePass = OnePass

-- instance XPrompt OnePass where
--   showXPrompt       OnePass = "1Password: "
--   commandToComplete _ c  = c
--   nextCompletion      _  = getNextCompletion

-- passPrompt :: XPConfig -> X ()
-- passPrompt c = do
--   li <- io getPasswords
--   mkXPrompt Pass c (mkComplFunFromList li) selectPassword

-- selectPassword :: String -> X ()
-- selectPassword s = spawn $ "pass -c " ++ s

-- getPasswords :: IO [String]
-- getPasswords = do
--   user <- getEnv "USER"
--   entries <- getDirectoryContents $
--     "/home/" ++ user ++ "/.password-store"
--   return $ fmap takeBaseName entries

-- promptConfig = defaultXPConfig
--   { font        = "xft:Source Code Pro:pixelsize=12"
--   , borderColor = "#1e2320"
--   , fgColor     = "#dddddd"
--   , fgHLight    = "#ffffff"
--   , bgColor     = "#1e2320"
--   , bgHLight    = "#5f5f5f"
--   , height      = 18
--   , position    = Top
--   }
------------------------------------------------------------------------

------------------------------------------------------------------------
-- Key bindings
--
-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask = mod1Mask

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
  ----------------------------------------------------------------------
  -- Custom key bindings
  --

  -- Start a terminal.  Terminal to start is specified by myTerminal variable.
  [ ((modMask, xK_u),
     spawn $ XMonad.terminal conf)
    --
  , ((modMask .|. controlMask, xK_Return),
     safeSpawn "emacsclient" ["-c", "-e", "(eshell)"]) --

  -- Lock the screen using command specified by myScreensaver.
  , ((modMask .|. controlMask, xK_l), --
     spawn mySuspend)

  -- Spawn the launcher using command specified by myLauncher.
  -- Use this to launch programs without a key binding.
  , ((modMask, xK_g),
     spawn myLauncher)

  -- On Mod f open the browser
  , ((modMask, xK_e),
     spawn myBrowser)

  , ((modMask, xK_i),
     safeSpawn "google-chrome-stable" mempty)

  -- run emacs everywhere
  , ((modMask, xK_a),
    safeSpawn "emacsclient" ["--eval", "(emacs-everywhere)"])

  -- On Mod f open the browser
  , ((modMask, xK_o),
     spawn myIDE)

  -- On Mod f open the browser
  , ((modMask .|. shiftMask, xK_o),
     spawn myIDESameBuf)

  -- On Mod d open the file bro
  -- , ((modMask, xK_u),
  --    spawn myFS)

  -- change themes with mod + ctrl + t
  , ((modMask .|. controlMask, xK_t), themePrompt def)

  -- lookup stuff in a dictionary easily and pipe to notification
  , ((modMask, xK_r),
     lookupPrompt)

  , ((modMask .|. shiftMask, xK_r)
    , safeSpawn "dunstctl" ["close"])


  -- Mute volume.
  , ((0, xF86XK_AudioMute),
     spawn "amixer -q set Master toggle")

  -- Decrease volume.
  , ((0, xF86XK_AudioLowerVolume),
     spawn "amixer -q set Master 5%-")

  -- Increase volume.
  , ((0, xF86XK_AudioRaiseVolume),
     spawn "amixer -q set Master 5%+")

  -- Decrease brightness.
  , ((0, xF86XK_MonBrightnessDown),
     spawn "light -U 5")

  -- Increase brightness.
  , ((0, xF86XK_MonBrightnessUp),
     spawn "light -A 5")

  -- -- Mute volume.
  , ((modMask .|. controlMask, xK_w),
     spawn "amixer -q set Master toggle")

  -- -- Decrease volume.
  , ((modMask .|. controlMask, xK_n),
     spawn "amixer -q set Master 5%-")

  -- -- Increase volume.
  , ((modMask .|. controlMask, xK_t),
     spawn "amixer -q set Master 5%+")

  -- Pause any spotify music
  , ((modMask .|. controlMask, xK_v), audioPlayPause)

  -- Previous
  , ((modMask .|. controlMask, xK_w), audioPrev)

  -- Next
  , ((modMask .|. controlMask, xK_z), audioNext)

  -- Decrease brightness.
  , ((modMask .|. controlMask, xK_c),
     spawn "light -U 2")

  -- -- Increase volume.
  , ((modMask .|. controlMask, xK_r),
     spawn "light -A 2")

  -- disable touchpad
  , ((modMask, xK_b),
     safeSpawn "xinput" ["--disable", "12"])

  -- enable touchpad
  , ((modMask .|. controlMask, xK_b),
     safeSpawn "xinput" ["--enable", "12"])
  --------------------------------------------------------------------
  -- "Standard" xmonad key bindings
  --

  -- Close focused window.
  , ((modMask, xK_j), kill)

  -- Cycle through the available layout algorithms.
  , ((modMask, xK_space),
     sendMessage NextLayout)

  --  Reset the layouts on the current workspace to default.
  , ((modMask .|. shiftMask, xK_space),
     setLayout $ XMonad.layoutHook conf)

  -- Resize viewed windows to the correct size.
  , ((modMask, xK_n),
     refresh)

  -- Move focus to the next window.
  , ((modMask, xK_Tab),
     windows W.focusDown)

  -- Move focus to the next window.
  , ((modMask, xK_n),
     windows W.focusDown)

  -- Move focus to the previous window.
  , ((modMask, xK_t),
     windows W.focusUp  )

  -- Move focus to the master window.
  , ((modMask, xK_m),
     windows W.focusMaster  )

  -- Swap the focused window and the master window.
  , ((modMask, xK_Return),
     windows W.swapMaster)

  -- Swap the focused window with the next window.
  , ((modMask .|. shiftMask, xK_n),
     windows W.swapDown  )

  -- Swap the focused window with the previous window.
  , ((modMask .|. shiftMask, xK_t),
     windows W.swapUp    )

  -- Shrink the master area.
  , ((modMask, xK_h),
     sendMessage Shrink)

  -- Expand the master area.
  , ((modMask, xK_s),
     sendMessage Expand)

  -- Push window back into tiling.
  , ((modMask, xK_y),
     withFocused $ windows . W.sink)

  -- Increment the number of windows in the master area.
  -- , ((modMask, xK_comma),
  --    sendMessage (IncMasterN 1))

  -- -- Decrement the number of windows in the master area.
  -- , ((modMask, xK_period),
  --    sendMessage (IncMasterN (-1)))

  -- Toggle the status bar gap.
  -- TODO: update this binding with avoidStruts, ((modMask, xK_b),

  -- Quit xmonad.
  , ((modMask .|. shiftMask, xK_apostrophe),
     io (exitWith ExitSuccess))

  -- Restart xmonad.
  , ((modMask, xK_apostrophe),
     restart "xmonad" True)
  ]
  ++

  -- mod-[1..9], Switch to workspace N
  -- mod-shift-[1..9], Move client to workspace N
  [((m .|. modMask, k), windows $ f i)
      | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
      , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
  -- ++

  -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
  -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
  -- [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
  --     | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
  --     , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


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
  ]


------------------------------------------------------------------------
-- Status bars and logging
-- Perform an arbitrary action on each internal state change or X event.
-- See the 'DynamicLog' extension for examples.
--
-- To emulate dwm's status bar
--
-- > logHook = dynamicLogDzen
--


------------------------------------------------------------------------
-- Startup hook
-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
myStartupHook = do
  setWMName "LG3D"
  safeSpawn "feh" ["--randomize", "--bg-scale", myWallPapers]

------------------------------------------------------------------------
-- custom stuff
lookupInDict :: String -> X ()
lookupInDict arg = do
  res <- runProcessWithInput "sdcv" [] arg
  safeSpawn "notify-send" ["-t", "500000000", res]

lookupPrompt :: X ()
lookupPrompt = inputPrompt greenXPConfig "λ" ?+ lookupInDict

------------------------------------------------------------------------
------------------------------------------------------------------------
-- Run xmonad with all the defaults we set up.
--
-- ON NIXOS, do this to recompile: nix-shell -p 'xmonad-with-packages.override { packages = p: with p; [ xmonad-extras xmonad-contrib xmonad dbus xmonad-spotify]; }'
main = do
  polybar <- spawnPipe "$HOME/.config/polybar/launch.sh"
  dbus <- D.connectSession
  D.requestName dbus (D.busName_ "org.xmonad.Log")
    [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]

  xmonad
    . ewmh
    . docks
    $ defaults
    { logHook = dynamicLogWithPP $ myLogHook dbus
    , manageHook = manageDocks <+> myManageHook
    }

-- Override the PP values as you would otherwise, adding colors etc depending
-- on  the statusbar used
myLogHook :: D.Client -> PP
myLogHook dbus = def { ppOutput = dbusOutput dbus }

-- Emit a DBus signal on log updates
dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
    let signal = (D.signal objectPath interfaceName memberName) {
            D.signalBody = [D.toVariant $ UTF8.decodeString str]
        }
    D.emit dbus signal
  where
    objectPath    = D.objectPath_ "/org/xmonad/Log"
    interfaceName = D.interfaceName_ "org.xmonad.Log"
    memberName    = D.memberName_ "Update"

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
    borderWidth        = myBorderWidth,
    modMask            = myModMask,
    workspaces         = myWorkspaces,
    normalBorderColor  = myNormalBorderColor,
    focusedBorderColor = myFocusedBorderColor,

    -- key bindings
    keys               = myKeys,
    mouseBindings      = myMouseBindings,

    -- hooks, layouts
    layoutHook         = smartBorders myLayout,
    manageHook         = myManageHook,
    startupHook        = myStartupHook
}

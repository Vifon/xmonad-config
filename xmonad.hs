{-# LANGUAGE FlexibleContexts, ImplicitParams #-}
module Main where

import XMonad hiding ( (|||) )  -- there is a modified version of ||| in XMonad.Layout.LayoutCombinators
import qualified XMonad.StackSet as W

import XMonad.Hooks.DebugStack
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FloatNext
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Place
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook

import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleRecentWS
import XMonad.Actions.CycleWS
import XMonad.Actions.DwmPromote
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.FloatSnap
import XMonad.Actions.Navigation2D
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves
import XMonad.Actions.SpawnOn
import XMonad.Actions.Submap
import XMonad.Actions.SwapWorkspaces
import XMonad.Actions.Warp
import XMonad.Actions.WindowGo
import qualified XMonad.Actions.FlexibleResize as Flex
import qualified XMonad.Actions.GridSelect as GridSelect
import qualified XMonad.Actions.WorkspaceNames as Labels

import XMonad.Layout.BoringWindows
import XMonad.Layout.ComboP
import XMonad.Layout.Gaps
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.LayoutBuilder
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.LimitWindows
import XMonad.Layout.Maximize
import XMonad.Layout.Minimize
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Named
import XMonad.Layout.NoBorders hiding (Never)
import XMonad.Layout.PerWorkspace
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spacing
import XMonad.Layout.StackTile
import XMonad.Layout.Tabbed
import XMonad.Layout.TwoPane
import qualified XMonad.Layout.Dwindle as Dwindle

import XMonad.Config.Desktop

import XMonad.Prompt
import XMonad.Prompt.AppendFile
import XMonad.Prompt.ConfirmPrompt

import XMonad.Util.EZConfig
import XMonad.Util.Run (spawnPipe, runProcessWithInput)
import XMonad.Util.WorkspaceCompare

import Data.Char
import Data.List
import Data.Maybe
import Data.Ratio ( (%) )
import Data.Tuple
import qualified Data.Map as M

import Control.Applicative ( (<$>) )
import Control.Monad.State

import System.Directory
import System.Exit
import System.IO
import System.Process

main :: IO ()
main = do
  xmproc <- spawnPipe xmobarCommand
  xmonad $ withNavigation2DConfig myNavigation2DConfig
         $ withUrgencyHookC BorderUrgencyHook { urgencyBorderColor = "red" }
                            urgencyConfig { suppressWhen = Focused }
    $ myConfig xmproc
    `additionalKeysP`
    myKeymap
  where xmobarCommand =
          "cabal --require-sandbox \
          \ --sandbox-config-file=$HOME/.xmonad/cabal.sandbox.config \
          \ exec xmobar"

term = "urxvtcd"

mySortOrder = getSortByIndex

commonLayoutHook l = (smartSpacing 2 . minimize . maximize) l
dwindle = named "dwindle" (commonLayoutHook $ limitWindows 8 $ Dwindle.Dwindle R Dwindle.CW 1.618 1.1)

commonLayouts = named "vsplit" (commonLayoutHook tall)
            ||| named "dishes" (commonLayoutHook $ StackTile 2 (3/100) (2/3))
            ||| dwindle
            ||| named "twopane" (commonLayoutHook $ limitSelect 1 1 tall)
            ||| named "resizable" (minimize . maximize $ mouseResizableTile)
            ||| named "grid"   (commonLayoutHook Grid)
            ||| named "full"   (smartBorders Full)
            ||| named "tabbed" (commonLayoutHook tabbed')
  where tall = Tall 1 (3/100) (1/2)

myNavigation2DConfig = def { defaultTiledNavigation = centerNavigation }

baseConfig = desktopConfig

myPP :: Handle -> PP
myPP h = xmobarPP
    { ppOutput          = hPutStrLn h
    , ppTitle           = xmobarColor "#4dafff" ""
                        . shorten 100
    , ppLayout          = xmobarColor "green" ""
    , ppCurrent         = xmobarColor "white" "#4169e1"
                        . pad
    , ppVisible         = xmobarColor "white" "#2139b1"
                        . pad
    , ppHiddenNoWindows = xmobarColor "#555555" ""
    , ppSep             = xmobarColor "#aaaaaa" "" " : "
    , ppSort            = mySortOrder
    , ppExtras          = [ willFloatAllNewPP (xmobarColor "red"  "")
                          , willFloatNextPP   (xmobarColor "cyan" "") ]
    , ppUrgent          = xmobarColor "white" "red"
                        . pad
    , ppOrder           = myOrder
    }
  where myOrder (ws:l:t:ex) = (ws:l:ex) ++ [t]
        myOrder defaultOrder = defaultOrder

-- | Override the layout name color depending on the currently used layout.
perLayoutColorPP :: PP -> X PP
perLayoutColorPP pp = do
  return pp { ppLayout = ppLayout pp . perLayoutColor }
    where perLayoutColor layoutName =
            let style = case layoutName of
                  "Full"   -> fg "red"
                  _        -> id
            in style layoutName
          fg c = xmobarColor c ""
          bg c = xmobarColor "" c

windowCountPP :: PP -> X PP
windowCountPP pp = do
  -- It could have been done with ppExtras but I prefer the counter to
  -- be shown alongside the layout, without any separators in between.
  ws <- currentStack
  let wc = windowCount ws
      wn = maybe 0 currentWindowIndex ws
  return pp { ppLayout = addCounter wn wc . ppLayout pp }
    where addCounter wn wc layout
            | wc > 1    = layout ++ " [" ++ show wn ++ "/" ++ show wc ++ "]"
            | otherwise = layout

workspaceCopiesPP :: (WorkspaceId -> String) -> PP -> X PP
workspaceCopiesPP trans pp = do
  -- It takes an additional transformation to apply at the same time,
  -- "atomically". I use to set the names/labels for workspaces
  -- affected by this PP. It must be done at the same time, otherwise
  -- one of the functions breaks the other.
  --
  -- Use identity (id) if you don't need this functionality.
  copies <- wsContainingCopies
  let checkCopies ws | ws `elem` copies = (xmobarColor "red" "" . trans) ws
                     | otherwise = ws
  return pp { ppHidden = ppHidden pp . checkCopies }

workspaceNamesWithCopiesPP :: PP -> X PP
workspaceNamesWithCopiesPP pp = do
  -- The order of these two PP transformers is important.
  names <- Labels.getWorkspaceNames
  return pp
    >>= Labels.workspaceNamesPP
    >>= workspaceCopiesPP names

myLogHook :: Handle -> X ()
myLogHook h = do
  let pp = myPP h
  return pp
    >>= workspaceNamesWithCopiesPP
    >>= windowCountPP
    >>= perLayoutColorPP
    >>= dynamicLogWithPP

myConfig h = baseConfig
        { terminal      = term
        , clickJustFocuses = False
        , modMask       = mod4Mask
        , keys          = myKeys
        , mouseBindings = myMouseBindings
        , layoutHook    = avoidStruts
                          . boringWindows
                          . mkToggle (MIRROR ?? NBFULL ?? EOT)
                          $ onWorkspace "1" (dwindle ||| commonLayouts)
                          $ onWorkspace "float" (named "floating" (commonLayoutHook simplestFloat)
                                                 ||| commonLayouts)
                          $ commonLayouts

        , manageHook    = placeHook (inBounds (smart (0.1, 0.1)))
                      <+> floatNextHook
                      <+> insertPosition Above Newer
                      <+> myManageHook
                      <+> manageHook baseConfig
        , startupHook   = startupHook baseConfig
                       >> checkKeymap (myConfig h) myKeymap
                       >> labelWorkspaces myWorkspacesLabels
                       >> fixJava
        , workspaces    = myWorkspaces
        , logHook       = logHook baseConfig <+> myLogHook h
        , handleEventHook = handleEventHook baseConfig
                        <+> fullscreenEventHook
        , focusedBorderColor = "#00bfff"
        , normalBorderColor  = "#2f4f4f"
        , borderWidth        = 1
        }
  where fixJava = setWMName "LG3D"

myKeymap =
  [ ("M-r"           , shellPromptHere myXPConfig)
  , ("M-u"           , do
        home <- io getHomeDirectory
        let path = home ++ "/.xmonad/notes"
        appendFilePrompt myXPConfig path)
  , ("M-C-p"         , resetWSLabel >> renameWorkspace myXPConfig)
  , ("M-="           , Labels.renameWorkspace myXPConfig)
  , ("M-S-="         , resetWSLabel)
  , ("M-o"           , Labels.renameWorkspace myXPConfig)
  , ("M-S-o"         , resetWSLabel)
  , ("M-M1-="        , labelWorkspaces myWorkspacesLabels)
  , ("M-M1-["        , labelWorkspaces myWorkspacesLabelsWork)
  , ("M-p"           , selectWorkspace myXPConfig)
  , ("M-S-p"         , withWorkspace myXPConfig (windows . W.shift))
  , ("M-S-<Backspace>", withWorkspace myXPConfig
                        (windows . copy))
  , ("M-C-<Backspace>", resetWSLabel >> removeWorkspace)
  , ("M-S-q"         , kill1)
  , ("M-S-M1-q"      , killAllOtherCopies)
  , ("M-S-s"         , banish' (1%50) UpperRight)
  , ("M-s"           , warp')
  , ("M-;"           , toggleFloatNext >> runLogHook)
  , ("M-S-;"         , spawn "pkill compton || compton --config ~/.xmonad/compton-focus-dim.conf")
  , ("M-S-d"         , sendMessage ToggleGaps >> sendMessage ToggleStruts)
  , ("M-C-d"         , sendMessage NextLayout)
  , ("M-C-<Return>"  , sendMessage NextLayout)
  , ("M-<Tab>"       , ifScreenChanges warp' . focusUrgentOr
                       $ cycleRecentWS [xK_Super_L] xK_Tab xK_q)
  , ("M-q"           , toggleWS)
  , ("M-j"           , focusDown)
  , ("M-k"           , focusUp)
  , ("M-m"           , focusMaster)
  , ("M-b"           , withFocused minimizeWindow)
  , ("M-S-b"         , sendMessage RestoreNextMinimizedWin)
  , ("M-<Return>"    , dwmpromote)
  , ("M5-<Return>"   , dwmpromote)
  , ("M-S-<Return>"  , promote)
  , ("M-S-j"         , windows W.swapDown)
  , ("M-S-k"         , windows W.swapUp)
  , ("M-C-j"         , rotSlavesDown)
  , ("M-C-k"         , rotSlavesUp)
  , ("M-h"           , sendMessage Shrink)
  , ("M-l"           , sendMessage Expand)
  , ("M-t"           , withFocused $ windows . W.sink)
  , ("M-,"           , sendMessage (IncMasterN 1))
  , ("M-."           , sendMessage (IncMasterN (-1)))
  , ("M-C-r"         , sendMessage $ Toggle MIRROR)
  , ("M-f"           , sendMessage $ Toggle NBFULL)
  , ("M-C-f"         , do
        sendMessage $ Toggle NBFULL
        sendMessage ToggleGaps
        sendMessage ToggleStruts)
  , ("M-i"           , GridSelect.goToSelected def)
  , ("M-S-e"         , exit)
  , ("M-S-r"         , spawn "make -C ~/.xmonad 2> ~/.xmonad/xmonad.err \
                             \ || xmessage -file ~/.xmonad/xmonad.err")
  , ("M-S-m"         , mediaPlayer)
  , ("M-<Escape>"    , mediaPlayer)
  , ("C-M-S-o"       , spawnHere "firefox -P default")
  , ("C-M-S-M5-o"    , spawnHere "firefox -P Business")
  ]
  ++
  [ (key, maximizeWindow)
  | key <- ["M-<Backspace>"
           ,"M5-<Backspace>"]]
  ++
  -- 2D navigation
  [ (m ++ k, ifScreenChanges warp' $ f dir False)
  | (m, f) <- [("M-"  , windowGo)
              ,("M-C-", windowSwap)]
  , (k, dir) <- [("<Up>"   , U)
                ,("<Down>" , D)
                ,("<Left>" , L)
                ,("<Right>", R)]]
  ++
  -- z/x switches prev/next workspaces
  -- with control empty workspaces are not skipped
  -- with shift the active window is moved too
  [ ("M-" ++ m1 ++ m2 ++ k, doTo dir pred mySortOrder f)
  | (dir, k) <- [(Next, "x")
                ,(Prev, "z")]
  , (m1, f) <- [(""  , \ws -> (windows . view) ws)
               ,("S-", \ws -> windows (W.shift ws)
                           >> windows (view ws))]
  , (m2, pred) <- [(""  , HiddenNonEmptyWS)
                  ,("C-", AnyWS)]
  ]
  ++
  -- M-w   - switch to empty desktop
  -- M-S-w - send to empty desktop
  -- M-C-w - send to empty desktop and switch there
  -- M-A-w - clone to empty desktop and switch there
  [("M-" ++ modifier ++ "w", doTo Next EmptyWS mySortOrder action)
  | (modifier, action) <- [(""  , windows . view)
                          ,("C-", \ws -> (windows . W.shift) ws
                                      >> (windows . view) ws)
                          ,("S-", windows . W.shift)
                          ,("M1-", \ws -> (windows . copy) ws
                                       >> (windows . view) ws)]]
  ++
  [(key, sendMessage $ JumpToLayout layout)
  | (key, layout) <- [("M-v"  , "dwindle")
                     ,("M-S-v", "vsplit")
                     ,("M-C-v", "twopane")
                     ,("M-g"  , "grid")
                     ,("M-S-f", "full")
                     ,("M-c"  , "tabbed")
                     ,("M-C-s", "dishes")]]
  ++
  -- workspace switching
  [("M-" ++ m ++ k, f i)
  | (i, k) <- zip myWorkspaces myWorkspacesKeys
  , (m, f) <- [(""     , ifScreenChanges warp' . toggleOrView')
              ,("S-"   , windows . W.shift)
              ,("M1-", windows . copy)
              ,("C-"   , Labels.swapWithCurrent)]]
  ++
  -- monitor switching
  [("M-" ++ m ++ "a", action)
  | (m, action) <- [(""  , nextScreen >> warp')
                   ,("S-", shiftNextScreen >> nextScreen >> warp')
                   ,("C-", swapNextScreen)]]
  where toggleOrView'  = toggleOrDoSkip [] view
        maximizeWindow = withFocused $ sendMessage . maximizeRestore
        view           = W.greedyView
        resetWSLabel   = Labels.setCurrentWorkspaceName ""
        exit = confirmPrompt myXPConfig "exit" $ io (exitWith ExitSuccess)
        mediaPlayer = toggleFloatNext >> spawn "urxvtcd -g 150x32 -e ncmpcpp-run"

myWorkspaces     = map show $ [1..10]
myWorkspacesKeys = map show $ [1..9] ++ [0]

data WSLabels =
  WSLabels [String]                -- ^ Ordered labels
           [(WorkspaceId, String)] -- ^ Unordered labels

myWorkspacesLabels :: WSLabels
myWorkspacesLabels = WSLabels ["www" , "IRC", "", "", ""] [("10","")]

myWorkspacesLabelsWork :: WSLabels
myWorkspacesLabelsWork = WSLabels
                           ["www" , "IRC", "chat", "code", "scratch"]
                           [("10","worklogs")]

labelWorkspaces :: WSLabels -> X ()
labelWorkspaces labels =
  uncurry Labels.setWorkspaceName `mapM_` (enumerate ordered ++ unordered)
  where WSLabels ordered unordered = labels
        seq_ids = show <$> [1..]
        enumerate = zip seq_ids

myManageHook = composeAll
    [ isFullscreen --> doFullFloat
    , className =? "Keepassx" <&&> title =? "Auto-Type - KeePassX" --> doFloat
    , className =? "TrayCalendar" --> doIgnore
    , manageSpawn
    , manageDocks
    ] where role = stringProperty "WM_WINDOW_ROLE"

myFont size = "xft:Bitstream Vera Sans Mono:size="
           ++ show size
           ++ ":antialias=true"

myXPConfig = def { position = Bottom
                 , historySize = 50
                 , font = myFont 12 ++ ":bold"
                 , promptKeymap = emacsLikeXPKeymap' isWordSeparator
                 , historyFilter = deleteAllDuplicates
                 } where isWordSeparator c = isSpace c || c == '/'

myTabbedTheme = def { fontName = myFont 10 ++ ":bold"
                    , activeColor       = "#00688b"
                    , activeBorderColor = "#009acd"
                    }

tabbed' = tabbed shrinkText myTabbedTheme

myKeys conf@(XConfig {XMonad.modMask = modm}) = let ?conf = conf in M.fromList
    [ ((modm .|. shiftMask, xK_space), resetLayouts)
    , ((modm, xK_apostrophe), submapT
        [ ("m", Just "Thunderbird", spawnHere "thunderbird")
        , ("g", Just "GnuCash", runOrRaise "gnucash" (className =? "Gnucash"))
        , ("c", Just "calibre", runOrRaise "calibre" (className =? "libprs500"))
        , ("s", Just "Signal", runOrRaise "signal-desktop"
                               (resource =? signalResource
                                <||> className =? "Signal"))
        , ("t", Just "Telegram", runOrRaise "telegram" (className =? "TelegramDesktop"))
        , ("k", Just "KeePassX", spawnHere "keepassx")
        , ("M-m", Just "Spotify", runOrRaise "spotify" (className =? "Spotify"))
        , ("S-m", Just "Mumble", spawnHere "mumble")
        , ("p", Just "pavucontrol", spawnHere "pavucontrol")
        , ("a", Nothing, spawnHere "arandr")
        , ("S-s", Just "Synergy", spawnHere "run-one synergy")
        , ("S-t", Just "Transmission", spawnHere "transmission-gtk")
        , ("[", Nothing, spawnHere "touch ~/.pomodoro_session")
        , ("]", Nothing, spawnHere "rm -f ~/.pomodoro_session")
        , ("S-[", Nothing, spawnHere "pymodoro -l 25 | dzen2")
        , ("S-]", Nothing, spawnHere "pkill pymodoro")
        , ("1", Nothing, spawnHere "~/.screenlayout/single.sh")
        , ("2", Nothing, spawnHere "~/.screenlayout/multidisplay.sh")
        , ("3", Nothing, spawnHere "~/.screenlayout/external.sh")
        , ("S-d", Just "Debug", debugStackString >>= io . displayText)
        ])
    , ((modm, xK_d), submapT'
                     [("d", "dwindle")
                     ,("w", "twopane")
                     ,("v", "vsplit")
                     ,("S-d", "dishes")
                     ,("r", "resizable")
                     ,("t", "tabbed")
                     ,("g", "grid")
                     ,("f", "full")])
    ]
  where resetLayouts = setLayout $ XMonad.layoutHook conf
        signalResource = "crx_bikioccmkafdpakkkcpdbppfkghcmihk"
        displayText text = do
          (Just std_in, _, _, _) <- createProcess (proc "zenity"
                                                        [ "--text-info"
                                                        , "--font"
                                                        , "monospace 10"])
                                    { std_in = CreatePipe }
          hPutStr std_in text
          hClose std_in

-- | A high-level wrapper around `submap` that displays a tooltip with
-- all the keys and their descriptions.
submapT :: (?conf :: XConfig Layout)       -- ^ An implicitly passed XMonad config.
        => [(String, Maybe String, X ())]  -- ^ [(key, description, action)]; don't display if description is empty.
        -> X ()
submapT spec = do
  let outer_sep = "   "
      inner_sep = ": "
  let tooltip = concat . intercalate [outer_sep] $
                [ ["^fg(red)", key, "^fg()", inner_sep, description]
                | (key, Just description, _) <- spec
                ]
  dzen_std_in <- io $ do
    (Just std_in, _, _, _) <- createProcess (proc "dzen2" [])
                              { std_in = CreatePipe }
    hPutStrLn std_in tooltip
    hFlush std_in
    return std_in
  submap . mkKeymap ?conf $
    [ (key, action) | (key, _, action) <- spec ]
  io $ hClose dzen_std_in

-- | A specialized version of `submapT` that is used to switch the XMonad layouts.
submapT' :: (?conf :: XConfig Layout) -- ^ An implicitly passed XMonad config.
         => [(String, String)]        -- ^ [(key, layoutname)]
         -> X()
submapT' spec =
  let (keys, layouts) = unzip spec in
    submapT $ zip3 keys (Just <$> layouts) $ sendMessage . JumpToLayout <$> layouts

myMouseBindings conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> snapMagicMove (Just magicSnapThreshold)
                                                        (Just magicSnapThreshold) w))
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
    , ((modm, button3), (\w -> focus w >> Flex.mouseResizeEdgeWindow (1/2) w
                                       >> snapMagicResize [L,R,U,D] (Just magicSnapThreshold)
                                                                    (Just magicSnapThreshold) w))
    ]
  where magicSnapThreshold = 30

-- | Move the mouse cursor to the center of the current window.
warp :: X ()
warp = warpToWindow (1%2) (1%2)

-- | Move the mouse cursor to the center of the current screen.
warpScreen :: X ()
warpScreen = do
  ws <- gets windowset
  let screen = W.screen . W.current $ ws
  warpToScreen screen (1%2) (1%2)

-- | Move the mouse cursor to the center of the current window. If
-- there are no windows, center the cursor on the screen.
warp' :: X ()
warp' = do
  windowCount <- currentWindowCount
  if windowCount == 0
    then warpScreen
    else warp

-- | Move the mouse cursor to the upper right corner of the current
-- window with some margin.
banish' :: Rational -> Corner ->  X ()
banish' margin direction = case direction of
  LowerRight -> warpToWindow max max
  LowerLeft  -> warpToWindow min max
  UpperLeft  -> warpToWindow min min
  UpperRight -> warpToWindow max min
  where min = 0 + margin
        max = 1 - margin

-- | Perform the first action only if the second action changes the
-- active screen.
ifScreenChanges :: X () -- ^ Action performed conditionally.
                -> X () -- ^ Action that may change the screen.
                -> X ()
ifScreenChanges action x = do
  screenBefore <- currentScreen
  x
  screenAfter <- currentScreen
  when (screenBefore /= screenAfter)
    action
  where currentScreen = (W.screen . W.current) <$> gets windowset

-- | Current workspace window stack.
currentStack :: X (Maybe (W.Stack Window))
currentStack = (W.stack . W.workspace . W.current) <$> gets windowset

-- | Current workspace window layout.
currentLayout :: X (Layout Window)
currentLayout = (W.layout . W.workspace . W.current) <$> gets windowset

-- | Number of windows in a possibly empty stack.
windowCount :: Maybe (W.Stack Window) -> Int
windowCount Nothing = 0
windowCount (Just (W.Stack focus up dn)) = 1 + length up + length dn

-- | Index of the current window in the stack.
currentWindowIndex :: W.Stack Window -> Int
currentWindowIndex (W.Stack _ up _) = 1 + length up

-- | Number of windows on the current workspace.
currentWindowCount :: X Int
currentWindowCount = windowCount <$> currentStack

-- | Focus the urgent window if there are any. Otherwise perform the
-- passed action.
focusUrgentOr :: X () -> X ()
focusUrgentOr x = do
  urgents <- readUrgents
  if length urgents > 0
    then focusUrgent
    else x

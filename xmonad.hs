{-# LANGUAGE ImplicitParams #-}
import XMonad hiding ( (|||) )  -- there is a modified version of ||| in XMonad.Layout.LayoutCombinators
import qualified XMonad.StackSet as W

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
import XMonad.Actions.Promote
import XMonad.Actions.SpawnOn
import XMonad.Actions.SwapWorkspaces
import XMonad.Actions.Warp
import qualified XMonad.Actions.FlexibleResize as Flex
import qualified XMonad.Actions.WorkspaceNames as Labels

import XMonad.Layout.BoringWindows
import XMonad.Layout.ComboP
import XMonad.Layout.Gaps
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.LayoutBuilder
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Maximize
import XMonad.Layout.Minimize
import XMonad.Layout.Named
import XMonad.Layout.NoBorders hiding (Never)
import XMonad.Layout.PerWorkspace
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spacing
import XMonad.Layout.Spiral
import XMonad.Layout.StackTile
import XMonad.Layout.Tabbed
import XMonad.Layout.TwoPane

import XMonad.Config.Desktop

import XMonad.Prompt

import XMonad.Util.EZConfig
import XMonad.Util.Run (spawnPipe, runProcessWithInput)
import XMonad.Util.WorkspaceCompare

import Data.Char
import Data.Maybe
import Data.Ratio ( (%) )
import qualified Data.Map as M

import Control.Applicative ( (<$>) )
import Control.Monad.State

import System.Exit
import System.IO

main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ withUrgencyHookC BorderUrgencyHook { urgencyBorderColor = "red" }
                            urgencyConfig { suppressWhen = Focused }
    $ myConfig xmproc
    `additionalKeysP`
    myKeymap

term = "urxvtcd"

mySortOrder = getSortByTag

commonLayoutHook l = (smartSpacing 2 . minimize . maximize) l

commonLayouts = named "vsplit" (commonLayoutHook tall)
            ||| named "dishes" (commonLayoutHook $ StackTile 2 (3/100) (2/3))
            ||| named "tabbed split" (commonLayoutHook tallTabbed)
            ||| named "spiral" (commonLayoutHook $ spiral (6/7))
            ||| named "hsplit" (commonLayoutHook $ Mirror tall)
            ||| named "grid"   (commonLayoutHook Grid)
            ||| named "full"   (smartBorders Full)
            ||| named "tabbed" (commonLayoutHook tabbed')
  where tall = Tall 1 (3/100) (1/2)

baseConfig = desktopConfig

myPP :: Handle -> PP
myPP h = xmobarPP
    { ppOutput          = hPutStrLn h
    , ppTitle           = xmobarColor "#4dafff" "" . shorten 100
    , ppLayout          = xmobarColor "green"   ""
    , ppCurrent         = xmobarColor "white"  "#4169e1" . pad
    , ppVisible         = xmobarColor "white"  "#2139b1" . pad
    , ppHiddenNoWindows = xmobarColor "#555555" ""
    , ppSep             = xmobarColor "#aaaaaa" "" " : "
    , ppSort            = mySortOrder
    , ppExtras          = [ willFloatAllNewPP (xmobarColor "red"  "")
                          , willFloatNextPP   (xmobarColor "cyan" "") ]
    , ppUrgent          = xmobarColor "white" "red" . pad
    }

workspaceCopiesPP :: (WorkspaceId -> String) -> PP -> X PP
workspaceCopiesPP trans pp = do
  -- It takes an additional transformation to apply at the same time,
  -- "atomically". I use to set the names/labels for workspaces
  -- affected by this PP. It must be done at the same time, otherwise
  -- one of the functions breaks the other.
  --
  -- Use identity (id) if you don't need this functionality.
  copies <- wsContainingCopies
  let checkCopies ws | ws `elem` copies = (xmobarColor "red" "black" . trans) ws
                     | otherwise = ws
  return $ pp
    { ppHidden = ppHidden pp . checkCopies
    }

myLogHook :: Handle -> X ()
myLogHook h = do
  names <- Labels.getWorkspaceNames
  let pp = myPP h
  return pp
    >>= Labels.workspaceNamesPP -- <- The order of these two is important.
    >>= workspaceCopiesPP names -- <-
    >>= dynamicLogWithPP

myConfig h = baseConfig
        { terminal      = term
        , modMask       = mod4Mask
        , keys          = myKeys
        , mouseBindings = myMouseBindings
        , layoutHook    = gaps [(U,20)] . avoidStruts . boringWindows
                          $ onWorkspace "browser" (named "browser" (commonLayoutHook browserLayout)
                                                   ||| commonLayouts)
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
                       >> fixJava
        , workspaces    = myWorkspaces
        , logHook       = logHook baseConfig <+> myLogHook h
        , handleEventHook = handleEventHook baseConfig <+> fullscreenEventHook
        , focusedBorderColor = "#00bfff"
        , normalBorderColor  = "#2f4f4f"
        }
  where fixJava = setWMName "LG3D"

myKeymap =
  [ ("M-r"           , shellPromptHere myXPConfig)
  , ("M-C-p"         , resetWSName >> renameWorkspace myXPConfig)
  , ("M-="           , Labels.renameWorkspace myXPConfig)
  , ("M-S-="         , resetWSName)
  , ("M-p"           , selectWorkspace myXPConfig)
  , ("M-S-p"         , withWorkspace myXPConfig (windows . W.shift))
  , ("M-S-<Backspace>", withWorkspace myXPConfig
                        (windows . copy))
  , ("M-C-<Backspace>", resetWSName >> removeWorkspace)
  , ("M-S-q"         , kill1)
  , ("M-S-s"         , banish UpperRight)
  , ("M-C-s"         , warp')
  , ("M-;"           , toggleFloatNext >> runLogHook)
  , ("M-d"           , sendMessage NextLayout)
  , ("M-<Tab>"       , warpIfScreenChanges . focusUrgentOr
                       $ cycleRecentWS [xK_Super_L] xK_Tab xK_q)
  , ("M-q"           , toggleWS)
  , ("M-j"           , focusDown)
  , ("M-k"           , focusUp)
  , ("M-m"           , focusMaster)
  , ("M-b"           , withFocused minimizeWindow)
  , ("M-S-b"         , sendMessage RestoreNextMinimizedWin)
  , ("M-<Return>"    , dwmpromote)
  , ("M-S-<Return>"  , promote)
  , ("M-S-j"         , windows W.swapDown)
  , ("M-S-k"         , windows W.swapUp)
  , ("M-h"           , sendMessage Shrink)
  , ("M-l"           , sendMessage Expand)
  , ("M-t"           , withFocused $ windows . W.sink)
  , ("M-,"           , sendMessage (IncMasterN 1))
  , ("M-."           , sendMessage (IncMasterN (-1)))
  , ("M-S-e"         , exit)
  , ("M-S-r"         , spawn "xmonad --recompile && xmonad --restart")
  ]
  ++
  [ (key, maximizeWindow)
  | key <- ["M-f"
           ,"M-<Backspace>"
           ,"M5-<Backspace>"]]
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
  [("M-" ++ modifier ++ "w", doTo Next EmptyWS mySortOrder action)
  | (modifier, action) <- [(""  , windows . view)
                          ,("C-", \ws -> (windows . W.shift) ws
                                      >> (windows . view) ws)
                          ,("S-",   windows . W.shift)]]
  ++
  [(key, sendMessage $ JumpToLayout layout)
  | (key, layout) <- [("M-v"  , "tabbed split")
                     ,("M-S-v", "vsplit")
                     ,("M-g"  , "grid")
                     ,("M-S-f", "full")
                     ,("M-c"  , "tabbed")
                     ,("M-s"  , "dishes")]]
  ++
  -- workspace switching
  [("M-" ++ m ++ i, f i)
  | i <- myWorkspaces
  , (m, f) <- [(""     , warpIfScreenChanges . toggleOrView')
              ,("S-"   , windows . W.shift)
              ,("S-M1-", windows . copy)
              ,("C-"   , Labels.swapWithCurrent)]]
  ++
  -- monitor switching
  [("M-" ++ m ++ "a", action)
  | (m, action) <- [(""  , nextScreen >> warp')
                   ,("S-", shiftNextScreen >> nextScreen >> warp')
                   ,("C-", swapNextScreen)]]
  where toggleOrView'  = toggleOrDoSkip [] view
        maximizeWindow = withFocused $ sendMessage . maximizeRestore
        view           = W.view
        resetWSName    = Labels.setCurrentWorkspaceName ""

myWorkspaces = map show [0..9]

myManageHook = composeAll
    [ isFullscreen --> doFullFloat
    , manageSpawn
    , manageDocks
    ] where role = stringProperty "WM_WINDOW_ROLE"

myFont size = "xft:Bitstream Vera Sans Mono:size="
           ++ show size
           ++ ":bold:antialias=true"

myXPConfig = defaultXPConfig { position = Bottom
                             , historySize = 10
                             , font = myFont 12
                             , promptKeymap = emacsLikeXPKeymap' isWordSeparator
                             , historyFilter = deleteAllDuplicates
                             } where isWordSeparator c = isSpace c || c == '/'

myTabbedTheme = defaultTheme { fontName = myFont 10
                             , activeColor       = "#00688b"
                             , activeBorderColor = "#009acd"
                             }

tabbed' = tabbed shrinkText myTabbedTheme

tallTabbed = layoutN 1 (relBox 0 0 0.5 1) (Just $ relBox 0 0 1 1) Full
             $ layoutN 1 (relBox 0.5 0 1 0.5) (Just $ relBox 0.5 0 1 1) Full
             $ layoutAll (relBox 0.5 0.5 1 1) tabbed'

programmingLayout =
  combineTwoP (Mirror $ TwoPane (3/100) (3/4))
              tabbed'
              (Mirror $ Tall 0 (3/100) (1/2))
              (Not $ ClassName "URxvt")

browserLayout = withIM (2%5) (Not isBrowser) tabbed'
  where isBrowser :: Property
        isBrowser = foldl Or (Const False) $ fmap ClassName browserClasses
          where browserClasses :: [String]
                browserClasses = ["Chromium", "Chromium-browser", "luakit", "Firefox", "Opera"]

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm .|. shiftMask, xK_space), resetLayouts) ]
  where resetLayouts = setLayout $ XMonad.layoutHook conf

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


exit :: X ()
exit = do
  response <- runProcessWithInput "dmenu" ["-p", "Really quit?"] "no\nyes\n"
  when (response == "yes\n") $ io (exitWith ExitSuccess)

warp :: X ()
warp = warpToWindow (1%2) (1%2)

warpScreen :: X ()
warpScreen = do
  ws <- gets windowset
  let screen = W.screen . W.current $ ws
  warpToScreen screen (1%2) (1%2)

warp' :: X ()
warp' = do
  windowCount <- currentWindowCount
  if windowCount == 0
    then warpScreen
    else warp

warpIfScreenChanges :: X () -> X ()
warpIfScreenChanges x = do
  screenBefore <- currentScreen
  x
  screenAfter <- currentScreen
  when (screenBefore /= screenAfter)
    warp'
  where currentScreen = (W.screen . W.current) <$> gets windowset

currentStack :: X (Maybe (W.Stack Window))
currentStack = (W.stack . W.workspace . W.current) `fmap` gets windowset

windowCount :: (Maybe (W.Stack Window)) -> Int
windowCount Nothing = 0
windowCount (Just (W.Stack focus up dn)) = 1 + length up + length dn

currentWindowCount :: X Int
currentWindowCount = windowCount `fmap` currentStack

focusUrgentOr :: X () -> X ()
focusUrgentOr x = do
  urgents <- readUrgents
  if length urgents > 0
    then focusUrgent
    else x

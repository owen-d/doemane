{-# LANGUAGE OverloadedStrings #-}

module Repl where

import qualified Brick
import qualified Brick.BChan                as BChan
import qualified Brick.Widgets.Border       as Border
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center       as C
import           Control.Concurrent         (forkIO, threadDelay)
import           Control.Monad              (forever, void)
import qualified Data.Char                  as Char
import           Data.List                  (nub, (\\))
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (catMaybes, fromMaybe)
import           Data.String                (words)
import           Graphics.Vty               as V
import           PTree                      (PTree)
import qualified PTree


allowed = ['a'..'z'] ++ ['A'..'Z'] ++ [' ']

eval :: Map String [String] -> PTree String String -> [String] -> [[String]]
eval cache tree ws =
  let sounds = concat . catMaybes $ map lookupWord ws
  in (fromMaybe [] (PTree.find tree sounds)) \\ [ws]
  where
    lookupWord w = Map.lookup w cache

data Engine =
  Engine
    { input      :: String
    , homophones :: [[String]]
    , cache      :: Map [Char] [String]
    , tree       :: PTree String String
    }

step :: Engine -> Engine
step e = e {homophones = eval (cache e) (tree e) (words $ input e)}

data Tick = Tick
type Name = ()

app :: Brick.App Engine Tick Name
app = Brick.App
  { Brick.appDraw = drawUI
  , Brick.appChooseCursor = Brick.neverShowCursor
  , Brick.appHandleEvent = handleEvent
  , Brick.appStartEvent = return
  , Brick.appAttrMap = const theMap
  }

main :: Map [Char] [String] -> PTree String String -> IO ()
main cache tree = do
  chan <- BChan.newBChan 10
  -- TODO: remove unnecessary tick types/events
  forkIO $
    forever $ do
      BChan.writeBChan chan Tick
      threadDelay 100000
  let e = Engine {input = [], homophones = [], cache = cache, tree = tree}
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  void $ Brick.customMain initialVty buildVty (Just chan) app e
  return ()

handleEvent :: Engine -> Brick.BrickEvent Name Tick -> Brick.EventM Name (Brick.Next Engine)
handleEvent e (Brick.VtyEvent (V.EvKey V.KBS [])) = Brick.continue . step . inputPred $ e
handleEvent e (Brick.VtyEvent (V.EvKey V.KEnter [])) = Brick.continue . step . clear $ e
handleEvent e (Brick.VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) = Brick.halt e
handleEvent e (Brick.VtyEvent (V.EvKey (V.KChar k) [])) = Brick.continue . step $ handleKey k e
handleEvent e (Brick.AppEvent Tick) = Brick.continue e
handleEvent e _ = Brick.continue e

-- remove one char from input
inputPred :: Engine -> Engine
inputPred e
  | input e == [] = e
  | otherwise = e{input=dropTail 1 $ input e}
  where
    dropTail _ [] = []
    dropTail 0 xs = xs
    dropTail n xs = dropTail (n - 1) (init xs)

handleKey :: Char -> Engine -> Engine
handleKey k e
  | k `elem` allowed = e{input=(input e) ++ [Char.toUpper k]}
  | otherwise = e

clear :: Engine -> Engine
clear e = e{input=[], homophones=[]}

drawUI :: Engine -> [Brick.Widget Name]
drawUI e = (:[]) $
  box "doemane" $
  Brick.vBox [ C.hCenter $ Brick.str $ "input = " <> input e
             , drawMatches (homophones e)
             ]

drawMatches :: [[String]] -> Brick.Widget Name
drawMatches xs =
  let
    groups = byLength xs
  in
    Brick.hBox $ map mkBox groups
  where
    mkBox (ln, ws) = box (show ln <> " length words") $
      Brick.vBox $ map (Brick.str . show) ws

box :: String -> Brick.Widget a -> Brick.Widget a
box label x = Brick.withBorderStyle BS.unicodeBold $
  Border.borderWithLabel (Brick.str label) $
  C.hCenter $
  Brick.padAll 1 $
  x

theMap :: Brick.AttrMap
theMap = Brick.attrMap V.defAttr []

-- byLength separates a list of lists, grouping them by their lengths
byLength :: [[a]] -> [(Int, [[a]])]
byLength [] = []
byLength xs =
  let
    lengths = nub $ map length xs
  in [ (l, filter ((==l) . length) xs)| l <- lengths]


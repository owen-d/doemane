{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Repl where

import           Brick                      ((<+>))
import qualified Brick
import qualified Brick.BChan                as BChan
import qualified Brick.Widgets.Border       as Border
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center       as C
import           Control.Concurrent         (forkIO, threadDelay)
import           Control.Monad              (forever, void)
import           Data.Aeson                 (FromJSON, ToJSON, encode)
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.Char                  as Char
import           Data.List                  ((\\))
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (catMaybes, fromMaybe)
import           Data.String                (words)
import           GHC.Generics               (Generic)
import           Graphics.Vty               as V
import           PTree                      (PTree)
import qualified PTree
import qualified System.IO                  as IO

data Result =
  Result
    { sounds  :: [String]
    , matches :: [[String]]
    , sources :: [String]
    }
  deriving (Show, Generic)

instance FromJSON Result
instance ToJSON Result

allowed = ['a'..'z'] ++ ['A'..'Z'] ++ [' ']

prompt :: IO [String]
prompt = do
  IO.putStrLn "Enter Input (space separated words):"
  line <- IO.getLine
  let ws = words $ (filter $ \x -> x `elem` allowed) $ map Char.toUpper line
  return ws

eval :: Map String [String] -> PTree String String -> [String] -> [[String]]
eval cache tree ws =
  let sounds = concat . catMaybes $ map lookupWord ws
  in (fromMaybe [] (PTree.find tree sounds)) \\ [ws]
  where
    lookupWord w = Map.lookup w cache

display :: String -> [[String]] -> IO ()
display src results = do
  IO.putStrLn $ src <> " -> " <> (CL.unpack . encode $ results)

run :: Map [Char] [String] -> PTree String String -> IO ()
run map tree = do
  ws <- prompt
  let matches = eval map tree ws
  display (unwords ws) matches
  run map tree


-- ------------------------ brick

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
handleEvent e (Brick.VtyEvent (V.EvKey V.KBS [])) = Brick.continue $ inputPred e
handleEvent e (Brick.VtyEvent (V.EvKey V.KEnter [])) = Brick.continue $ clear e
handleEvent e (Brick.VtyEvent (V.EvKey (V.KChar k) [])) = handleKey k e
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

handleKey :: Char -> Engine -> Brick.EventM Name (Brick.Next Engine)
handleKey k e
  | k `elem` allowed = Brick.continue $ e{input=(input e) ++ [Char.toUpper k]}
  | otherwise = Brick.continue e

clear :: Engine -> Engine
clear e = e{input=[], homophones=[]}

drawUI :: Engine -> [Brick.Widget Name]
drawUI e = (:[]) $
  Brick.withBorderStyle BS.unicodeBold $
  Border.borderWithLabel (Brick.str "doemane") $
  box "matches" $
  drawMatches (homophones e)

drawMatches :: [[String]] -> Brick.Widget Name
drawMatches _ = Brick.vBox [ ]

drawMatch :: [String] -> Brick.Widget Name
drawMatch ws = Brick.vBox $ map (Brick.str . show) ws

box :: String -> Brick.Widget a -> Brick.Widget a
box label x = Brick.withBorderStyle BS.unicodeBold $
  Border.borderWithLabel (Brick.str label) $
  C.hCenter $
  Brick.padAll 1 $
  x

theMap :: Brick.AttrMap
theMap = undefined

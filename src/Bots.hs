{-# LANGUAGE OverloadedStrings #-}
-- Move a block around using the arrow keys.

module Bots
where

import Control.Applicative ((<$>), (<*>))
import Control.Lens
import Control.Monad.State (State, execState, get)
import Control.Monad (when)

import Data.Set (Set, member, empty, insert, delete)

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import System.Random (randomRs, newStdGen)

import Debug.Trace
import qualified Data.ByteString.Char8 as BS

data World = W
             { _player :: (Int,Int)
             , _keys   :: Set Key
             }

gameSize = 300

playerSpeed = 1

windowWidth = 800
windowHeight = 600

sqSize = 20

initial :: World
initial = W (0,0) empty

drawWorld :: World -> Picture
drawWorld (W (x,y) _) = scale sqSize sqSize $ Pictures
  [ drawPlayer       `at` (x, y)
  , drawX            `at` (3,4)
  , circle (0.5)     `at` (-2,2)
  , drawDiamond      `at` (-2,-5)
  , drawText "Hello" `at` (-3,10)
  , boxBitMap        `at` (4,-4)
  ]
  where
    p `at` (x,y) = translate (fromIntegral x) (fromIntegral y) p; infixr 1 `at`

box on off width height =
  BS.concat $ [ top ] ++ replicate (height-2) side ++ [ top ]
  where top  = BS.concat $ replicate width on
        side = BS.concat $ [ on ] ++ replicate (width-2) off ++  [ on ]

boxBitMap = scale 0.05 0.05 $ bitmapOfByteString 20 20 (box "\xff\xff\xff\x00" "\x00\x00\x00\x00" 20 20) True

drawText :: String -> Picture
drawText str = scale 0.01 0.01 $ text str

drawPlayer :: Picture
drawPlayer = rectangleSolid 1.0 1.0

drawX :: Picture
drawX = Pictures [ line [ (-d,-d), (d,d) ],
                   line [ (-d,d), (d,-d) ] ]
  where d = 0.5

drawDiamond :: Picture
drawDiamond = line [ (d,0), (0,d), (-d,0), (0,-d), (d, 0) ]
  where d = 0.5

updateKey k True w = w { _keys = insert k (_keys w) }
updateKey k False w = w { _keys = delete k (_keys w) }

handle :: Event -> World -> World
handle (EventKey k s _ _) = updateKey k (s == Down)
handle _ = id

draw = drawWorld

update :: Float -> World -> World
update time w =
  let keys = _keys w
      (x,y) = _player w
      keyPressed k = member (SpecialKey k) keys
      delta = 1
      go | keyPressed KeyUp    = w { _player = (x, y+delta) }
         | keyPressed KeyDown  = w { _player = (x, y-delta) }
         | keyPressed KeyLeft  = w { _player = (x-delta,y) }
         | keyPressed KeyRight = w { _player = (x+delta,y) }
         | otherwise           = w
  in go

main = do
  let world = initial
  play display backColor fps world draw handle update
  where
    display   = InWindow "Walk!" (windowWidth, windowHeight) (200, 200)
    backColor = white
    fps       = 10


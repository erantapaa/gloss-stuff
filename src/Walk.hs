
-- Move a block around using the arrow keys.

module Walk
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

data World = W
             { _player :: (Float,Float)
             , _keys   :: Set Key
             }

gameSize = 300

playerSpeed = 1

windowWidth = 800
windowHeight = 600

sqSize = 0.02

initial :: World
initial = W (0,0) empty

drawWorld :: World -> Picture
drawWorld (W (x,y) _) = scale gameSize gameSize $ Pictures
  [ drawPlayer `at` (x,y)
  ]
  where
    p `at` (x,y) = translate x y p; infixr 1 `at`

drawPlayer :: Picture
drawPlayer = rectangleSolid sqSize sqSize

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
      delta = time*playerSpeed
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
    fps       = 120


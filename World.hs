module World where

import Utilities
import System.Console.ANSI

type Sprite = (Char, Color)
data Entity = Entity 
    { pos :: Coord
    , lastPos :: Coord
    , sprite :: Sprite
    , solid :: Bool
    }

data World = World
    { hero :: Entity
    , mapWidth :: Int
    , mapHeight :: Int
    , tilemap :: [Entity]
    }

moveBy :: Entity -> Coord -> Entity
moveBy entity@(Entity (x, y) _ _ _) (dx, dy) = Entity
    { pos = (x + dx, x + dy)
    , lastPos = (x, y)
    , sprite = sprite entity
    , solid = solid entity
    }

reverseIfCollides :: Entity -> World -> Entity
reverseIfCollides entity@(Entity (x, y) lp _ _) world@(World _ w h tm) = Entity
    { pos = if collides then lp else (x, y)
    , lastPos = lp
    , sprite = sprite entity
    , solid = solid entity
    }
    where collides = if solid $ tm!!(x * (h + 1) + y) then True else False

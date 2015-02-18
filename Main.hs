--file: Main.hs
module Main where

import Prelude hiding (Either(..))
import System.Console.ANSI
import System.IO
import Utilities
import World

data Input = Up
           | Down
           | Left
           | Right
           | Exit
           deriving (Eq)

createTileFromCoord :: Coord -> Coord -> Entity
createTileFromCoord coord bounds = Entity coord coord tileSprite isWall
    where tileSprite = (if isWall then '#' else '.', White)
          isWall = if x == 0 || y == 0 || x == width || y == height then True else False
          (x, y) = coord
          (width, height) = bounds

main :: IO ()
main = do
    hSetEcho stdin False
    hSetBuffering stdin  NoBuffering
    hSetBuffering stdout NoBuffering
    hideCursor
    clearScreen
    setTitle "Thieflike"
    mapM drawEntity (tilemap world)
    gameLoop $ world
    where world = World
              { hero = (Entity (1, 1) (0, 0) ('@', Blue) True)
              , mapWidth = 20
              , mapHeight = 10
              , tilemap = [createTileFromCoord (x, y) (20, 10)
                    | x <- [0..20]
                    , y <- [0..10]]
              }

drawEntity :: Entity -> IO ()
drawEntity entity = do
  setCursorPosition entityY entityX
  setSGR [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Vivid entityColor ]
  putStr [entityChar]
  where (entityX, entityY) = pos entity
        (entityChar, entityColor) = sprite entity

-- receive a character and return our Input data structure,
-- recursing on invalid input
getInput :: IO Input
getInput = do
  char <- getChar
  case char of
    'q' -> return Exit
    'w' -> return Up
    'k' -> return Up
    's' -> return Down
    'j' -> return Down
    'a' -> return Left
    'h' -> return Left
    'd' -> return Right
    'l' -> return Right
    _ -> getInput

dirToCoord d
  | d == Up    = (0, -1)
  | d == Down  = (0,  1)
  | d == Left  = (-1, 0)
  | d == Right = (1,  0)
  | otherwise  = (0,  0)

-- add the supplied direction to the hero's position, and set that
-- to be the hero's new position, making sure to limit the hero's
-- position between 0 and 80 in either direction
handleDir :: World -> Input -> IO ()
handleDir world@(World hero width height tiles) input = gameLoop World
    { hero = reverseIfCollides (hero `moveBy` delta) world
    , mapWidth = width
    , mapHeight = height
    , tilemap = tiles
    }
    where delta = dirToCoord input

-- update the game loop to add in the goodbye message
gameLoop :: World -> IO ()
gameLoop world@(World hero _ height tiles) = do
  drawEntity hero
  let (oldX, oldY) = lastPos hero in drawEntity $ tiles!!(oldX * (height + 1) + oldY)
  input <- getInput
  case input of
    Exit -> handleExit
    _    -> handleDir world input
  where (oldX, oldY) = pos hero

-- when the user wants to exit we give them a thank you
-- message and then reshow the cursor
handleExit = do
  clearScreen
  setCursorPosition 0 0
  showCursor
  putStrLn "Thank you for playing!"

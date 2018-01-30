module TurtleGraphics 
  ( newTurtle, unturtle
  , lower, raise, getPen
  , forward, turn
  , trail, clearTrail, trailToXYs
  , push, pop
  , getX, setX
  , getY, setY
  , getDir, setDir
  , runCommands
  , Turtle
  , Point
  ) where

import Data.List

type Point       = (Float, Float)
type TurtleStack = [(Point, Float)]
type Trail       = [(Point, Float, Bool)]
data Turtle = Turtle { tPos   :: Point     -- (X,Y) Point
                     , tDir   :: Float           -- Angle in radians
                     , tPen   :: Bool            -- True: pen down / False: pen up
                     , tTrail :: Trail         -- Trail drawn by the turtle
                     , tStack :: TurtleStack } -- (Position, Angle) LIFO-stack
                     deriving (Eq)

instance Show Turtle where
  show (Turtle pos dir pen t s) = "Turtle " ++ show pos ++ " "
                                            ++ show dir ++ " "
                                            ++ show pen 

-- Create a new turtle with the pen down
newTurtle :: Point -> Float -> Bool -> Turtle
newTurtle c d p = Turtle { tPos = c
                         , tDir = d
                         , tPen = p
                         , tTrail = []
                         , tStack = [] }

-- Get turtle's current state
unturtle :: Turtle -> (Point, Float, Bool)
unturtle (Turtle c d p _ _) = (c, d, p)

-- Lower the pen
lower :: Turtle -> Turtle
lower tr = let tr' = updateTrail tr
           in tr'{tPen = True}

-- Raise the pen
raise :: Turtle -> Turtle
raise tr = let tr' = updateTrail tr
           in tr' {tPen = False}

-- Get pen's current state
getPen :: Turtle -> Bool
getPen = tPen

-- Add current current state to turtle's trail
updateTrail :: Turtle -> Turtle
updateTrail tr = tr {tTrail = s : (tTrail tr)}
  where s = unturtle tr

-- Retrive trail containing current state
trail :: Turtle -> [(Point, Float, Bool)]
trail = tTrail . updateTrail

-- Trail to XY
trailToXYs :: [(Point, Float, Bool)] -> [Point]
trailToXYs = map (\(c,_,_) -> c)

-- Delete sections of trail which has a certain pen state
clearTrail :: Bool
           -> [(Point, Float, Bool)] 
           -> [[(Point, Float, Bool)]]
clearTrail b t = goClear $ groupBy (\(_,_,p) (_,_,p') -> p == p') t
  where goClear [] = []
        goClear (x:xs)
          | (\(_,_,p) -> p) (head x) == b = goClear xs
          | otherwise                     = x : goClear xs

-- Save current positon and direction into the record stack
push :: Turtle -> Turtle
push (Turtle c d p t s) = Turtle c d p t ((c,d):s)

-- Retrive and update postion and direction from the record stack
pop :: Turtle -> Turtle
pop (Turtle _ _ _ _ [])        = error "TrutleGraphics.pop: stack is empty"
pop (Turtle _ _ p t ((c,d):s)) = Turtle c d p t s

-- Retrive current X coordinate
getX :: Turtle -> Float
getX (Turtle (x,_) _ _ _ _) = x

-- Retrive current Y coordinate
getY :: Turtle -> Float
getY (Turtle (_,y) _ _ _ _) = y

-- Overwrite turtle's X coordinate
setX :: Float -> Turtle -> Turtle
setX x tr = let y = getY tr in tr {tPos = (x,y)}

-- Overwrite turtle's Y coordinate
setY :: Float -> Turtle -> Turtle
setY y tr = let x = getX tr in tr {tPos = (x,y)} 

-- Retrive current direction
getDir :: Turtle -> Float
getDir (Turtle _ d _ _ _) = d

-- Overwrite turtle's direction
setDir :: Float -> Turtle -> Turtle
setDir d tr = tr {tDir = d}

-- Move turtle forward in current direction
forward :: Float -> Turtle -> Turtle
forward l tr = Turtle (x', y') d p t s
  where (Turtle (x,y) d p t s) = updateTrail tr
        (x', y') = (x + l * cos (d/180*pi), y + l * sin (d/180*pi))

-- Add angle to current direction
turn :: Float -> Turtle -> Turtle
turn a tr = let d = getDir tr in tr {tDir = a + d}

-- Run a list of commands
runCommands :: [Turtle -> Turtle] -> Turtle -> Turtle
runCommands cs = foldl (flip (.)) id cs
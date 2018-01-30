import Graphics.TurtleGraphics
import Graphics.Gloss
import Data.List

{-
     L-system definition:
     Alphabet: "01+-[]"
        -- 0: leaf          
        -- 1: trunk
        -- +: branch left
         -: branch right
         [: push position to stack
         ]: raise pen,pop position from stack, and lower pen
     Axiom:    0
     Rules:    0 -> 11[+0]-0
               1 -> 11
-}

type Rule = (Char, String)

applyRules :: [Rule] -> String -> String
applyRules _  [] = []
applyRules rs (c:ss) =
  case lookup c rs of
    Just b  -> b ++ applyRules rs ss
    Nothing -> c :  applyRules rs ss

generateSentence :: [Rule] -> Int -> String -> String
generateSentence rs depth a = foldr applyRules a (replicate depth rs)

axiom = "0"
rules = [ ('0',"11[+0]-0")
        , ('1',"11") ]

charToCommand :: Char -> (Turtle -> Turtle)
charToCommand '0' = forward 50
charToCommand '1' = forward 50
charToCommand '[' = push
charToCommand ']' = lower . pop . raise
charToCommand '+' = turn 30
charToCommand '-' = turn (-30)

turtle       = newTurtle (0,0) 90 True
treeSentence = generateSentence rules 2 axiom
treeCommands = map charToCommand treeSentence
treeTrail    = map trailToXYs $ clearTrail False
             $ trail $ runCommands treeCommands turtle
glossPicture = translate 0 (-300) $ scale s s $ pictures $ map line treeTrail
    where s = (600/) $ foldr (\(_,y) y' -> y `max` y') 0 $ concat treeTrail

main =
  display (InWindow "Fractal tree" (600,600) (40,40)) white glossPicture
import Graphics.TurtleGraphics
import Graphics.Gloss

{-
L-system: F+-
  'F': go one step forward
  '+': turn right by 90 degrees
  '-': turn left by 90 degrees
Axiom: "F--F--F"
Rule: 'F' -> "F+F--F+F"
-}

type Rule = (Char, String)
type Sentence = String

axiom = "F--F--F"
rules = [('F',"F+F--F+F")]

applyRules :: [Rule] -> Sentence -> Sentence
applyRules _  [] = []
applyRules rs (c:ss) =
  case lookup c rs of
    Just b  -> b ++ applyRules rs ss
    Nothing -> c :  applyRules rs ss

generateSentence :: [Rule] -> Int -> Sentence -> Sentence
generateSentence rs depth a = foldr applyRules a (replicate depth rs)

charToCommand :: Char -> (Turtle -> Turtle)
charToCommand 'F' = forward 50
charToCommand '+' = turn 60 
charToCommand '-' = turn (-60)

turtle       = newTurtle (0,0) 0 True
kochSentence = generateSentence rules 5 axiom
kochCommands = map charToCommand kochSentence
kochTrail    = trailToXYs $ trail
             $ runCommands kochCommands turtle
             
glossPicture = translate (-(maxX-minX)*s/2) (s*maxY) $ scale s s
             $ line kochTrail
  where maxX = foldr (\(x,_) x' -> x `max` x') 0 $ kochTrail
        minX = foldr (\(x,_) x' -> x `min` x') 0 $ kochTrail
        maxY = foldr (\(_,y) y' -> y `max` y') 0 $ kochTrail
        minY = foldr (\(_,y) y' -> y `min` y') 0 $ kochTrail
        s    = (600/) $ max (maxX - minX) (maxY - minY)

main =
  display (InWindow "Koch curve" (600,600) (40,40)) white glossPicture

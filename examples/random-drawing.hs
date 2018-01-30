import Graphics.TurtleGraphics
import Graphics.Gloss
import System.Random

commands = [ forward 50
           , turn 40
           , (forward 10) . (turn 15) . (forward 10) 
                          . (turn 15) . (forward 10) 
                          . (turn 15) . (forward 40)
           , raise
           , lower
           , turn (-60) ]

sequenceCommands :: [a] -> [Int] -> [a]
sequenceCommands _  []     = []
sequenceCommands cs (n:ns) = cs !! ((abs n) `mod` (length cs))
                           : sequenceCommands cs ns

turtle          = newTurtle (0,0) 90 True
randomTrail n g = clearTrail False $ trail $
                  flip runCommands turtle $
                  sequenceCommands commands $
                  take n $ randoms g
randomColors a b c = map ($1) $ zipWith3 makeColor (rnd a) (rnd b) (rnd c)
  where rnd = randomRs (0,1) :: StdGen -> [Float]

glossPicture t c1 c2 c3 = pictures $ zipWith color (randomColors c1 c2 c3) 
                        $ map line $ map trailToXYs $ randomTrail 10000 t

main = do [t, c1, c2, c3] <- sequence (replicate 4 newStdGen)
          display (InWindow "Random drawings" (600,600) (40, 40)) 
                  white $ glossPicture t c1 c2 c3
            
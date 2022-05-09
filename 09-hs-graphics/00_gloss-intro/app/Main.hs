module Main where

import Graphics.Gloss

main :: IO ()
main = main5

-- Funkcije za prikaz: 
--  display
--  animate
--  simulate
--  game
--
-- Display - FullScreen ili InWindow
-- Color   - black, white, ...
-- Picture 
--
-- scale, color, ...

main1 = display FullScreen white (Circle 100)

windowTitle = "test"
windowSize  = (400,400)
windowPos   = (10,10)
windowDisplay = InWindow windowTitle windowSize windowPos

main2 = display windowDisplay white (Circle 100)

main3 = display windowDisplay black (color white $ Circle 100)

main4 = display
    windowDisplay
    black
    $ pictures [ color white $ Circle 100
               , color red   $ Polygon [(0,0), (100,150), (120,120)]
               ]

update :: Float -> Picture
update t = scale 100 100 $ color white $ Circle $ sin t

main5 = animate windowDisplay black update



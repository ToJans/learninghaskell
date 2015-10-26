import Graphics.Gloss
import Game
import Draw


main :: IO ()
main = myPlay initLevel eventHandler timeStepHandler

import           Engine
import           Game

main :: IO ()
main = runEngine initLevel eventHandler timeStepHandler

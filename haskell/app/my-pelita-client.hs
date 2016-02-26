import Control.Monad.State
import Control.Comonad
import System.Random
import Debug.Trace
import PelitaClient

data MyPlayer = MyPlayer Int StdGen deriving (Show)

directions = [(1, 0), (0, 1), (-1, 0), (0, -1)]

traceState = modify (\s -> trace (show s) s)

adjacent :: (Int, Int) -> [(Int, Int)]
adjacent (x, y) = do
    (dx, dy) <- [(-1, 0), (0, -1), (0, 0), (1, 0), (0, 1)]
    return (x + dx, y + dy)

-- instance Comonad Maze

instance Player MyPlayer where
  teamName = return "My Team"
  setInitial universe gameState = return ()
  getMove universe gameState = traceState >> do
    (MyPlayer i rnd) <- get
    let (rndIdx, newRnd) = randomR (0, 3) rnd
    put $ MyPlayer (i + 1) (trace (show rndIdx) newRnd)
    let (Universe maze food) = universe

    let direction = directions !! rndIdx
    return $ trace (show i) $ trace (show universe) direction

main :: IO ()
main = evalStateT withPelita (MyPlayer 0 (mkStdGen 0))

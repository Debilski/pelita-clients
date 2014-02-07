import Control.Monad.State
import Control.Comonad
import System.Random
import Debug.Trace
import PelitaClient

data MyPlayer = MyPlayer Int StdGen deriving (Show)

traceState = modify (\s -> trace (show s) s)

adjacent :: (Int, Int) -> [(Int, Int)]
adjacent (x, y) = do
    (dx, dy) <- [(-1, 0), (0, -1), (0, 0), (1, 0), (0, 1)]
    return (x + dx, y + dy)

-- instance Comonad Maze

instance Player MyPlayer where
  setInitial universe gameState = return ()
  getMove universe gameState = traceState >> do
    (MyPlayer i rnd) <- get
    let (newInt, newRnd) = next rnd
    put $ MyPlayer (i + 1) (trace (show newInt) newRnd)
    let (Universe maze) = universe
    
    return $ trace (show i) $ trace (show universe) (0, 0)

main = withPelita "My Team" $ MyPlayer 0 (mkStdGen 0)


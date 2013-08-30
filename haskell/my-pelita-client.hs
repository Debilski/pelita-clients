
import Debug.Trace
import PelitaClient

data MyPlayer = MyPlayer

instance Player MyPlayer where
  setInitial universe gameState = return ()
  getMove universe gameState = return (0, 0)

main = withPelita "My Team" MyPlayer


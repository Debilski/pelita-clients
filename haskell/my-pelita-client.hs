
import Debug.Trace
import PelitaClient

data MyPlayer = MyPlayer
instance Player MyPlayer where
  setInitial p universe gameState = trace "initial" ((), p)
  getMove p universe gameState = trace "move" ((0, 0), p)

main = withPelita "My Team" MyPlayer


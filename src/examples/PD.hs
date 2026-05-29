module PD where

import Bimatrix
import Game
import Listable

-- Prisoner's dilemma moves
data MovesPD = Cooperate | Defect
  deriving (Eq, Show)

instance Listable MovesPD where
  allValues :: [MovesPD]
  allValues = [Cooperate, Defect]

payoffPD :: Bimatrix MovesPD MovesPD
payoffPD (Cooperate, Cooperate) = (2, 2)
payoffPD (Cooperate, Defect) = (0, 3)
payoffPD (Defect, Cooperate) = (3, 0)
payoffPD (Defect, Defect) = (1, 1)

gamePD :: Game (MovesPD, MovesPD) (MovesPD, MovesPD) TwoDoubles () () (MovesPD, MovesPD) TwoDoubles
gamePD = bimatrixGame payoffPD

equilibriaPD :: [(MovesPD, MovesPD)]
equilibriaPD = equilibria gamePD (bimatrixContext payoffPD)

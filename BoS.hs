module BoS where

import Bimatrix
import Game
import Listable

-- Back or Stravinsky moves
data MovesBoS = Bach | Stravinsky
  deriving (Eq, Show)

instance Listable MovesBoS where
  allValues :: [MovesBoS]
  allValues = [Bach, Stravinsky]

payoffBoS :: Bimatrix MovesBoS MovesBoS
payoffBoS (Bach, Bach) = (3, 2)
payoffBoS (Bach, Stravinsky) = (1, 1)
payoffBoS (Stravinsky, Bach) = (0, 0)
payoffBoS (Stravinsky, Stravinsky) = (2, 3)

gameBoS :: Game (MovesBoS, MovesBoS) (MovesBoS, MovesBoS) TwoDoubles () () (MovesBoS, MovesBoS) TwoDoubles
gameBoS = bimatrixGame payoffBoS

equilibriaBoS :: [(MovesBoS, MovesBoS)]
equilibriaBoS = equilibria gameBoS (bimatrixContext payoffBoS)
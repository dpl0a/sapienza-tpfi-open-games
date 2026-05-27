module Bimatrix where

import Game
import Listable
import Optics

-- The type of a pair of doubles with the right order structure
type TwoDoubles = (Double, Double)

instance {-# OVERLAPPING #-} Ord TwoDoubles where
  (<=) :: TwoDoubles -> TwoDoubles -> Bool
  (x, y) <= (x', y') = x <= x' && y <= y'
  (<) :: TwoDoubles -> TwoDoubles -> Bool
  (x, y) < (x', y') = x < x' && y < y'

-- Helpers for bimatrix games
type Bimatrix moves1 moves2 = (moves1, moves2) -> TwoDoubles

bimatrixArena :: Arena (moves1, moves2) TwoDoubles () () (moves1, moves2) TwoDoubles
bimatrixArena = normL >->> (corner #### corner)

bimatrixContext :: Bimatrix moves1 moves2 -> Context () () (moves1, moves2) TwoDoubles
bimatrixContext b = MkContext (scalarToState ()) (funToCostate b)

bimatrixGame :: (Listable moves1, Listable moves2) => Bimatrix moves1 moves2 -> Game (moves1, moves2) (moves1, moves2) TwoDoubles () () (moves1, moves2) TwoDoubles
bimatrixGame bimatrix = MkGame (argmaxPlayer ## argmaxPlayer) bimatrixArena

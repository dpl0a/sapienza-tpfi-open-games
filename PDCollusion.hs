module PDCollusion where

import Bimatrix
import Game
import PD

gamePDCollusion :: Game (MovesPD, MovesPD) (MovesPD, MovesPD) TwoDoubles () () (MovesPD, MovesPD) TwoDoubles
gamePDCollusion = MkGame argmaxPlayer bimatrixArena

equilibriaPDCollusion :: [(MovesPD, MovesPD)]
equilibriaPDCollusion = equilibria gamePDCollusion (bimatrixContext payoffPD)

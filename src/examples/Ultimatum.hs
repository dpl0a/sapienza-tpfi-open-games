module Ultimatum where

import Game
import Listable
import Optics

data MovesUltimatum1 = Fair | Unfair
  deriving (Eq, Show)

instance Listable MovesUltimatum1 where
  allValues :: [MovesUltimatum1]
  allValues = [Fair, Unfair]

data MovesUltimatum2 = Accept | Reject
  deriving (Eq, Show)

instance Listable MovesUltimatum2 where
  allValues :: [MovesUltimatum2]
  allValues = [Accept, Reject]

payoffUltimatum :: (MovesUltimatum1, MovesUltimatum2) -> (Double, Double)
payoffUltimatum (Fair, Accept) = (5, 5)
payoffUltimatum (Fair, Reject) = (0, 0)
payoffUltimatum (Unfair, Accept) = (8, 2)
payoffUltimatum (Unfair, Reject) = (0, 0)

firstStage :: ParaLens MovesUltimatum1 Double () () MovesUltimatum1 Double
firstStage = corner

interlude :: Lens x r (x, x) ((), r)
interlude = nonPara (\x -> (x, x)) (\_ ((), r) -> r)

secondStage :: ParaLens (MovesUltimatum1 -> MovesUltimatum2) Double MovesUltimatum1 () MovesUltimatum2 Double
secondStage = MkLens (\f x -> f x) (\_ _ r -> ((), r))

arenaUltimatum :: ParaLens (MovesUltimatum1, MovesUltimatum1 -> MovesUltimatum2) (Double, Double) () () (MovesUltimatum1, MovesUltimatum2) (Double, Double)
arenaUltimatum = (firstStage >>-> interlude) >>>> (secondStage ##-# idLens) >>-> exchange

contextUltimatum :: Context () s (MovesUltimatum1, MovesUltimatum2) (Double, Double)
contextUltimatum = MkContext (scalarToState ()) (funToCostate payoffUltimatum)

gameUltimatum :: Game (MovesUltimatum1, MovesUltimatum1 -> MovesUltimatum2) (MovesUltimatum1, MovesUltimatum1 -> MovesUltimatum2) (Double, Double) () () (MovesUltimatum1, MovesUltimatum2) (Double, Double)
gameUltimatum = MkGame (argmaxPlayer ## argmaxPlayer) arenaUltimatum

equilibriaUltimatum :: [(MovesUltimatum1, MovesUltimatum1 -> MovesUltimatum2)]
equilibriaUltimatum = equilibria gameUltimatum contextUltimatum

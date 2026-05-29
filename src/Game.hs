module Game where

import Listable
import Optics

-- Player
type Player profiles utility actions = Lens profiles (profiles -> Bool) actions (actions -> utility)

-- Argmax selection function
argmax :: (Ord a, Eq a, Listable x) => (x -> a) -> (x -> Bool)
argmax k x = k x == maximum (map k allValues)

-- A player adopting the argmax selection function (Nash)
--                                           Lens x (x -> Bool) x (x -> a)
argmaxPlayer :: (Ord u, Eq u, Listable s) => Player s u s
argmaxPlayer = nonPara id (\p k -> argmax k)

-- A player playing epsilon-Nash
epsilonPlayer :: (Ord u, Num u, Listable s) => u -> Player s u s
epsilonPlayer epsilon = nonPara 
  id 
  (\_ k -> \action -> 
      let maxUtility = maximum (map k allValues)
       in k action >= maxUtility - epsilon
  )

-- A player with an utility satisfaction treshold (Bounded Rationality)
satisficingPlayer :: (Ord u) => u -> Player s u s
satisficingPlayer tau = nonPara 
  id 
  (\_ k -> \action -> k action >= tau)

-- A stubborn/byzantine player that always plays the same move
stubbornPlayer :: (Eq s) => s -> Player s u s
stubbornPlayer targetAction = nonPara 
  id 
  (\_ k -> \action -> action == targetAction)

-- An altruistic player playing for social optimum (Pareto)
altruisticPlayer :: (Ord u, Num u, Listable s) => Player s (u, u) s
altruisticPlayer = nonPara 
  id 
  (\_ k -> \action -> 
      let 
          socialUtility a = myU + theirU where (myU, theirU) = k a 
          maxSocialUtility = maximum (map socialUtility allValues)
       in socialUtility action == maxSocialUtility
  )

-- Oplaxator, basically the equilibrium AND-gate
oplaxator :: Lens (x, x') ((x, x') -> Bool) (x, x') (x -> Bool, x' -> Bool)
oplaxator = nonPara id (\_ (phi, psi) (x, x') -> phi x && psi x')

-- Product of two players
infixr 4 ##

(##) :: Player s u a -> Player s' u' a' -> Player (s, s') (u, u') (a, a')
player ## player' = oplaxator >--> (player #--# player') >--> nashator

-- Arena
type Arena actions utility states copayoffs moves payoffs = ParaLens actions utility states copayoffs moves payoffs

-- These two lenses get rid of spurious ()
runitor :: Lens () (() -> ()) () ()
runitor = nonPara id (const const)

lunitor :: Lens () () () (() -> ())
lunitor = nonPara id const

-- Game
data Game profiles actions utility states copayoffs moves payoffs where
  MkGame ::
    Player profiles utility actions ->
    Arena actions utility states copayoffs moves payoffs ->
    Game profiles actions utility states copayoffs moves payoffs

-- Context
data Context x s y r where
  MkContext :: State x s -> CoState y r -> Context x s y r

-- Pairs an arena with a context, by first differentiating the first and then composing them
packup :: Arena a u x s y r -> Context x s y r -> ParaLens a (a -> u) () () () ()
packup arena (MkContext x k) = lunitor >->> paraRdiff (x >->> arena >>-> k) >>-> runitor

-- Find fixpoints for a selection function in a listable type
fixpoints :: (Listable x) => (x -> x -> Bool) -> [x]
fixpoints f = filter (\x -> f x x) allValues

-- Computes the equilibria of a game in a given context
equilibria :: (Listable profiles) => Game profiles actions utility states copayoffs moves payoffs -> Context states copayoffs moves payoffs -> [profiles]
equilibria (MkGame player arena) ctx =
    let game = player *** packup arena ctx in fixpoints (paraStateTofun game)

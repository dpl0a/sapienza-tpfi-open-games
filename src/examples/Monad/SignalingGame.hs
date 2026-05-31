module SignalingGame where

import MonadOptics
import Listable
import qualified Numeric.Probability.Distribution as Dist

data Type = High | Low deriving (Eq, Ord, Show)
data Signal = Costly | Cheap deriving (Eq, Ord, Show)
data Action = Hire | Reject deriving (Eq, Ord, Show)

instance Listable Type where allValues = [High, Low]
instance Listable Signal where allValues = [Costly, Cheap]
instance Listable Action where allValues = [Hire, Reject]

senderArena :: Monad m => ParaMonadOptic m (Type -> Signal) Double Type () (Type, Signal) Double
senderArena = MkOptic
  (\strat trueType -> return ((), (trueType, strat trueType)))
  (\strat () uS -> return ((), uS))

receiverArena :: Monad m => ParaMonadOptic m (Signal -> Action) Double (Type, Signal) Double (Type, Signal, Action) (Double, Double)
receiverArena = MkOptic
  (\strat (trueType, sig) -> return ((), (trueType, sig, strat sig)))
  (\strat () (uS, uR) -> return (uS, uR))

gameArena :: Monad m => ParaMonadOptic m (Type -> Signal, Signal -> Action) (Double, Double) Type () (Type, Signal, Action) (Double, Double)
gameArena = senderArena >>>> receiverArena

prior :: Dist.T Double (Type, Type)
prior = Dist.uniform [(High, High), (Low, Low)]

payoffs :: Type -> (Type, Signal, Action) -> Dist.T Double (Double, Double)
payoffs trueType (_, sig, act) = return (uS, uR)
  where
    uR = case (trueType, act) of
           (High, Hire)   -> 1.0
           (Low, Reject)  -> 1.0
           _              -> 0.0

    cost = case (trueType, sig) of
             (High, Costly) -> 0.2
             (Low, Costly)  -> 1.0
             (_, Cheap)     -> 0.0

    reward = case act of
               Hire   -> 1.0
               Reject -> 0.0

    uS = reward - cost

players :: (Monad m, MonadEvaluate m Double) 
        => MonadPlayer m (Type -> Signal, Signal -> Action) (Double, Double) (Type -> Signal, Signal -> Action)
players = argmaxPlayer ## argmaxPlayer

gameArenaDiff :: ParaMonadOptic   (Dist.T Double)   (Type -> Signal, Signal -> Action)   ((Type -> Signal, Signal -> Action)    -> Dist.T Double (Double, Double))   Type   (Type -> Dist.T Double ())   (Type, Signal, Action)   ((Type, Signal, Action) -> Dist.T Double (Double, Double))
gameArenaDiff = paraRDiff gameArena

equilibriaExAnte :: [(Type -> Signal, Signal -> Action)]
equilibriaExAnte = solveGame players (plugExAnte prior payoffs gameArena)

equilibriaExPost :: [(Type -> Signal, Signal -> Action)]
equilibriaExPost = solveGame players (plugExPost prior payoffs gameArenaDiff)

equilibriaInterim :: [(Type -> Signal, Signal -> Action)]
equilibriaInterim = solveGame players (plugInterim prior payoffs gameArenaDiff)
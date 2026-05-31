module Monad.SignalingGame where

import MonadOptics
import Listable
import qualified Numeric.Probability.Distribution as Dist

data Type = High | Low deriving (Eq, Ord, Show)
data Signal = Costly | Cheap deriving (Eq, Ord, Show)
data Action = Hire | Reject deriving (Eq, Ord, Show)

instance Listable Type where allValues = [High, Low]
instance Listable Signal where allValues = [Costly, Cheap]
instance Listable Action where allValues = [Hire, Reject]

-- 1. Il Sender riceve il tipo, sceglie il segnale e passa la coppia (Type, Signal) a valle.
--    Nel backward pass, propaga semplicemente il payoff Double ricevuto dall'alto.
senderArena :: Monad m => ParaMonadOptic m (Type -> Signal) Double Type () (Type, Signal) Double
senderArena = MkOptic
  (\strat trueType -> do
      let sig = strat trueType
      return ((), (trueType, sig))
  )
  (\strat () uS -> return ((), uS))

-- 2. Arena unica per il Receiver. La sua strategia p' è la funzione (Signal -> Action).
--    Questo forza la valutazione sequenziale su TUTTI i segnali del dominio, on-path e off-path.
receiverArena :: Monad m => ParaMonadOptic m (Signal -> Action) Double (Type, Signal) Double (Type, Signal, Action) (Double, Double)
receiverArena = MkOptic
  (\strat (trueType, sig) -> do
      let act = strat sig
      return ((), (trueType, sig, act))
  )
  (\strat () (uS, uR) -> return (uS, uR))

-- 3. Composizione sequenziale diretta tramite >>>>. 
--    Non serve più collapseOptic perché i tipi combaciano nativamente.
gameArena :: Monad m => ParaMonadOptic m 
  (Type -> Signal, Signal -> Action)     
  (Double, Double)             
  Type                                   
  () 
  (Type, Signal, Action)                 
  (Double, Double)                       
gameArena = senderArena >>>> receiverArena

-- 4. Composizione dei giocatori: ora abbiamo 2 macro-agenti (Sender e Receiver).
--    La tupla dei profili diventa (Type -> Signal, Signal -> Action).
players :: (Monad m, MonadEvaluate m Double) 
        => MonadPlayer m (Type -> Signal, Signal -> Action) 
                         (Double, Double) 
                         (Type -> Signal, Signal -> Action)
players = argmaxPlayer ## argmaxPlayer

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

getSignal :: (Type, Signal, Action) -> Signal
getSignal (_, sig, _) = sig

offPathBeliefs :: Signal -> Dist.T Double Type
offPathBeliefs _ = return Low

mergePayoffs :: (Double, Double) -> (Double, Double) -> (Double, Double)
mergePayoffs (uS_actual, _uR_actual) (_uS_post, uR_post) = (uS_actual, uR_post)

gameArenaDiff :: ParaMonadOptic (Dist.T Double) 
  (Type -> Signal, Signal -> Action) 
  ((Type -> Signal, Signal -> Action) -> Dist.T Double (Double, Double)) 
  Type 
  (Type -> Dist.T Double ()) 
  (Type, Signal, Action) 
  ((Type, Signal, Action) -> Dist.T Double (Double, Double))
gameArenaDiff = paraRDiff gameArena

equilibriaExAnte :: [(Type -> Signal, Signal -> Action)]
equilibriaExAnte = solveGame players (plugExAnte prior payoffs gameArena)

equilibriaExPost :: [(Type -> Signal, Signal -> Action)]
equilibriaExPost = solveGame players (plugExPost prior payoffs gameArenaDiff)

equilibriaInterim :: [(Type -> Signal, Signal -> Action)]
equilibriaInterim = solveGame players (plugInterim prior offPathBeliefs getSignal mergePayoffs payoffs gameArenaDiff)
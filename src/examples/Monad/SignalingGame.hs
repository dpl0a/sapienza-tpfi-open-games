module SignalingGame where

import MonadOptics
import Listable
import qualified Numeric.Probability.Distribution as Dist

-- 1. Dominio
data Type = High | Low deriving (Eq, Ord, Show)
data Signal = Costly | Cheap deriving (Eq, Ord, Show)
data Action = Hire | Reject deriving (Eq, Ord, Show)

instance Listable Type where allValues = [High, Low]
instance Listable Signal where allValues = [Costly, Cheap]
instance Listable Action where allValues = [Hire, Reject]

-- 2. Arene Locali 
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

-- 3. Natura e Payoff
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

-- 4. Chiusura Universale Ex-Ante (La grande differenza)
-- Creiamo l'universo chiuso inserendo il filo di theta senza usare alcun Context.
closedSignalingArena :: ParaMonadOptic (Dist.T Double) (Type -> Signal, Signal -> Action) (Double, Double) () () () ()
closedSignalingArena = pureClosedGame prior gameArena payoffs

-- 5. Giocatori
players :: MonadPlayer (Dist.T Double) (Type -> Signal, Signal -> Action) (Double, Double) (Type -> Signal, Signal -> Action)
players = argmaxPlayer ## argmaxPlayer

-- 6. Calcolo degli Equilibri
pureEquilibria :: [ (Type -> Signal, Signal -> Action) ]
pureEquilibria = 
    let 
        -- 1. Differenziamo l'universo globalmente chiuso
        diffUniverse = paraRDiff closedSignalingArena
        
        -- 2. Applichiamo i giocatori (questo trasforma i payoff q = m r in q = Bool)
        composedGame = players *** diffUniverse
        
        -- 3. Estraiamo l'oracolo di selezione (profilo -> m (profilo -> Bool))
        selectionFunc = runClosedGame composedGame
        
    in filter (\p -> evaluate $ do
           isEqPredicate <- selectionFunc p
           return (isEqPredicate p)
       ) allValues
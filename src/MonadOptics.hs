{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MonadOptics where

import Control.Monad.Identity
import qualified Control.Monad.Bayes.Class as Bayes
import qualified Control.Monad.Bayes.Enumerator as BayesEnum
import qualified Numeric.Probability.Distribution as Dist
import Listable

data ParaMonadOptic m p q x s y r where
  MkOptic :: (p -> x -> m (w, y)) -> (p -> w -> r -> m (s, q)) -> ParaMonadOptic m p q x s y r

type MonadOptic m x s y r = ParaMonadOptic m () () x s y r

monadNonPara :: (Monad m) => (x -> m y) -> (x -> r -> m s) -> MonadOptic m x s y r
monadNonPara play coplay = MkOptic
  ( \() x -> do
      y <- play x
      return (x, y)
  )
  ( \() x r -> do
      s <- coplay x r
      return (s, ())
  )

(>>>>) :: (Monad m) => ParaMonadOptic m p q x s y r -> ParaMonadOptic m p' q' y r z t -> ParaMonadOptic m (p, p') (q, q') x s z t
(MkOptic play coplay) >>>> (MkOptic play' coplay') =
  MkOptic
    ( \(p, p') x -> do
        (w, y) <- play p x
        (w', z) <- play' p' y
        return ((w, w'), z)
    )
    ( \(p, p') (w, w') t -> do
        (r, q') <- coplay' p' w' t
        (s, q) <- coplay p w r
        return (s, (q, q'))
    )

infixr 4 >-->
(>-->) :: (Monad m) => MonadOptic m x s y r -> MonadOptic m y r z t -> MonadOptic m x s z t
ll >--> lr = monadNormL *** (ll >>>> lr)

infixr 4 >->>
(>->>) :: (Monad m) => MonadOptic m x s y r -> ParaMonadOptic m p q y r z t -> ParaMonadOptic m p q x s z t
l >->> pl = monadNormL *** (l >>>> pl)

infixr 4 >>->
(>>->) :: (Monad m) => ParaMonadOptic m p q x s y r -> MonadOptic m y r z t -> ParaMonadOptic m p q x s z t
pl >>-> l = monadNormR *** (pl >>>> l)

(####) :: (Monad m) => ParaMonadOptic m p q x s y r -> ParaMonadOptic m p' q' x' s' y' r' -> ParaMonadOptic m (p, p') (q, q') (x, x') (s, s') (y, y') (r, r')
(MkOptic play coplay) #### (MkOptic play' coplay') =
  MkOptic
    ( \(p, p') (x, x') -> do
        (w, y) <- play p x
        (w', y') <- play' p' x'
        return ((w, w'), (y, y'))
    )
    ( \(p, p') (w, w') (r, r') -> do
        (s, q) <- coplay p w r
        (s', q') <- coplay' p' w' r'
        return ((s, s'), (q, q'))
    )

infixr 4 #--#
(#--#) :: (Monad m) => MonadOptic m x s y r -> MonadOptic m x' s' y' r' -> MonadOptic m (x, x') (s, s') (y, y') (r, r')
ll #--# lr = monadNormL *** (ll #### lr)

infixr 4 #-##
(#-##) :: (Monad m) => MonadOptic m x s y r -> ParaMonadOptic m p q x' s' y' r' -> ParaMonadOptic m p q (x, x') (s, s') (y, y') (r, r')
l #-## pl = monadNormL *** (l #### pl)

infixr 4 ##-#
(##-#) :: (Monad m) => ParaMonadOptic m p q x s y r -> MonadOptic m x' s' y' r' -> ParaMonadOptic m p q (x, x') (s, s') (y, y') (r, r')
pl ##-# l = monadNormR *** (pl #### l)

(***) :: (Monad m) => MonadOptic m p' q' p q -> ParaMonadOptic m p q x s y r -> ParaMonadOptic m p' q' x s y r
(MkOptic play coplay) *** (MkOptic play' coplay') =
  MkOptic
    ( \p' x -> do
        (_, p) <- play () p'
        play' p x
    )
    ( \p' w r -> do
        (wint, p) <- play () p'
        (s, q) <- coplay' p w r
        (q', _) <- coplay () wint q
        return (s, q')
    )

monadNormL :: (Monad m) => MonadOptic m p q ((), p) ((), q)
monadNormL = monadNonPara 
  (\p -> return ((), p)) 
  (\_ (_, q) -> return q)

monadNormR :: (Monad m) => MonadOptic m p q (p, ()) (q, ())
monadNormR = monadNonPara 
  (\p -> return (p, ())) 
  (\_ (q, _) -> return q)

paraRDiff :: (Monad m) => ParaMonadOptic m p q x s y r -> ParaMonadOptic m p (p -> m q) x (x -> m s) y (y -> m r)
paraRDiff (MkOptic play coplay) =
  MkOptic
    ( \p x -> do
        (w, y) <- play p x
        return ((w, x), y)
    )
    ( \p (w, x) oracle -> do
        let evalPast x' = do
              (w_sim, y_sim) <- play p x'
              r_sim <- oracle y_sim
              (s_sim, _) <- coplay p w_sim r_sim
              return s_sim
        let evalPolicy p' = do
              (w_sim, y_sim) <- play p' x
              r_sim <- oracle y_sim
              (_, q_sim) <- coplay p' w_sim r_sim
              return q_sim
        return (evalPast, evalPolicy)
    )

monadNashator :: (Monad m) => MonadOptic m (x, x') (x -> m s, x' -> m s') (x, x') ((x, x') -> m (s, s'))
monadNashator =
  monadNonPara
    return
    ( \(x0, x0') f -> return $
        let k1 x  = fmap fst (f (x, x0'))
            k2 x' = fmap snd (f (x0, x'))
         in (k1, k2)
    )

monadOplaxator :: (Monad m) => MonadOptic m (x, x') ((x, x') -> Bool) (x, x') (x -> Bool, x' -> Bool)
monadOplaxator = monadNonPara return (\_ (phi, psi) -> return (\(x, x') -> phi x && psi x'))

data Context m x y r where
  MkContext :: Ord theta => m (theta, x) -> (theta -> y -> m r) -> Context m x y r

class (Monad m) => MonadCondition m where
  condition :: (Eq x, Ord theta) => m (theta, x) -> x -> m theta

-- `m` is the monad, `u` is the utility type
class (Monad m) => MonadEvaluate m u where
  evaluate :: m u -> u

expectedUtility :: (MonadCondition m, MonadEvaluate m u, Eq x) => Context m x y u -> x -> y -> u
expectedUtility (MkContext p k) x y =
  let posterior = condition p x
   in evaluate $ do
        theta <- posterior
        k theta y

instance MonadCondition Identity where
  condition :: (Eq x, Ord theta) => Identity (theta, x) -> x -> Identity theta
  condition (Identity (theta, _)) _ = return theta

instance MonadEvaluate Identity x where
  evaluate :: Identity x -> x
  evaluate (Identity x) = x

instance MonadEvaluate (Dist.T Double) Double where
  evaluate :: Dist.T Double Double -> Double
  evaluate = Dist.expected

instance MonadEvaluate (Dist.T Double) Bool where
  evaluate :: Dist.T Double Bool -> Bool
  evaluate dist = Dist.expected (fmap (\b -> if b then 1.0 else 0.0) dist) > 0.5

instance MonadCondition (Dist.T Double) where
  condition :: (Eq x, Ord theta) => Dist.T Double (theta, x) -> x -> Dist.T Double theta
  condition prior obs = Dist.norm $ Dist.filter (\(theta, x) -> x == obs) prior >>= \(theta, _) -> return theta

instance MonadEvaluate BayesEnum.Enumerator Double where
  evaluate :: BayesEnum.Enumerator Double -> Double
  evaluate dist = 
    let marginals = BayesEnum.enumerate dist
     in sum [ val * prob | (val, prob) <- marginals ]

instance MonadCondition BayesEnum.Enumerator where
  condition :: (Eq x, Ord theta) => BayesEnum.Enumerator (theta, x) -> x -> BayesEnum.Enumerator theta
  condition prior obs = do
    (theta, x) <- prior
    Bayes.condition (x == obs)
    return theta

plugExPost 
  :: Monad m 
  => ParaMonadOptic m p q x s y r
  -> Context m x y r
  -> (p -> m q)
plugExPost (MkOptic play coplay) (MkContext prior continuation) = \p -> do
    (theta, x) <- prior
    (w, y) <- play p x
    r <- continuation theta y
    (s, q) <- coplay p w r
    return q

plugExAnte
  :: (MonadCondition m, Eq x)
  => ParaMonadOptic m p q x s y r
  -> Context m x y r
  -> (p -> m q)
plugExAnte (MkOptic play coplay) (MkContext prior continuation) = \p -> do
    (_, x) <- prior    
    (w, y) <- play p x    
    theta_post <- condition prior x    
    r <- continuation theta_post y    
    (s, q) <- coplay p w r
    return q

-- Takes an already composed arena+player pair (Player *** paraRDiff Arena)
paraPlugExPost
  :: Monad m 
  => ParaMonadOptic m p q x s y (y -> m r)
  -> Context m x y r
  -> (p -> m q)
paraPlugExPost (MkOptic play coplay) (MkContext prior continuation) = \p -> do
    (theta, x) <- prior
    (w, y) <- play p x
    let oracle = continuation theta
    (_, q) <- coplay p w oracle
    return q

paraPlugExAnte
  :: MonadCondition m => Eq x
  => ParaMonadOptic m p q x s y (y -> m r) 
  -> Context m x y r 
  -> (p -> m q)
paraPlugExAnte (MkOptic play coplay) (MkContext prior continuation) = \p -> do
    (_, x) <- prior 
    (w, y) <- play p x
    
    let bayesianOracle y_sim = do
          theta_post <- condition prior x
          continuation theta_post y_sim
          
    (_, q) <- coplay p w bayesianOracle
    return q


-- Il giocatore riceve un profilo, produce un'azione, osserva un'utilità (monadica) e restituisce un Bool (es. è un equilibrio?)
-- Il Player invece si aspetta la funzione di utilità differenziata 
-- (a -> m u) come input nel backward pass
type MonadPlayer m profiles utility actions = MonadOptic m profiles (profiles -> Bool) actions (actions -> m utility)

infixr 4 ##
(##) :: (Monad m) 
     => MonadPlayer m s u a 
     -> MonadPlayer m s' u' a' 
     -> MonadPlayer m (s, s') (u, u') (a, a')
player ## player' = monadOplaxator >--> (player #--# player') >--> monadNashator

-- L'Arena "nuda" non sa nulla di funzioni di utilità: 
-- il parametro 'u' è lo scalare (es. Double)
type MonadArena m actions utility states copayoffs moves payoffs = 
    ParaMonadOptic m actions utility states copayoffs moves payoffs

data MonadGame m profiles actions utility states copayoffs moves payoffs where
  MkMonadGame ::
    MonadPlayer m profiles utility actions ->
    MonadArena m actions utility states copayoffs moves payoffs ->
    MonadGame m profiles actions utility states copayoffs moves payoffs


-- Un giocatore che massimizza l'utilità attesa
argmaxPlayer :: (Monad m, MonadEvaluate m u, Ord u, Eq u, Listable s) => MonadPlayer m s u s
argmaxPlayer = monadNonPara 
  return 
  (\_ k -> return (\action -> 
      let expectedUtility a = evaluate (k a)
          maxUtility = maximum (map expectedUtility allValues)
       in expectedUtility action == maxUtility
  ))


monadPack 
  :: (MonadCondition m, Eq x)
  => MonadGame m p a u x s y r 
  -> Context m x y r 
  -> (p -> m (p -> Bool))
monadPack (MkMonadGame player arena) ctx = 
  let
      diffArena = paraRDiff arena 
      composedGame = player *** diffArena   
   in paraPlugExAnte composedGame ctx

monadEquilibria 
  :: (MonadCondition m, MonadEvaluate m Bool, Eq x, Listable p) 
  => MonadGame m p a u x s y r 
  -> Context m x y r 
  -> [p]
monadEquilibria game ctx =
    let selectionFunc = monadPack game ctx
    in filter (\p -> 
         evaluate $ do
           isEqPredicate <- selectionFunc p
           return (isEqPredicate p)
       ) allValues

-- Il filo di identità per scavalcare l'arena
monadIdOptic :: (Monad m) => MonadOptic m t t t t
monadIdOptic = monadNonPara 
  return 
  (\_ r -> return r)

-- L'ottica della Natura (assorbe il ritorno)
priorOptic :: (Monad m) => m (theta, x) -> MonadOptic m () () (theta, x) (theta, s)
priorOptic prior = monadNonPara 
  (\() -> prior)
  (\() (theta, s) -> return ())

-- L'ottica dei Payoff (rimbalza theta)
payoffOptic :: (Monad m) => (theta -> y -> m r) -> MonadOptic m (theta, y) (theta, r) () ()
payoffOptic k = monadNonPara 
  (\_ -> return ())
  (\(theta, y) () -> do
      r <- k theta y
      return (theta, r)
  )

-- Chiusura pura (Prior + ArenaLiftata + Payoff)
pureClosedGame :: Monad m 
               => m (theta, x) 
               -> ParaMonadOptic m p q x s y r 
               -> (theta -> y -> m r) 
               -> ParaMonadOptic m p q () () () ()
pureClosedGame prior arena k = 
  priorOptic prior >->> (monadIdOptic #-## arena) >>-> payoffOptic k

-- Esecutore di un gioco completamente chiuso e differenziato
runClosedGame :: (Monad m) 
              => ParaMonadOptic m p q () (() -> m ()) () (() -> m ()) 
              -> (p -> m q)
runClosedGame (MkOptic play coplay) p = do
    (w, _) <- play p ()
    (_, q) <- coplay p w (\() -> return ())
    return q
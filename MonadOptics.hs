{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MonadOptics where

import Control.Monad.Identity

data ParaMonadOptic m p q x s y r where
  MkOptic :: (p -> x -> m (w, y)) -> (p -> w -> r -> m (s, q)) -> ParaMonadOptic m p q x s y r

type MonadOptic m x s y r = ParaMonadOptic m () () x s y r

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

data Context m x y r where
  MkContext :: m (theta, x) -> (theta -> y -> m r) -> Context m x y r

class (Monad m) => MonadCondition m where
  condition :: (Eq x) => m (theta, x) -> x -> m theta

-- `m` is the monad, `u` is the utility type
class (Monad m) => MonadEvaluate m u where
  evaluate :: m u -> u

instance MonadCondition Identity where
  condition :: (Eq x) => Identity (theta, x) -> x -> Identity theta
  condition (Identity (theta, _)) _ = return theta

instance MonadEvaluate Identity x where
  evaluate :: Identity x -> x
  evaluate (Identity x) = x

expectedUtility :: (MonadCondition m, MonadEvaluate m u, Eq x) => Context m x y u -> x -> y -> u
expectedUtility (MkContext p k) x y =
  let posterior = condition p x
   in evaluate $ do
        theta <- posterior
        k theta y

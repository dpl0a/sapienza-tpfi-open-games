{-# LANGUAGE GADTs #-}

module Optics where

import Data.Tuple (swap)

data ParaLens p q x s y r where
  MkLens :: (p -> x -> y) -> (p -> x -> r -> (s, q)) -> ParaLens p q x s y r

type Lens = ParaLens () ()

nonPara :: (x -> y) -> (x -> r -> s) -> Lens x s y r
nonPara play coplay = MkLens (\_ x -> play x) (\_ x r -> (coplay x r, ()))

type CoState x s = Lens x s () ()

type State = Lens () ()

-- ParaLens composition
infixr 4 >>>>

(>>>>) :: ParaLens p q x s y r -> ParaLens p' q' y r z t -> ParaLens (p, p') (q, q') x s z t
(MkLens play coplay) >>>> (MkLens play' coplay') =
  MkLens
    (\(p, p') x -> play' p' (play p x))
    ( \(p, p') x t ->
        let (r, q') = coplay' p' (play p x) t
            (s, q) = coplay p x r
         in (s, (q, q'))
    )

-- Non-parametric/mixed compositions
infixr 4 >-->

(>-->) :: Lens x s y r -> Lens y r z t -> Lens x s z t
ll >--> lr = normL *** (ll >>>> lr)

infixr 4 >->>

(>->>) :: Lens x s y r -> ParaLens p q y r z t -> ParaLens p q x s z t
l >->> pl = normL *** (l >>>> pl)

infixr 4 >>->

(>>->) :: ParaLens p q x s y r -> Lens y r z t -> ParaLens p q x s z t
pl >>-> l = normR *** (pl >>>> l)

-- ParaLens product
infixr 4 ####

(####) :: ParaLens p q x s y r -> ParaLens p' q' x' s' y' r' -> ParaLens (p, p') (q, q') (x, x') (s, s') (y, y') (r, r')
(MkLens play coplay) #### (MkLens play' coplay') =
  MkLens
    (\(p, p') (x, x') -> (play p x, play' p' x'))
    ( \(p, p') (x, x') (r, r') ->
        let (s, q) = coplay p x r
            (s', q') = coplay' p' x' r'
         in ((s, s'), (q, q'))
    )

-- Non-parametric/mixed products
infixr 4 #--#

(#--#) :: Lens x s y r -> Lens x' s' y' r' -> Lens (x, x') (s, s') (y, y') (r, r')
ll #--# lr = normL *** (ll #### lr)

infixr 4 #-##

(#-##) :: Lens x s y r -> ParaLens p q x' s' y' r' -> ParaLens p q (x, x') (s, s') (y, y') (r, r')
l #-## pl = normL *** (l #### pl)

infixr 4 ##-#

(##-#) :: ParaLens p q x s y r -> Lens x' s' y' r' -> ParaLens p q (x, x') (s, s') (y, y') (r, r')
pl ##-# l = normR *** (pl #### l)

--- Normalize after product/composition with a non-parametric lens
normL :: Lens p q ((), p) ((), q)
normL = nonPara ((),) (\_ (_, q) -> q)

normR :: Lens p q (p, ()) (q, ())
normR = nonPara (,()) (\_ (q, _) -> q)

-- Reparameterization
infixr 5 ***

(***) :: Lens p' q' p q -> ParaLens p q x s y r -> ParaLens p' q' x s y r
(MkLens play coplay) *** (MkLens play' coplay') =
  MkLens
    (play' . play ())
    ( \p' x r ->
        let (s, q) = coplay' (play () p') x r
            (q', _) = coplay () p' q
         in (s, q')
    )

-- ParaLens reverse derivative
paraRdiff :: ParaLens p q x s y r -> ParaLens p (p -> q) x (x -> s) y (y -> r)
paraRdiff (MkLens play coplay) =
  MkLens
    play
    ( \p x payoff ->
        let f x' = fst $ coplay p x' (payoff (play p x'))
            g p' = snd $ coplay p' x (payoff (play p' x))
         in (f, g)
    )

-- Non-parametric lens reverse derivative
rdiff :: Lens x s y r -> Lens x (x -> s) y (y -> r)
rdiff lens = scalarToState () *** paraRdiff lens

-- Nashator
nashator :: Lens (x, x') (x -> s, x' -> s') (x, x') ((x, x') -> (s, s'))
nashator =
  nonPara
    id
    ( \(x0, x0') f ->
        let k1 x = fst $ f (x, x0')
            k2 x' = snd $ f (x0, x')
         in (k1, k2)
    )

-- Some ways to build lenses
scalarToState :: x -> State x s
scalarToState x = nonPara (const x) const

stateToScalar :: State x s -> x
stateToScalar (MkLens x _) = x () ()

funToCostate :: (x -> s) -> CoState x s
funToCostate f = nonPara (const ()) (\x _ -> f x)

costateToFun :: CoState x s -> (x -> s)
costateToFun (MkLens _ f) x = fst $ f () x ()

paraStateTofun :: ParaLens p q () () () () -> (p -> q)
paraStateTofun (MkLens _ coplay) p = snd $ coplay p () ()

funToParaState :: (p -> q) -> ParaLens p q () () () ()
funToParaState f = MkLens (\_ _ -> ()) (\p _ _ -> ((), f p))

idLens :: Lens x s x s
idLens = nonPara id (\_ x -> x)

bwd :: (r -> s) -> Lens x s x r
bwd f = nonPara id (\_ r -> f r)

fwd :: (x -> y) -> Lens x s y s
fwd fsharp = nonPara fsharp (\_ x -> x)

adapter :: (x -> y) -> (r -> s) -> Lens x s y r
adapter f fsharp = fwd f >--> bwd fsharp

-- Symmetry morphism for the monoidal category of lenses
exchange :: Lens (x, x') (s, s') (x', x) (s', s)
exchange = adapter swap swap

corner :: ParaLens p q () () p q
corner = MkLens const (\_ _ q -> ((), q))

-- Builds a full parametric lens out of two half ones
nextto :: ParaLens p () x () y () -> ParaLens () q () s () r -> ParaLens p q x s y r
nextto (MkLens play _) (MkLens _ coplay) = MkLens play (\_ _ r -> coplay () () r)

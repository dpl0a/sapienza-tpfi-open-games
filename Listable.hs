module Listable where

class Listable x where
  allValues :: [x]

instance (Listable x, Listable y) => Listable (x, y) where
  allValues :: (Listable x, Listable y) => [(x, y)]
  allValues = [(a, b) | a <- allValues, b <- allValues]

buildfuns :: (Eq x) => [x] -> [y] -> [x -> y]
buildfuns [] ys = [const undefined]
buildfuns (x : xs) ys = [ext f x y | f <- buildfuns xs ys, y <- ys]

ext :: (Eq x) => (x -> y) -> x -> y -> x -> y
ext f x y x'
  | x == x' = y
  | otherwise = f x'

instance (Eq x, Listable x, Listable y) => Listable (x -> y) where
  allValues :: (Eq x, Listable x, Listable y) => [x -> y]
  allValues = buildfuns allValues allValues

instance (Eq x, Listable x, Listable y, Show x, Show y) => Show (x -> y) where
  show :: (Eq x, Listable x, Listable y, Show x, Show y) => (x -> y) -> String
  show f = let ss = [" " ++ show x ++ " -> " ++ show (f x) | x <- allValues] in foldl1 (\x y -> x ++ "," ++ y) ss

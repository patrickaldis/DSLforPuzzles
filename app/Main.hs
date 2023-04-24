{-# OPTIONS_GHC -Wall #-}
module Main where

main :: IO ()
main = putStrLn "Hello, Haskell!"

data Toy b next
  = Output b next
  | Bell next
  | Done
data Boy b
  = Output' b (Boy b)
  | Bell' (Boy b)
  | Done'

data Cheat f = Cheat (f (Cheat f))
data MyList a = Nil | Cons a (MyList a)

exp1 :: Toy String (Toy b next)
exp1 = Output "a" Done

exp2 :: Toy b1 (Toy String (Toy b2 next))
exp2 = Bell (Output "A" Done)


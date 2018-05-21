{-# LANGUAGE MultiParamTypeClasses,
             FunctionalDependencies,
             FlexibleInstances,
             FlexibleContexts #-}
import System.Environment
import  Data.Array
import Criterion.Main
import Data.Function.Memoize
import Data.Function (on)

-- Utilizando función estándar: data function memoize
memoKnapsack_memoize :: [(Int, Int)] -> Int ->  Int -> Int
memoKnapsack_memoize items n wmax = memoize2 (knapsack' items) n wmax
  where
    knapsack' :: [(Int, Int)] -> Int ->  Int -> Int
    knapsack' items 0 w = 0
    knapsack' items i 0 = 0
    knapsack' items i w
      | snd(items !! (i-1)) > w = memoKnapsack_memoize items (i - 1) w
      | otherwise = max (memoKnapsack_memoize items (i - 1) w)
                    (memoKnapsack_memoize items (i - 1) (w - snd(items !! (i-1)))) + fst(items !! (i-1))

memoKnapsack_memoize2 :: [(Int, Int)] -> Int ->  Int -> Int
memoKnapsack_memoize2 items n wmax = kmemo n wmax
  where
    kmemo n wmax = memoize2 knapsack' n wmax
    knapsack' :: Int ->  Int -> Int
    knapsack' 0 w = 0
    knapsack' i 0 = 0
    knapsack' i w
      | snd(items !! (i-1)) > w = kmemo (i - 1) w
      | otherwise = max (kmemo (i - 1) w)
                    (kmemo (i - 1) (w - snd(items !! (i-1)))) + fst(items !! (i-1))

memoKnapsack_memoize3 :: [(Int, Int)] -> Int ->  Int -> Int
memoKnapsack_memoize3 items n wmax = kmemo n wmax
  where
    kmemo = memoFix2 knapsack'
    knapsack' r 0 w = 0
    knapsack' r i 0 = 0
    knapsack' r i w
      | snd(items !! (i-1)) > w = r (i - 1) w
      | otherwise = max (r (i - 1) w)
                    (r (i - 1) (w - snd(items !! (i-1)))) + fst(items !! (i-1))

benchmark = defaultMain [
    bgroup "Mochila" [
               bench "memoKnapsack_memoize" $ whnf (memoKnapsack_memoize [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4)]) 15
               ,bench "memoKnapsack_memoize2" $ whnf (memoKnapsack_memoize2 [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4)]) 15
               ,bench "memoKnapsack_memoize3" $ whnf (memoKnapsack_memoize3 [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4)]) 15
         ]
    ]

benchmark2 = defaultMain [
    bgroup "Mochila" [
              bench "memoKnapsack_memoize" $ whnf (memoKnapsack_memoize [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (4, 12), (2, 2), (2, 1), (1, 1), (10, 4)]) 50
              ,bench "memoKnapsack_memoize2" $ whnf (memoKnapsack_memoize2 [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (4, 12), (2, 2), (2, 1), (1, 1), (10, 4)]) 50
              ,bench "memoKnapsack_memoize3" $ whnf (memoKnapsack_memoize3 [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (4, 12), (2, 2), (2, 1), (1, 1), (10, 4)]) 50
          ]
    ]

benchmark3 = defaultMain [
    bgroup "Mochila" [
              bench "memoKnapsack_memoize" $ whnf (memoKnapsack_memoize [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (5, 5), (3,8), (1,1), (9,1), (7, 3), (5, 5), (3,8), (1,1), (9,1), (7, 3)]) 100
              ,bench "memoKnapsack_memoize2" $ whnf (memoKnapsack_memoize2 [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (5, 5), (3,8), (1,1), (9,1), (7, 3), (5, 5), (3,8), (1,1), (9,1), (7, 3)]) 100
              ,bench "memoKnapsack_memoize3" $ whnf (memoKnapsack_memoize3 [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (5, 5), (3,8), (1,1), (9,1), (7, 3), (5, 5), (3,8), (1,1), (9,1), (7, 3)]) 100
          ]
    ]

benchmark4 = defaultMain [
    bgroup "Mochila" [
              bench "memoKnapsack_memoize" $ whnf (memoKnapsack_memoize [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (5, 5), (3,8), (1,1), (9,1), (7, 3), (5, 5), (3,8), (1,1), (9,1), (7, 3), (10, 4), (5, 5), (3,8), (1,1), (9,1), (7, 3), (5, 5), (3,8), (1,1), (9,1), (7, 3)]) 140
              ,bench "memoKnapsack_memoize2" $ whnf (memoKnapsack_memoize2 [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (5, 5), (3,8), (1,1), (9,1), (7, 3), (5, 5), (3,8), (1,1), (9,1), (7, 3), (10, 4), (5, 5), (3,8), (1,1), (9,1), (7, 3), (5, 5), (3,8), (1,1), (9,1), (7, 3)]) 140
              ,bench "memoKnapsack_memoize3" $ whnf (memoKnapsack_memoize3 [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (5, 5), (3,8), (1,1), (9,1), (7, 3), (5, 5), (3,8), (1,1), (9,1), (7, 3), (10, 4), (5, 5), (3,8), (1,1), (9,1), (7, 3), (5, 5), (3,8), (1,1), (9,1), (7, 3)]) 140
          ]
    ]

main = do
{
    putStrLn "----EJECUTANDO BENCHMARK [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4)] 15--------------------------------------------------------------------------------------";
    benchmark;
    putStrLn "----EJECUTANDO BENCHMARK [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (4, 12), (2, 2), (2, 1), (1, 1), (10, 4)] 50--";
    benchmark2;
    putStrLn "----EJECUTANDO BENCHMARK [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (5, 5), (3,8), (1,1), (9,1), (7, 3), (5, 5), (3,8), (1,1), (9,1), (7, 3)] 100-";
    benchmark3;
    putStrLn "----EJECUTANDO BENCHMARK [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (5, 5), (3,8), (1,1), (9,1), (7, 3), (5, 5), (3,8), (1,1), (9,1), (7, 3), (10, 4), (5, 5), (3,8), (1,1), (9,1), (7, 3), (5, 5), (3,8), (1,1), (9,1), (7, 3)]) 140-";
    benchmark4;
}

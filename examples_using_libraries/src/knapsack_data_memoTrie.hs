{-# LANGUAGE MultiParamTypeClasses,
             FunctionalDependencies,
             FlexibleInstances,
             FlexibleContexts #-}

import System.Environment
import  Data.Array
import Criterion.Main
import Data.MemoTrie

memoKnapsack_memoTrie :: [(Int, Int)] -> Int ->  Int -> Int
memoKnapsack_memoTrie items n wmax = knapsack' n wmax
  where
    knapsack' :: Int ->  Int -> Int
    knapsack' 0 w = 0
    knapsack' i 0 = 0
    knapsack' i w
      | snd(items !! (i-1)) > w = memo2 knapsack' (i - 1) w
      | otherwise = max (memo2 knapsack' (i - 1) w)
                    (memo2 knapsack' (i - 1) (w - snd(items !! (i-1)))) + fst(items !! (i-1))

benchmark = defaultMain [
    bgroup "Mochila" [
               bench "memoKnapsack_memoTrie" $ whnf (memoKnapsack_memoTrie [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4)]) 15
         ]
    ]

benchmark2 = defaultMain [
    bgroup "Mochila" [
              bench "memoKnapsack_memoTrie" $ whnf (memoKnapsack_memoTrie [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (4, 12), (2, 2), (2, 1), (1, 1), (10, 4)]) 50
          ]
    ]

benchmark3 = defaultMain [
    bgroup "Mochila" [
              bench "memoKnapsack_memoTrie" $ whnf (memoKnapsack_memoTrie [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (5, 5), (3,8), (1,1), (9,1), (7, 3), (5, 5), (3,8), (1,1), (9,1), (7, 3)]) 100
          ]
    ]

benchmark4 = defaultMain [
    bgroup "Mochila" [
              bench "memoKnapsack_memoTrie" $ whnf (memoKnapsack_memoTrie [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (5, 5), (3,8), (1,1), (9,1), (7, 3), (5, 5), (3,8), (1,1), (9,1), (7, 3), (10, 4), (5, 5), (3,8), (1,1), (9,1), (7, 3), (5, 5), (3,8), (1,1), (9,1), (7, 3)]) 140
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

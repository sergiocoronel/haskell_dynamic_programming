import System.Environment
import Data.Array
import Criterion.Main
import Data.MemoCombinators
import Data.Function.Memoize
import Data.Function (on)
import Data.List (sortBy)
import Data.Hashable
import qualified Data.HashMap.Strict as H
import Debug.Trace


memoKnapsack_array :: [(Int, Int)] -> Int -> Int
memoKnapsack_array items w = table ! (n, w)
    where
        n = length items
        bnds  = ((0,0),(n,w))
        table = array bnds [((i,w'), knps i w') | (i,w') <- range bnds]

        knps _ 0 = 0
        knps 0 _ = 0
        knps i j
            | j < wi = noUsarItem
            | otherwise = max usarItem noUsarItem
                where (vi,wi) = items !! (i - 1)
                      usarItem   = vi + table ! (i - 1, j - wi)
                      noUsarItem = table ! (i -1, j)

memoKnapsack_array2 :: [(Int, Int)] -> Int -> Int
memoKnapsack_array2 items w = table ! (n, w)
    where
        n = length items
        bnds  = ((0,0),(n,w))
        table = array bnds [((i,w'), knps i w') | (i,w') <- range bnds]
        itemarray = listArray (0, n - 1) items

        knps _ 0 = 0
        knps 0 _ = 0
        knps i j
            | j < wi = noUsarItem
            | otherwise = max usarItem noUsarItem
                where (vi,wi) = itemarray ! (i - 1)
                      usarItem   = vi + table ! (i - 1, j - wi)
                      noUsarItem = table ! (i -1, j)

memoKnapsack_array3 :: [(Int, Int)] -> Int -> Int
memoKnapsack_array3 items w = table ! (n-1, w)
    where
        n = length items
        table = array ((-1,0), (n-1, w)) $
                  [((-1,wi), 0) | wi <- [0 .. w]] ++
                  [((i,0), 0) | i <- [0 .. n-1]] ++
                  [((i,wi), knps i wi)
                      | i <- [0 .. n-1]
                      , wi <- [1 .. w]]

        knps _ 0 = 0
        knps 0 _ = 0
        knps i j
            | j < wi = noUsarItem
            | otherwise = max usarItem noUsarItem
                where (vi,wi) = items !! (i - 1)
                      usarItem   = vi + table ! (i - 1, j - wi)
                      noUsarItem = table ! (i -1, j)

memoKnapsack_array4 :: [(Int, Int)] -> Int -> Int
memoKnapsack_array4 items w = table ! (n, w)
    where
        n = length items - 1
        table = array ((-1,0), (n, w)) $
                  [((-1,wi), 0) | wi <- [0 .. w]] ++
                  [((i,0), 0) | i <- [0 .. n]] ++
                  [((i,wi), knps i wi)
                      | i <- [0 .. n]
                      , wi <- [1 .. w]]
        knps _ 0 = 0
        knps 0 _ = 0
        knps i j
            | j < wi = noUsarItem
            | otherwise = max usarItem noUsarItem
                where i' = i - 1
                      (vi,wi) = items !! i'
                      usarItem   = vi + table ! (i', j - wi)
                      noUsarItem = table ! (i', j)

memoKnapsack_array5 :: [(Int, Int)] -> Int -> Int
memoKnapsack_array5 items w = table ! (n, w)
    where
        n = length items - 1
        itemarray = listArray (0, n) items
        table = array ((-1,0), (n, w)) $
                  [((-1,wi), 0) | wi <- [0 .. w]] ++
                  [((i,0), 0) | i <- [0 .. n]] ++
                  [((i,wi), knps i wi)
                      | i <- [0 .. n]
                      , wi <- [1 .. w]]
        knps _ 0 = 0
        knps 0 _ = 0
        knps i j
            | j < wi = noUsarItem
            | otherwise = max usarItem noUsarItem
                where i' = i - 1
                      (vi,wi) = itemarray ! i
                      usarItem   = vi + table ! (i', j - wi)
                      noUsarItem = table ! (i', j)

memoKnapsack_array6 :: [(Int, Int)] -> Int -> Int
memoKnapsack_array6 items w = table ! (n, w)
    where
        n = length items - 1
        itemarray = listArray (0, n) items
        table = array ((-1,0), (n, w)) $
                  [((-1,wi), 0) | wi <- [0 .. w]] ++
                  [((i,0), 0) | i <- [0 .. n]] ++
                  [((i,wi), knps elem i wi)
                      | i <- [0 .. n], let elem = itemarray ! i
                      , wi <- [1 .. w]]
        knps elem _ 0 = 0
        knps elem 0 _ = 0
        knps (vi,wi) i j
            | j < wi = noUsarItem
            | otherwise = max usarItem noUsarItem
                where i' = i - 1
                      usarItem   = vi + table ! (i', j - wi)
                      noUsarItem = table ! (i', j)

memoKnapsack_array7 :: [(Int, Int)] -> Int -> Int
memoKnapsack_array7 items w = table ! (n, w)
    where
        n = length items - 1
        itemarray = listArray (0, n) (sortBy (compare `on` snd) items)
        table = array ((-1,0), (n, w)) $
                  [((-1,wi), 0) | wi <- [0 .. w]] ++
                  [((i,0), 0) | i <- [0 .. n]] ++
                  [((i,wi), knps elem i wi)
                      | i <- [0 .. n], let elem = itemarray ! i
                      , wi <- [1 .. w]]
        knps elem _ 0 = 0
        knps elem 0 _ = 0
        knps (vi,wi) i j
            | j < wi = noUsarItem
            | otherwise = max usarItem noUsarItem
                where i' = i - 1
                      usarItem   = vi + table ! (i', j - wi)
                      noUsarItem = table ! (i', j)

benchmark xs n = defaultMain [
    bgroup "Mochila" [
                 bench "memoKnapsack_array" $ whnf (memoKnapsack_array xs) n
                 , bench "memoKnapsack_array2" $ whnf (memoKnapsack_array2 xs) n
                 , bench "memoKnapsack_array3" $ whnf (memoKnapsack_array3 xs) n
                 , bench "memoKnapsack_array4" $ whnf (memoKnapsack_array4 xs) n
                 , bench "memoKnapsack_array5" $ whnf (memoKnapsack_array5 xs) n
                 , bench "memoKnapsack_array6" $ whnf (memoKnapsack_array6 xs) n
                 , bench "memoKnapsack_array7" $ whnf (memoKnapsack_array7 xs) n
         ]
    ]

main = do
{
    putStrLn "----EJECUTANDO BENCHMARK [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4)] 15--------------------------------------------------------------------------------------";
    benchmark [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4)] 15;
    putStrLn "----EJECUTANDO BENCHMARK [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (4, 12), (2, 2), (2, 1), (1, 1), (10, 4)] 50--";
    benchmark [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (4, 12), (2, 2), (2, 1), (1, 1), (10, 4)] 50;
    putStrLn "----EJECUTANDO BENCHMARK [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (5, 5), (3,8), (1,1), (9,1), (7, 3), (5, 5), (3,8), (1,1), (9,1), (7, 3)] 100-";
    benchmark [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (5, 5), (3,8), (1,1), (9,1), (7, 3), (5, 5), (3,8), (1,1), (9,1), (7, 3)] 100;
    putStrLn "----EJECUTANDO BENCHMARK [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (5, 5), (3,8), (1,1), (9,1), (7, 3), (5, 5), (3,8), (1,1), (9,1), (7, 3), (10, 4), (5, 5), (3,8), (1,1), (9,1), (7, 3), (5, 5), (3,8), (1,1), (9,1), (7, 3)]) 140-";
    benchmark [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (5, 5), (3,8), (1,1), (9,1), (7, 3), (5, 5), (3,8), (1,1), (9,1), (7, 3), (10, 4), (5, 5), (3,8), (1,1), (9,1), (7, 3), (5, 5), (3,8), (1,1), (9,1), (7, 3)] 140;
}

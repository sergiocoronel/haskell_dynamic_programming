{-# LANGUAGE MultiParamTypeClasses,
             FunctionalDependencies,
             FlexibleInstances,
             FlexibleContexts #-}

import System.Environment
import Data.Array.ST
import Data.Array.MArray
import Criterion.Main
import Data.Function (fix)
import Prelude hiding (lookup)
import Control.Monad.State
import Data.Map.Strict
import Control.Monad.Identity
import Control.Monad.ST ( ST, runST )

-- Revisar recorrido, de principio a fin o vice
-- En este caso no seria ordenado
knapsack :: Monad m => ([(Int,Int)], Int) -> m Int
knapsack (_, 0)  = return 0
knapsack ([], w) = return 0
knapsack (items, w)
        | w < snd(head items) = knapsack ((tail items), w)
        | otherwise = do
                usarItem   <- knapsack ((tail items), (w - snd(head items)))
                noUsarItem <- knapsack ((tail items), w)
                return (max (usarItem + fst(head items)) noUsarItem)

knapsack2 :: [(Int, Int)] -> Int -> Int
knapsack2 items maxW = knapsack2' n maxW
  where
        n = length items

        knapsack2' _ 0 = 0
        knapsack2' 0 _ = 0
        knapsack2' i w
          | wi > w  = noUsar
          | otherwise = max usar noUsar
          where wi = snd(it1)
                j = i -1
                it1 = items !! j
                noUsar = knapsack2' j w
                usar = (knapsack2' j (w - wi)) + fst(it1)

-- Open recursion
type Gen a = a -> a

knapsack' :: Monad m => Gen(([(Int,Int)], Int) -> m Int)
knapsack' _ (_, 0)  = return 0
knapsack' _ ([], w) = return 0
knapsack' self (items, w)
        | w < snd(head items) = self ((tail items), w)
        | otherwise = do
                usarItem   <- self ((tail items), (w - snd(head items)))
                noUsarItem <- self ((tail items), w)
                return (max (usarItem + fst(head items)) noUsarItem)


--mknapsack2 :: [(Int, Int)] -> Int -> Int
--mknapsack2 items maxW = knapsack2' (n, maxW)
--  where
--        n = length items
--
--        knapsack2':: Monad m => (Int, Int) -> m Int
--        knapsack2' (_, 0) = return 0
--        knapsack2' (0, _) = return 0
--        knapsack2' (i, w)
--            | wi > w = knapsack2' (j, w)
--            | otherwise = do
--                usarItem   <- knapsack2' (j, (w - wi))
--                noUsarItem <- knapsack2' (j, w)
--                return (max (usarItem + fst(it1)) noUsarItem)
--            where
--                j = (i -1)
--                it1 = items !! j
--                wi = snd(it1)


--mknapsack2 :: [(Int, Int)] -> Int -> Int
--mknapsack2 items maxW = knapsack2' (n, maxW)
--  where
--        n = length items
--
--        knapsack2':: Monad m => (Int, Int) -> m Int
--        knapsack2' (_, 0) = return 0
--        knapsack2' (0, _) = return 0
--        knapsack2' (i, w) = do
--            j <- (i -1)
--            it1 <- items !! j
--            wi <- snd(it1)
--            noUsar <- knapsack2' (j, w)
--            if (wi > w)
--                then return noUsar
--                else do
--                    usar <- (knapsack2' (j, (w - wi))) + fst(it1)
--                    return (max usar noUsar)


type Dict a b m = (a -> m (Maybe b), a -> b -> m ())

memo :: Monad m => Dict a b m -> Gen (a -> m b)
memo (check, store) super a = do
    b <- check a
    case b of
        Just b  -> return b
        Nothing -> do
            b <- super a
            store a b
            return b

mapDict :: Ord a => Dict a b (State (Map a b))
mapDict = (check, store) where
    check a   = gets (lookup a)
    store a b = modify (insert a b)

type MapMemoized a b = a -> State (Map a b) b
--type ArrayMemoized a b = a -> State (MArray a b) b


--arrayDict :: Ord a => Dict a b (State (MArray a b))
--arrayDict = (check, store) where
--    check a   = gets (readArray a)
--    store a b = modify (writeArray a b)
--

memoKnapsack :: MapMemoized ([(Int, Int)], Int) Int
memoKnapsack = fix (memo mapDict . knapsack')

runMemoKnapsack :: [(Int, Int)] -> Int -> Int
runMemoKnapsack items w = evalState (memoKnapsack (items, w)) empty


memoknapsack2 :: [(Int, Int)] -> Int -> Int
memoknapsack2 items maxW = evalState (memoKnapsack2' (n, maxW)) empty
  where
        n = length items
        memoKnapsack2' :: MapMemoized (Int, Int) Int
        memoKnapsack2' = fix (memo mapDict . knapsack2')

        knapsack2':: Monad m => Gen((Int, Int) -> m Int)
        knapsack2' _ (_, 0) = return 0
        knapsack2' _ (0, _) = return 0
        knapsack2' self (i, w)
            | wi > w = self (j, w)
            | otherwise = do
                usarItem   <- self (j, (w - wi))
                noUsarItem <- self (j, w)
                return (max (usarItem + fst(it1)) noUsarItem)
            where
                j = (i -1)
                it1 = items !! j
                wi = snd(it1)

--memoknapsack3 :: [(Int, Int)] -> Int -> Int
--memoknapsack3 items maxW = evalState (memoKnapsack3' (n, maxW)) empty
--  where
--        n = length items
--        size = n * maxW
--
--
--        memoKnapsack3' :: ArrayMemoized (Int, Int) Int
--        memoKnapsack3' = fix (memo arrayDict . knapsack3')
--
--        knapsack3':: Monad m => Gen((Int, Int) -> m Int)
--        knapsack3' _ (_, 0) = return 0
--        knapsack3' _ (0, _) = return 0
--        knapsack3' self (i, w)
--            | wi > w = self (j, w)
--            | otherwise = do
--                usarItem   <- self (j, (w - wi))
--                noUsarItem <- self (j, w)
--                return (max (usarItem + fst(it1)) noUsarItem)
--            where
--                j = (i -1)
--                it1 = items !! j
--                wi = snd(it1)

--

benchmark = defaultMain [
    bgroup "Mochila" [
                 bench "runMemoKnapsack2" $ whnf (memoknapsack2 [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4)]) 15
                 ,bench "runMemoKnapsack" $ whnf (runMemoKnapsack [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4)]) 15
         ]
    ]

benchmark2 = defaultMain [
    bgroup "Mochila" [
              bench "runMemoKnapsack2" $ whnf (memoknapsack2 [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (4, 12), (2, 2), (2, 1), (1, 1), (10, 4)]) 50
              ,bench "runMemoKnapsack" $ whnf (runMemoKnapsack [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (4, 12), (2, 2), (2, 1), (1, 1), (10, 4)]) 50
          ]
    ]

benchmark3 = defaultMain [
    bgroup "Mochila" [
              bench "runMemoKnapsack2" $ whnf (memoknapsack2 [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (5, 5), (3,8), (1,1), (9,1), (7, 3), (5, 5), (3,8), (1,1), (9,1), (7, 3)]) 100
              ,bench "runMemoKnapsack" $ whnf (runMemoKnapsack [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (5, 5), (3,8), (1,1), (9,1), (7, 3), (5, 5), (3,8), (1,1), (9,1), (7, 3)]) 100
          ]
    ]

benchmark4 = defaultMain [
    bgroup "Mochila" [
              bench "runMemoKnapsack2" $ whnf (memoknapsack2 [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (5, 5), (3,8), (1,1), (9,1), (7, 3), (5, 5), (3,8), (1,1), (9,1), (7, 3), (10, 4), (5, 5), (3,8), (1,1), (9,1), (7, 3), (5, 5), (3,8), (1,1), (9,1), (7, 3)]) 140
              ,bench "runMemoKnapsack" $ whnf (runMemoKnapsack [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (5, 5), (3,8), (1,1), (9,1), (7, 3), (5, 5), (3,8), (1,1), (9,1), (7, 3), (10, 4), (5, 5), (3,8), (1,1), (9,1), (7, 3), (5, 5), (3,8), (1,1), (9,1), (7, 3)]) 140
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

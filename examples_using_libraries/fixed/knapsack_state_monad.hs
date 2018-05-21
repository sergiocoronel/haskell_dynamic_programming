{-# LANGUAGE MultiParamTypeClasses, 
             FunctionalDependencies, 
             FlexibleInstances, 
             FlexibleContexts #-}
import System.Environment
import  Data.Array
import Criterion.Main
import Control.Monad.Memo
import Data.Function (on)
import Control.Applicative hiding (empty) 
import Control.Monad.ST ( ST, runST )

import Data.Hashable
import qualified Data.HashMap.Strict as H
import qualified Control.Monad.Memo.Vector.Expandable as VE

memoknapsack2 :: [(Int, Int)] -> Int -> Int           
memoknapsack2 items maxW = startEvalMemo $ knapsack2' n maxW
  where 
        n = length items
        
        knapsack2':: (Num Int, Ord Int, MonadMemo (Int, Int) Int m) => Int -> Int -> m Int
        knapsack2' _ 0 = return 0
        knapsack2' 0 _ = return 0
        knapsack2' i w 
            | wi > w = for2 memo knapsack2' j w 
            | otherwise = do 
                usarItem   <- for2 memo knapsack2' j (w - wi)
                noUsarItem <- for2 memo knapsack2' j w
                return (max (usarItem + fst(it1)) noUsarItem)
            where
                j = (i -1)
                it1 = items !! j
                wi = snd(it1)               

instance (Eq k, Hashable k) => MapLike (H.HashMap k v) k v where
    lookup = H.lookup
    add = H.insert
                    
memoknapsack3 :: [(Int, Int)] -> Int -> Int           
memoknapsack3 items maxW = evalMemoState (knapsack2' n maxW) H.empty
  where 
        n = length items
        
        knapsack2':: (Num Int, Ord Int, MonadMemo (Int, Int) Int m) => Int -> Int -> m Int
        knapsack2' _ 0 = return 0
        knapsack2' 0 _ = return 0
        knapsack2' i w 
            | wi > w = for2 memo knapsack2' j w 
            | otherwise = do 
                usarItem   <- for2 memo knapsack2' j (w - wi)
                noUsarItem <- for2 memo knapsack2' j w
                return (max (usarItem + fst(it1)) noUsarItem)
            where
                j = (i -1)
                it1 = items !! j
                wi = snd(it1)

memoknapsack4 :: [(Int, Int)] -> Int -> Int           
memoknapsack4 items maxW = runST $ evalArrayMemo (knapsack2' n maxW) ((0,0),(n,maxW))  
  where 
        n = length items
        
        knapsack2':: (Num Int, Ord Int, MonadMemo (Int, Int) Int m) => Int -> Int -> m Int
        knapsack2' _ 0 = return 0
        knapsack2' 0 _ = return 0
        knapsack2' i w 
            | wi > w = for2 memo knapsack2' j w 
            | otherwise = do 
                usarItem   <- for2 memo knapsack2' j (w - wi)
                noUsarItem <- for2 memo knapsack2' j w
                return (max (usarItem + fst(it1)) noUsarItem)
            where
                j = (i -1)
                it1 = items !! j
                wi = snd(it1)
--
--memoknapsack5 :: [(Int, Int)] -> Int -> Int           
--memoknapsack5 items maxW = runST $ evalVectorMemo (knapsack2' n maxW) (n * maxW)
--  where 
--        n = length items
--        
--        knapsack2':: (Num Int, Ord Int, MonadMemo (Int, Int) Int m) => Int -> Int -> m Int
--        knapsack2' _ 0 = return 0
--        knapsack2' 0 _ = return 0
--        knapsack2' i w 
--            | wi > w = for2 memo knapsack2' j w 
--            | otherwise = do 
--                usarItem   <- for2 memo knapsack2' j (w - wi)
--                noUsarItem <- for2 memo knapsack2' j w
--                return (max (usarItem + fst(it1)) noUsarItem)
--            where
--                j = (i -1)
--                it1 = items !! j
--                wi = snd(it1)
--                                                
main = defaultMain [
    bgroup "fibonacci_fixed" [ 
                
                 bench "runMemoKnapsack2" $ whnf (memoknapsack2 [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4)]) 15
                , bench "runMemoKnapsack2_50" $ whnf (memoknapsack2 [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (4, 12), (2, 2), (2, 1), (1, 1), (10, 4)]) 50
                , bench "runMemoKnapsack2_100" $ whnf (memoknapsack2 [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (5, 5), (3,8), (1,1), (9,1), (7, 3), (5, 5), (3,8), (1,1), (9,1), (7, 3)]) 100
                , bench "runMemoKnapsack3" $ whnf (memoknapsack3 [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4)]) 15
                , bench "runMemoKnapsack3_50" $ whnf (memoknapsack3 [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (4, 12), (2, 2), (2, 1), (1, 1), (10, 4)]) 50
                , bench "runMemoKnapsack3_100" $ whnf (memoknapsack3 [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (5, 5), (3,8), (1,1), (9,1), (7, 3), (5, 5), (3,8), (1,1), (9,1), (7, 3)]) 100
                , bench "runMemoKnapsack4" $ whnf (memoknapsack4 [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4)]) 15
                , bench "runMemoKnapsack4_50" $ whnf (memoknapsack4 [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (4, 12), (2, 2), (2, 1), (1, 1), (10, 4)]) 50
                , bench "runMemoKnapsack4_100" $ whnf (memoknapsack4 [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (5, 5), (3,8), (1,1), (9,1), (7, 3), (5, 5), (3,8), (1,1), (9,1), (7, 3)]) 100
--                bench "runMemoKnapsack5" $ whnf (memoknapsack5 [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4)]) 15
--                , bench "runMemoKnapsack5_50" $ whnf (memoknapsack5 [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (4, 12), (2, 2), (2, 1), (1, 1), (10, 4)]) 50
--                , bench "runMemoKnapsack5_100" $ whnf (memoknapsack5 [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (5, 5), (3,8), (1,1), (9,1), (7, 3), (5, 5), (3,8), (1,1), (9,1), (7, 3)]) 100
             ]
            ]                  
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

             
fixedknapsack :: [(Int, Int)] -> Int -> Int           
fixedknapsack items maxW = (fix (memoize . fib)) n maxW
  where 
        n = length items
        memoize :: (Int -> a) -> (Int -> a)
        memoize f = (map f [0 ..] !!)
        
        knapsack2' _ 0 = 0
        knapsack2' 0 _ = 0
        knapsack2' self i w 
            | wi > w = self j w 
            | otherwise = max (usarItem + fst(it1)) noUsarItem              
            where
                j = (i -1)
                it1 = items !! j
                wi = snd(it1)
                usarItem   = self j (w - wi)
                noUsarItem = self j w
  
main = defaultMain [
    bgroup "fibonacci_fixed" [ 
                bench "runMemoKnapsack" $ whnf (runMemoKnapsack [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4)]) 15
                , bench "runMemoKnapsack50" $ whnf (runMemoKnapsack [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (4, 12), (2, 2), (2, 1), (1, 1), (10, 4)]) 50
                , bench "runMemoKnapsack100" $ whnf (runMemoKnapsack [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (5, 5), (3,8), (1,1), (9,1), (7, 3), (5, 5), (3,8), (1,1), (9,1), (7, 3)]) 100
                
                , bench "runMemoKnapsack2" $ whnf (memoknapsack2 [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4)]) 15
                , bench "runMemoKnapsack2_50" $ whnf (memoknapsack2 [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (4, 12), (2, 2), (2, 1), (1, 1), (10, 4)]) 50
                , bench "runMemoKnapsack2_100" $ whnf (memoknapsack2 [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (5, 5), (3,8), (1,1), (9,1), (7, 3), (5, 5), (3,8), (1,1), (9,1), (7, 3)]) 100
             ]
            ]    
    
    
    
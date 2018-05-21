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

-- monadic fib
mFib :: Monad m => Int -> m Int
mFib 0 = return 0
mFib 1 = return 1
mFib n = do { a <- mFib (n-2); b <- mFib (n-1); return (a+b)}

--original fib from monadic
--runIdentity :: Identity a -> a
--fib :: Int -> Int
--fib = runIdentity . mFib

-- Open recursion
type Gen a = a -> a
gFib :: Gen (Int -> Int)
gFib self 0 = 0
gFib self 1 = 1
gFib self n = self (n-2) + self (n-1)

fibg :: Int -> Int
fibg = fix gFib

--Open recursion and monadic fib
gmFib :: Monad m => Gen (Int -> m Int)
gmFib self 0 = return 0
gmFib self 1 = return 1
gmFib self n = do {a <- self (n-2); b <- self (n-1); return (a + b)}

fibgm :: Int -> Int
fibgm = runIdentity . (fix gmFib)

--Memo table
type Dict a b m = (a -> m (Maybe b), a -> b -> m ())

memo :: Monad m => Dict a b m -> Gen (a -> m b)
memo (check,store) super a = do
    b <- check a
    case b of
        Just b -> return b
        Nothing -> do
            b <- super a
            store a b
            return b

--Memoized fib
type Memoized a b m = Dict a b m -> a -> m b
memoFib :: Monad m => Memoized Int Int m
memoFib dict = fix (memo dict . gmFib)

mapDict :: Ord a => Dict a b (State (Map a b))
mapDict = (check,store) where
    check a = gets (lookup a)
    store a b = modify (insert a b)
memoMapFib :: Int -> State (Map Int Int) Int
memoMapFib = memoFib mapDict

runMemoMapFib :: Int -> Int
runMemoMapFib n = evalState (memoMapFib n) empty

arrayDict :: (MArray arr (Maybe b) m,Ix a, Ord a) =>
    a -> arr a (Maybe b) -> Dict a b m
arrayDict size arr = (check,store) where
    check a = if a > size then return Nothing else readArray arr a
    store a b = if a > size then return () else writeArray arr a (Just b)

newSTArray :: Ix i => (i, i) -> e -> ST s (STArray s i e)
newSTArray = newArray

runMemoArrayFib :: Int -> Int -> Int
runMemoArrayFib size n = runST (do
    arr <- newSTArray (0,size) Nothing
    memoFib (arrayDict size arr ) n)

-- Benchmarking
benchmark n = defaultMain [
    bgroup "fibonacci" [
                    bench "runMemoMapFib" $ whnf (runMemoMapFib) n
                    , bench "runMemoArrayFib" $ whnf (runMemoArrayFib n) n
                 ]
                ]

main = do
{
    putStrLn "-----------------------------------------EJECUTANDO BENCHMARK N=30-----------------------------------------";
    benchmark 30;
    putStrLn "-----------------------------------------EJECUTANDO BENCHMARK N=15000-----------------------------------------";
    benchmark 15000;
    putStrLn "-----------------------------------------EJECUTANDO BENCHMARK N=50000-----------------------------------------";
    benchmark 50000;
    putStrLn "-----------------------------------------EJECUTANDO BENCHMARK N=100000-----------------------------------------";
    benchmark 100000;
    putStrLn "-----------------------------------------EJECUTANDO BENCHMARK N=130000-----------------------------------------";
    benchmark 130000;
}

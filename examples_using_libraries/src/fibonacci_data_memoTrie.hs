{-# LANGUAGE MultiParamTypeClasses,
             FunctionalDependencies,
             FlexibleInstances,
             FlexibleContexts #-}
import System.Environment
import  Data.Array
import Criterion.Main
import Data.MemoTrie


fib_memoTrie :: Int -> Int
fib_memoTrie = memoFix mfib
    where
        mfib r 0 = 0
        mfib r 1 = 1
        mfib r n = r (n-2) + r (n-1)

fib_memoTrie2 :: Int -> Int
fib_memoTrie2 = mfib
    where
        mfib 0 = 0
        mfib 1 = 1
        mfib n = memo mfib (n-2) + memo mfib (n-1)

fib_memoTrie3 :: Int -> Int
fib_memoTrie3 0 = 0
fib_memoTrie3 1 = 1
fib_memoTrie3 n = memo fib_memoTrie2 (n-2) + memo fib_memoTrie2 (n-1)

-- Benchmarking
benchmark n = defaultMain [
    bgroup "fibonacci" [
                    bench "fib_memoTrie" $ whnf (fib_memoTrie) n
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

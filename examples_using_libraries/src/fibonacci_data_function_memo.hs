{-# LANGUAGE MultiParamTypeClasses,
             FunctionalDependencies,
             FlexibleInstances,
             FlexibleContexts #-}
import System.Environment
import  Data.Array
import Criterion.Main
import Data.Function.Memoize
import Data.Function (on)



fib_memoize :: Integer -> Integer
fib_memoize n = memoize fibm n
   where
         fibm 0 = 0
         fibm 1 = 1
         fibm n = fib_memoize (n-2) + fib_memoize (n-1)

_fib :: Integer -> Integer
_fib = memoFix $ \fib n -> case n of
  0 -> 1
  1 -> 1
  _ -> fib (n - 1) + fib (n - 2)

-- Benchmarking
benchmark n = defaultMain [
    bgroup "fibonacci" [
                    bench "_fib" $ whnf (_fib) n
                    , bench "fib_memoize" $ whnf (fib_memoize) n
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

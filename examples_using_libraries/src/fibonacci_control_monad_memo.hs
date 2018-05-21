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

fib_memo :: Int -> Int
fib_memo n = startEvalMemo $ fibm n
   where
         fibm :: (Eq n, Num n, Ord n) => n -> Memo n n n
         fibm 0 = return 0
         fibm 1 = return 1
         fibm n = do
                    a <- memo fibm (n-2)
                    b <- memo fibm (n-1)
                    return (a + b)


instance (Eq k, Hashable k) => MapLike (H.HashMap k v) k v where
    lookup = H.lookup
    add = H.insert

fib_memo2 :: Int -> Int
fib_memo2 n = evalMemoState (fibm n) H.empty
   where
         fibm :: (MonadMemo Int Int m) => Int -> m Int
         fibm 0 = return 0
         fibm 1 = return 1
         fibm n = do
                    a <- memo fibm (n-2)
                    b <- memo fibm (n-1)
                    return (a + b)

fib_memo3 :: Int -> Int
fib_memo3 n = runST $ evalArrayMemo (fibm n) (0,n)
   where
         fibm :: (MonadMemo Int Int m) => Int -> m Int
         fibm 0 = return 0
         fibm 1 = return 1
         fibm n = do
                    a <- memo fibm (n-2)
                    b <- memo fibm (n-1)
                    return (a + b)

-- Benchmarking
benchmark n = defaultMain [
    bgroup "fibonacci" [
                    bench "fib_memo" $ whnf (fib_memo) n
                    , bench "fib_memo2" $ whnf (fib_memo2) n
                    , bench "fib_memo3" $ whnf (fib_memo3) n
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

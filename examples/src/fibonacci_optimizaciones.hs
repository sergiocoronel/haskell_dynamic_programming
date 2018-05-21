import System.Environment
import Data.Array as A
import Data.Bool
import Data.Char
import Criterion.Main
import Criterion.Main.Options
import Data.Vector as V

memoized_fib_array n = fibTab A.! n
  where fibTab = listArray (0, n) [mfib x | x <- [0..n]]
        mfib 0 = 0
        mfib 1 = 1
        mfib x = fibTab A.! (x - 2) + fibTab A.! (x - 1)

memoized_fib_array2 n = fibTab A.! n
  where fibTab = array (0,n) ((0,0):(1,1):[(i, mfib i) | i <- [2..n]])
        mfib 0 = 0
        mfib 1 = 1
        mfib x = fibTab A.! (x - 2) + fibTab A.! (x - 1)


memoized_fib_vector n = vect V.! n
  where
        vect = generate (n+1) mfib
        mfib 0 = 0
        mfib 1 = 1
        mfib x = vect V.! (x - 2) + vect V.! (x - 1)

memoized_fib_vector2 n = vect V.! n
  where
        vect = generate (n+1) mfib
        mfib 0 = 0
        mfib 1 = 1
        mfib x = unsafeIndex vect (x - 2) + unsafeIndex vect (x - 1)

-- Benchmarking
benchmark n = defaultMain [
    bgroup "fibonacci optimizaciones" [
                bench "memoized_fib_array" $ whnf (memoized_fib_array) n
                , bench "memoized_fib_array2" $ whnf (memoized_fib_array2) n
                , bench "memoized_fib_vector" $ whnf (memoized_fib_vector) n
                , bench "memoized_fib_vector2" $ whnf (memoized_fib_vector2) n
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

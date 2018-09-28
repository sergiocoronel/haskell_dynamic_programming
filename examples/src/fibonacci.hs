import System.Environment
import Data.Array
import Data.Bool
import Data.Char
import Criterion.Main
import Criterion.Main.Options


fibNaive 0 = 0
fibNaive 1 = 1
fibNaive n = fibNaive (n-1) + fibNaive (n-2)

memoized_fib_list :: Int -> Integer
memoized_fib_list n = fibTab !! n
   where fibTab = map fibm [0 .. n]
         fibm 0 = 0
         fibm 1 = 1
         fibm n = fibTab !! (n-2) + fibTab !! (n-1)

-- Esta funcion el compilador de haskell la optimiza y transforma la lista en global
-- por lo que sucesivas iteraciones de criterion terminan recorriendo la misma lista
-- ya resuelta -> Esta fue la primera implementacion realizada y nos produjo resultados
-- poco intuitivos. Se la mantiene para ilustrar una de las problematicas que se tuvo
memoized_fib_list_global :: Int -> Integer
memoized_fib_list_global n = fibTab !! n
  where fibTab = map fibm [0 ..]
        fibm 0 = 0
        fibm 1 = 1
        fibm n = fibTab !! (n-2) + fibTab !! (n-1)

memoized_fib_array n = fibTab ! n
  where fibTab = listArray (0, n) [mfib x | x <- [0..n]]
        mfib 0 = 0
        mfib 1 = 1
        mfib x = fibTab ! (x - 2) + fibTab ! (x - 1)

-- Funcion auxiliar para comparar que el resultado para n sea igual
compare_fib :: Int -> Bool
compare_fib 0 = (fibNaive 0 == memoized_fib_list 0)
compare_fib n = (fibNaive n == memoized_fib_list n) && compare_fib (n - 1)

-- Benchmarking
benchmark n = defaultMain [
    bgroup "fibonacci" [ bench "fibNaive" $ whnf (fibNaive) n
                 , bench "memoized_fib_list" $ whnf (memoized_fib_list) n
                 , bench "memoized_fib_list_global" $ whnf (memoized_fib_list_global) n
                 , bench "memoized_fib_array" $ whnf (memoized_fib_array) n
                 ]
                ]

-- Benchmarking
benchmark2 n = defaultMain [
    bgroup "best algo" [
                bench "memoized_fib_list" $ whnf (memoized_fib_list) n,
                bench "memoized_fib_list_global" $ whnf (memoized_fib_list_global) n
                , bench "memoized_fib_array" $ whnf (memoized_fib_array) n
         ]
   ]

benchmark3 n = defaultMain [
   bgroup "best algo" [
               bench "memoized_fib_list_global" $ whnf (memoized_fib_list_global) n
               , bench "memoized_fib_array" $ whnf (memoized_fib_array) n
        ]
  ]

benchmark4 = defaultMain [
    bgroup "memoized_fib" [
            bench "memoized_fib" $ whnf (memoized_fib_list) 40
            , bench "memoized_fib" $ whnf (memoized_fib_list) 15000
            , bench "memoized_fib" $ whnf (memoized_fib_list) 50000
            , bench "memoized_fib" $ whnf (memoized_fib_list) 100000
         ]
        ]
main = do
{
    putStrLn "-----------------------------------------EJECUTANDO BENCHMARK N=40-----------------------------------------";
    benchmark 40;
    putStrLn "-----------------------------------------EJECUTANDO BENCHMARK N=15000-----------------------------------------";
    benchmark2 15000;
    putStrLn "-----------------------------------------EJECUTANDO BENCHMARK N=50000-----------------------------------------";
    benchmark3 50000;
    putStrLn "-----------------------------------------EJECUTANDO BENCHMARK N=100000-----------------------------------------";
    benchmark3 100000;
    putStrLn "-----------------------------------------EJECUTANDO BENCHMARK N=130000-----------------------------------------";
    benchmark3 130000;
--    benchmark3
}

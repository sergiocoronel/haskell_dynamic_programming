import System.Environment
import  Data.Array
import Criterion.Main

{-
(.) :: (b -> c) -> (a -> b) -> a -> c
(.) f g = \ x -> f (g x)
 -}
fix :: (a -> a) -> a
fix f = let x = f x in x


fib :: (Int -> Integer) -> Int -> Integer
fib f 0 = 1
fib f 1 = 1
fib f n = f (n - 1) + f (n - 2)

memoize :: (Int -> a) -> (Int -> a)
memoize f = (map f [0 ..] !!)

fibMemo :: Int -> Integer
fibMemo = fix (memoize . fib)



main = defaultMain [
    bgroup "fibonacci_fixed" [ 
            bench "fibonacci_fixed" $ whnf (fibMemo) 30
            , bench "fibonacci_fixed" $ whnf (fibMemo) 15000
            , bench "fibonacci_fixed" $ whnf (fibMemo) 50000
            , bench "fibonacci_fixed" $ whnf (fibMemo) 100000
         ]
        ]
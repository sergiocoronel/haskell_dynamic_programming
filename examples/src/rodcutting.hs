import System.Environment
import Criterion.Main
import Data.Array
import Data.Bool
import Data.Char

rodCuttingNaive :: [Int] -> Int -> Int
rodCuttingNaive precios 0 = 0
rodCuttingNaive precios n = maximum [precios !! (x-1) + rodCuttingNaive precios (n-x) | x <- [1..n]]

-- Recibe la lista de valores de los cortes y el largo restante
memo_rodc_list :: [Int] -> Int -> Int
memo_rodc_list precios n = rodTab !! n
  where
    rodTab = map rodc [0..n]
    rodc 0   = 0
    rodc n = maximum [precios !! (x-1) + rodTab !! (n-x) | x <- [1..n]]

memo_rodc_array :: [Int] -> Int -> Int
memo_rodc_array precios n = rodTab ! n
  where
    rodTab = listArray (0,n) [rodc i | i <- [0..n]]
    rodc 0   = 0
    rodc n = maximum [precios !! (x-1) + rodTab ! (n-x) | x <- [1..n]]

--Para ver que devuelvan los resultados correctos
--main = print (rodCutting 8 [1,5,8,9,10,17,17,20] 8)
--main = print (memoized_rodc [1,5,8,9,10,17,17,20] 8 8)

benchmark8 = defaultMain [
    bgroup "rodCutting" [ bench "rodCutting" $ whnf (rodCuttingNaive [1,5,8,9,10,17,17,20]) 8
                        , bench "memo_rodc_list" $ whnf (memo_rodc_list [1,5,8,9,10,17,17,20]) 8
                        , bench "memo_rodc_array" $ whnf (memo_rodc_array [1,5,8,9,10,17,17,20]) 8
                 ]
                ]

benchmark16 = defaultMain [
    bgroup "rodCutting" [ bench "rodCutting" $ whnf (rodCuttingNaive [1,5,8,9,10,17,17,20,22,24,30,28,31,34,36,40]) 16
                        , bench "memo_rodc_list" $ whnf (memo_rodc_list [1,5,8,9,10,17,17,20,22,24,30,28,31,34,36,40]) 16
                        , bench "memo_rodc_array" $ whnf (memo_rodc_array [1,5,8,9,10,17,17,20,22,24,30,28,31,34,36,40]) 16
                 ]
                ]

benchmark26 = defaultMain [
    bgroup "rodCutting" [ bench "rodCutting" $ whnf (rodCuttingNaive [1,5,8,9,10,17,17,20,22,24,30,28,31,34,36,40,42,44,46,48,50,48,50,52,57,60]) 26
                        , bench "memo_rodc_list" $ whnf (memo_rodc_list [1,5,8,9,10,17,17,20,22,24,30,28,31,34,36,40,42,44,46,48,50,48,50,52,57,60]) 26
                        , bench "memo_rodc_array" $ whnf (memo_rodc_array [1,5,8,9,10,17,17,20,22,24,30,28,31,34,36,40,42,44,46,48,50,48,50,52,57,60]) 26
                 ]
                ]

benchmark52 = defaultMain [
    bgroup "rodCutting" [ bench "memo_rodc_list" $ whnf (memo_rodc_list [1,5,8,9,10,17,17,20,22,24,30,28,31,34,36,40,42,44,46,48,50,48,50,52,57,60,63,66,69,72,80,87,56,89,43,10,100,102,106,108,11,156,190,12,13,45,170,199,208,208,209,240]) 52
                        , bench "memo_rodc_array" $ whnf (memo_rodc_array [1,5,8,9,10,17,17,20,22,24,30,28,31,34,36,40,42,44,46,48,50,48,50,52,57,60,63,66,69,72,80,87,56,89,43,10,100,102,106,108,11,156,190,12,13,45,170,199,208,208,209,240]) 52
                 ]
                ]

main = do
{
    putStrLn "----EJECUTANDO BENCHMARK [1,5,8,9,10,17,17,20] 8------------------------------------------------------------------------------";
    benchmark8;
    putStrLn "----EJECUTANDO BENCHMARK [1,5,8,9,10,17,17,20,22,24,30,28,31,34,36,40] 16-----------------------------------------------------";
    benchmark16;
    putStrLn "----EJECUTANDO BENCHMARK [1,5,8,9,10,17,17,20,22,24,30,28,31,34,36,40,42,44,46,48,50,48,50,52,57,60]) 26----------------------";
    benchmark26;
    putStrLn "----EJECUTANDO BENCHMARK [1,5,8,9,10,17,17,20,22,24,30,28,31,34,36,40,42,44,46,48,50,48,50,52,57,60,63,66,69,72,80,87,56,89,43,10,100,102,106,108,11,156,190,12,13,45,170,199,208,208,209,240] 52----------------------";
    benchmark52;
}

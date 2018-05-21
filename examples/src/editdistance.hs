import System.Environment
import Data.Array
import Criterion.Main
import Data.Bool
import Data.Char


editDistance :: Eq a => [a] -> [a] -> Int
editDistance xs ys = dist m n
  where
  (m,n) = (length xs, length ys)
  x = array (1,m) (zip [1..] xs)
  y = array (1,n) (zip [1..] ys)

  dist :: Int -> Int -> Int
  dist 0 j = j
  dist i 0 = i
  dist i j
      | x ! i == y ! j = dist (i-1) (j-1) --Si son iguales pasó a la siguiente letra
      | otherwise = minimum [
                      (dist (i-1) j) + 1,
                      (dist i (j-1)) + 1,
                      1 + dist (i-1) (j-1)
                    ]

editDistance_list :: Eq a => [a] -> [a] -> Int
editDistance_list xs ys = dist m n
  where
  (m,n) = (length xs, length ys)

  dist :: Int -> Int -> Int
  dist 0 j = j
  dist i 0 = i
  dist i j
      | xs !! (i-1) == ys !! (j-1) = dist (i-1) (j-1) --Si son iguales pasó a la siguiente letra
      | otherwise = minimum [
                      (dist (i-1) j) + 1,
                      (dist i (j-1)) + 1,
                      1 + dist (i-1) (j-1)
                    ]

memoeditDistance :: Eq a => [a] -> [a] -> Int
memoeditDistance xs ys = table ! (m,n)
  where
  (m,n) = (length xs, length ys)
  x     = array (1,m) (zip [1..] xs)
  y     = array (1,n) (zip [1..] ys)

  table :: Array (Int,Int) Int
  table = array bnds [(ij, dist ij) | ij <- range bnds]
  bnds  = ((0,0),(m,n))

  dist (0,j) = j
  dist (i,0) = i
  dist (i,j)
      | x ! i == y ! j = table ! (i-1,j-1) --Si son iguales pasó a la siguiente letra
      | otherwise = minimum [
                      table ! (i-1,j) + 1,
                      table ! (i,j-1) + 1,
                      1 + table ! (i-1,j-1)
                    ]

benchmark = defaultMain [
    bgroup "Edit Distance" [
              bench "editDistance" $ whnf (editDistance "Probando") "Proforma"
             , bench "editDistance_list" $ whnf (editDistance_list "Probando") "Proforma"
             , bench "memoeditDistance" $ whnf (memoeditDistance "Probando") "Proforma"
         ]
    ]

benchmark2 = defaultMain [
    bgroup "Edit Distance" [
              bench "editDistance" $ whnf (editDistance "PalabraLarga") "LargaAntena"
             , bench "editDistance_list" $ whnf (editDistance_list "PalabraLarga") "LargaAntena"
             , bench "memoeditDistance" $ whnf (memoeditDistance "PalabraLarga") "LargaAntena"
          ]
    ]

benchmark3 = defaultMain [
    bgroup "Mochila" [
              bench "memoeditDistance" $ whnf (memoeditDistance "Habia una vez un algo") "Habia un algoritmo largo"
          ]
    ]

main = do
{
    putStrLn "----Resultado de comparar: Probando - Proforma---------------------------------------------";
    putStr "Edit Distance: ";
    putStr (show(editDistance "Probando" "Proforma"));
    putStr " - Edit Distance usando List: ";
    putStr (show(editDistance_list "Probando" "Proforma"));
    putStr " - Memo Edit Distance: ";
    putStr (show(memoeditDistance "Probando" "Proforma"));
    putStrLn "";
    putStrLn "----Resultado de comparar: PalabraLarga - LargaAntena---------------------------";
    putStr "Edit Distance: ";
    putStr (show(editDistance "PalabraGrande" "NoEsTanGrandePalabra"));
    putStr " - Edit Distance usando List: ";
    putStr (show(editDistance_list "PalabraGrande" "NoEsTanGrandePalabra"));
    putStr " - Memo Edit Distance: ";
    putStr (show(memoeditDistance "PalabraGrande" "NoEsTanGrandePalabra"));
    putStrLn "";
    putStrLn "----FIN Comparar, a continuación se ejecutaran los benchmark-------------------------------";

    putStrLn "----EJECUTANDO BENCHMARK, comparando: Probando - Proforma--------------------------------------------------------------------------------------";
    benchmark;
    putStrLn "----EJECUTANDO BENCHMARK, comparando: PalabraLarga - LargaAntena-------------------------------------------------------------------------------";
    benchmark2;
    putStrLn "----EJECUTANDO BENCHMARK, comparando: Habia una vez un algo - Habia un algoritmo largo---------------------------------------------------------";
    benchmark3;
}

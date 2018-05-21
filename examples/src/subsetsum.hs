import System.Environment
import Criterion.Main
import Data.Array
import Data.Bool
import Data.Char
import Control.Exception

naiveSubsetSum :: [Int] -> Int -> Bool
naiveSubsetSum _ 0 = True
naiveSubsetSum [] _ = False
naiveSubsetSum (x:xs) s | s >= x = naiveSubsetSum xs (s - x) || naiveSubsetSum xs s
                        | otherwise = naiveSubsetSum xs s

naiveSubsetSum_ind :: [Int] -> Int -> Bool
naiveSubsetSum_ind xs suma = subsetS 0 suma
  where
    n = length xs
    subsetS _ 0          = True
    subsetS i _ | i == n = False
    subsetS i s | s >= xs!!i = subsetS (i+1) (s - xs!!i) || subsetS (i+1) s
                | otherwise  = subsetS (i+1) s

-- Como se admiten negativos la matriz no puede estar acotada en i al valor de sum
memo_subsetSum_list :: [Int] -> Int -> Bool
memo_subsetSum_list arr n = m (numItems*off + n)
  where m = (map (msum) [0 ..] !!)
        numItems = length arr
        off = (n+1)
        msum a
            | i <= 0 = False
            | x == 0 = True
            | arr !! j > x = noUsarItem
            | otherwise = usarItem || noUsarItem
            where   i = a `div` off
                    j = i -1
                    x = a `mod` off
                    noUsarItem = m (j * off + x)
                    usarItem = m (j * off + (x - (arr !! j)))

memo_subsetSum_list_2 :: [Int] -> Int -> Bool
memo_subsetSum_list_2 xs suma = (subsetTab !! 0) !! suma
  where
    m = length xs
    subsetTab = [[ subsetS i s | s <- [0..suma]] | i <- [0..m]]
    subsetS _ 0              = True
    subsetS i _ | i == m     = False
    subsetS i s | s >= xs!!i =  (subsetTab !! (i+1)) !! (s - xs!!i) || (subsetTab !! (i+1)) !! s
                | otherwise  = subsetTab !! (i+1) !! s

-- addErrorInfo info (ErrorCall str) = ErrorCall (str++":"++info)

memo_subsetSum_array :: [Int] -> Int -> Bool
memo_subsetSum_array xs suma = subsetTab ! (0,suma)
  where
    m = length xs
    bnds   = ((0,0),(m,suma))
    subsetTab = array bnds [((i,s), subsetS i s) | (i,s) <- range bnds]
    subsetS _ 0              = True
    subsetS i _ | i == m     = False
    subsetS i s | s >= xs!!i = subsetTab ! (i+1,s - xs!!i) || subsetTab ! (i+1,s)
                | otherwise  = subsetTab ! (i+1,s)

    --  | otherwise  = mapException (addErrorInfo (" ! "++show (i + 1)++" -s- "++show(s))) $ subsetTab ! (i+1,s - xs!!i) || subsetTab ! (i+1,s)

benchmark = defaultMain [
    bgroup "SubsetSum" [
                bench "naiveSubsetSum" $         whnf (naiveSubsetSum        [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4]) 22
                , bench "naiveSubsetSum_ind" $   whnf (naiveSubsetSum_ind    [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4]) 22
                , bench "memo_subsetSum_array" $ whnf (memo_subsetSum_array  [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4]) 22
                , bench "memo_subsetSum_list" $  whnf (memo_subsetSum_list   [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4]) 22
                , bench "memo_subsetSum_list2" $ whnf (memo_subsetSum_list_2 [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4]) 22
         ]
    ]

benchmark2 = defaultMain [
    bgroup "SubsetSum" [
                bench "naiveSubsetSum" $         whnf (naiveSubsetSum        [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4]) 100
                , bench "naiveSubsetSum_ind" $   whnf (naiveSubsetSum_ind    [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4]) 100
                , bench "memo_subsetSum_array" $ whnf (memo_subsetSum_array  [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4]) 100
                , bench "memo_subsetSum_list" $  whnf (memo_subsetSum_list   [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4]) 100
                , bench "memo_subsetSum_list2" $ whnf (memo_subsetSum_list_2 [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4]) 100
          ]
    ]

benchmark3 = defaultMain [
    bgroup "SubsetSum" [
                bench "naiveSubsetSum" $         whnf (naiveSubsetSum        [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4, 8, 34, 12, 10, 7, 5, 45, 5, 15, 40, 20, 2, 3, 1, 5, 7, 4]) 100
                , bench "naiveSubsetSum_ind" $   whnf (naiveSubsetSum_ind    [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4, 8, 34, 12, 10, 7, 5, 45, 5, 15, 40, 20, 2, 3, 1, 5, 7, 4]) 100
                , bench "memo_subsetSum_array" $ whnf (memo_subsetSum_array  [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4, 8, 34, 12, 10, 7, 5, 45, 5, 15, 40, 20, 2, 3, 1, 5, 7, 4]) 100
                , bench "memo_subsetSum_list" $  whnf (memo_subsetSum_list   [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4, 8, 34, 12, 10, 7, 5, 45, 5, 15, 40, 20, 2, 3, 1, 5, 7, 4]) 100
                , bench "memo_subsetSum_list2" $ whnf (memo_subsetSum_list_2 [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4, 8, 34, 12, 10, 7, 5, 45, 5, 15, 40, 20, 2, 3, 1, 5, 7, 4]) 100
          ]
    ]

benchmark4 = defaultMain [
    bgroup "SubsetSum" [
                bench "naiveSubsetSum" $         whnf (naiveSubsetSum        [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4, 8, 34, 12, 10, 7, 5, 45, 5, 15, 40, 20, 2, 3, 1, 5, 7, 4]) 700
--                , bench "naiveSubsetSum_ind" $   whnf (naiveSubsetSum_ind    [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4, 8, 34, 12, 10, 7, 5, 45, 5, 15, 40, 20, 2, 3, 1, 5, 7, 4]) 700
                , bench "memo_subsetSum_array" $ whnf (memo_subsetSum_array  [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4, 8, 34, 12, 10, 7, 5, 45, 5, 15, 40, 20, 2, 3, 1, 5, 7, 4]) 700
                , bench "memo_subsetSum_list" $  whnf (memo_subsetSum_list   [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4, 8, 34, 12, 10, 7, 5, 45, 5, 15, 40, 20, 2, 3, 1, 5, 7, 4]) 700
                , bench "memo_subsetSum_list2" $ whnf (memo_subsetSum_list_2 [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4, 8, 34, 12, 10, 7, 5, 45, 5, 15, 40, 20, 2, 3, 1, 5, 7, 4]) 700
          ]
    ]

benchmark5 = defaultMain [
    bgroup "SubsetSum" [
--                bench "naiveSubsetSum" $          whnf (naiveSubsetSum        [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4, 8, 34, 54, 12, 10, 7, 5, 45, 5, 15, 40, 80, 2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 2, 3, 1, 5, 7, 4, 9, 8, 3, 10]) 700
--                , bench "naiveSubsetSum_ind" $    whnf (naiveSubsetSum_ind    [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4, 8, 34, 54, 12, 10, 7, 5, 45, 5, 15, 40, 80, 2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 2, 3, 1, 5, 7, 4, 9, 8, 3, 10]) 700
                bench "memo_subsetSum_array" $  whnf (memo_subsetSum_array  [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4, 8, 34, 54, 12, 10, 7, 5, 45, 5, 15, 40, 80, 2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 2, 3, 1, 5, 7, 4, 9, 8, 3, 10]) 700
                , bench "memo_subsetSum_list" $   whnf (memo_subsetSum_list   [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4, 8, 34, 54, 12, 10, 7, 5, 45, 5, 15, 40, 80, 2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 2, 3, 1, 5, 7, 4, 9, 8, 3, 10]) 700
                , bench "memo_subsetSum_list_2" $ whnf (memo_subsetSum_list_2 [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4, 8, 34, 54, 12, 10, 7, 5, 45, 5, 15, 40, 80, 2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 2, 3, 1, 5, 7, 4, 9, 8, 3, 10]) 700
         ]
    ]

benchmark6 = defaultMain [
    bgroup "SubsetSum" [
                bench "naiveSubsetSum" $          whnf (naiveSubsetSum        [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4, 8, 34, 29, 12, 10, 7, 5, 19, 5, 15, 30, 20, 2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 2, 3, 1, 5, 7, 4, 9, 8, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4, 8, 34, 29, 12, 10, 7, 5, 19, 5, 15, 30, 20]) 300
--                , bench "naiveSubsetSum_ind" $    whnf (naiveSubsetSum_ind    [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4, 8, 34, 29, 12, 10, 7, 5, 19, 5, 15, 30, 20, 2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 2, 3, 1, 5, 7, 4, 9, 8, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4, 8, 34, 29, 12, 10, 7, 5, 19, 5, 15, 30, 20]) 300
                , bench "memo_subsetSum_array" $  whnf (memo_subsetSum_array  [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4, 8, 34, 29, 12, 10, 7, 5, 19, 5, 15, 30, 20, 2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 2, 3, 1, 5, 7, 4, 9, 8, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4, 8, 34, 29, 12, 10, 7, 5, 19, 5, 15, 30, 20]) 300
                , bench "memo_subsetSum_list" $   whnf (memo_subsetSum_list   [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4, 8, 34, 29, 12, 10, 7, 5, 19, 5, 15, 30, 20, 2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 2, 3, 1, 5, 7, 4, 9, 8, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4, 8, 34, 29, 12, 10, 7, 5, 19, 5, 15, 30, 20]) 300
                , bench "memo_subsetSum_list_2" $ whnf (memo_subsetSum_list_2 [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4, 8, 34, 29, 12, 10, 7, 5, 19, 5, 15, 30, 20, 2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 2, 3, 1, 5, 7, 4, 9, 8, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4, 8, 34, 29, 12, 10, 7, 5, 19, 5, 15, 30, 20]) 300
         ]
    ]

main = do
{
    putStrLn "----Resultado de comparar: [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4] 22---------------------------------------------";
    putStr (show(naiveSubsetSum [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4] 22));
    putStr (show(naiveSubsetSum_ind [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4] 22));
    putStr (show(memo_subsetSum_array [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4] 22));
    putStr (show(memo_subsetSum_list [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4] 22));
    putStr (show(memo_subsetSum_list_2 [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4] 22));
    putStrLn "";
    putStrLn "----Resultado de comparar: [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4] 100---------------------------------------------";
    putStr (show(naiveSubsetSum [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4] 100));
    putStr (show(naiveSubsetSum_ind [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4] 100));
    putStr (show(memo_subsetSum_array [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4] 100));
    putStr (show(memo_subsetSum_list [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4] 100));
    putStr (show(memo_subsetSum_list_2 [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4] 100));
    putStrLn "";
    putStrLn "----Resultado de comparar: [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4, 8, 34, 12, 10, 7, 5, 45, 5, 15, 40, 20, 2, 3, 1, 5, 7, 4] 100---------------------------------------------";
    putStr (show(naiveSubsetSum [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4, 8, 34, 12, 10, 7, 5, 45, 5, 15, 40, 20, 2, 3, 1, 5, 7, 4] 100));
    putStr (show(naiveSubsetSum_ind [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4, 8, 34, 12, 10, 7, 5, 45, 5, 15, 40, 20, 2, 3, 1, 5, 7, 4] 100));
    putStr (show(memo_subsetSum_array [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4, 8, 34, 12, 10, 7, 5, 45, 5, 15, 40, 20, 2, 3, 1, 5, 7, 4] 100));
    putStr (show(memo_subsetSum_list [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4, 8, 34, 12, 10, 7, 5, 45, 5, 15, 40, 20, 2, 3, 1, 5, 7, 4] 100));
    putStr (show(memo_subsetSum_list_2 [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4, 8, 34, 12, 10, 7, 5, 45, 5, 15, 40, 20, 2, 3, 1, 5, 7, 4] 100));
    putStrLn "";
    putStrLn "----Resultado de comparar: [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4, 8, 34, 12, 10, 7, 5, 45, 5, 15, 40, 20, 2, 3, 1, 5, 7, 4] 700---------------------------------------------";
    putStr (show(naiveSubsetSum [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4, 8, 34, 12, 10, 7, 5, 45, 5, 15, 40, 20, 2, 3, 1, 5, 7, 4] 700));
    putStr (show(naiveSubsetSum_ind [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4, 8, 34, 12, 10, 7, 5, 45, 5, 15, 40, 20, 2, 3, 1, 5, 7, 4] 700));
    putStr (show(memo_subsetSum_array [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4, 8, 34, 12, 10, 7, 5, 45, 5, 15, 40, 20, 2, 3, 1, 5, 7, 4] 700));
    putStr (show(memo_subsetSum_list [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4, 8, 34, 12, 10, 7, 5, 45, 5, 15, 40, 20, 2, 3, 1, 5, 7, 4] 700));
    putStr (show(memo_subsetSum_list_2 [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4, 8, 34, 12, 10, 7, 5, 45, 5, 15, 40, 20, 2, 3, 1, 5, 7, 4] 700));
    putStrLn "";
    putStrLn "----Resultado de comparar: [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4, 8, 34, 54, 12, 10, 7, 5, 45, 5, 15, 40, 80, 2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 2, 3, 1, 5, 7, 4, 9, 8] 700---------------------------------------------";
    -- putStr (show(naiveSubsetSum [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4, 8, 34, 54, 12, 10, 7, 5, 45, 5, 15, 40, 80, 2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 2, 3, 1, 5, 7, 4, 9, 8] 700));
    -- putStr (show(naiveSubsetSum_ind [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4, 8, 34, 54, 12, 10, 7, 5, 45, 5, 15, 40, 80, 2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 2, 3, 1, 5, 7, 4, 9, 8] 700));
    putStr (show(memo_subsetSum_array [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4, 8, 34, 54, 12, 10, 7, 5, 45, 5, 15, 40, 80, 2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 2, 3, 1, 5, 7, 4, 9, 8] 700));
    putStr (show(memo_subsetSum_list [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4, 8, 34, 54, 12, 10, 7, 5, 45, 5, 15, 40, 80, 2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 2, 3, 1, 5, 7, 4, 9, 8] 700));
    putStr (show(memo_subsetSum_list_2 [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4, 8, 34, 54, 12, 10, 7, 5, 45, 5, 15, 40, 80, 2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 2, 3, 1, 5, 7, 4, 9, 8] 700));
    putStrLn "";
    putStrLn "----Resultado de comparar: [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4, 8, 34, 29, 12, 10, 7, 5, 19, 5, 15, 30, 20, 2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 2, 3, 1, 5, 7, 4, 9, 8, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4, 8, 34, 29, 12, 10, 7, 5, 19, 5, 15, 30, 20] 1000---------------------------------------------";
    -- putStr (show(naiveSubsetSum [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4, 8, 34, 29, 12, 10, 7, 5, 19, 5, 15, 30, 20, 2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 2, 3, 1, 5, 7, 4, 9, 8, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4, 8, 34, 29, 12, 10, 7, 5, 19, 5, 15, 30, 20] 1000));
    -- putStr (show(naiveSubsetSum_ind [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4, 8, 34, 29, 12, 10, 7, 5, 19, 5, 15, 30, 20, 2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 2, 3, 1, 5, 7, 4, 9, 8, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4, 8, 34, 29, 12, 10, 7, 5, 19, 5, 15, 30, 20] 1000));
    putStr (show(memo_subsetSum_array [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4, 8, 34, 29, 12, 10, 7, 5, 19, 5, 15, 30, 20, 2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 2, 3, 1, 5, 7, 4, 9, 8, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4, 8, 34, 29, 12, 10, 7, 5, 19, 5, 15, 30, 20] 1000));
    putStr (show(memo_subsetSum_list [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4, 8, 34, 29, 12, 10, 7, 5, 19, 5, 15, 30, 20, 2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 2, 3, 1, 5, 7, 4, 9, 8, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4, 8, 34, 29, 12, 10, 7, 5, 19, 5, 15, 30, 20] 1000));
    putStr (show(memo_subsetSum_list_2 [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4, 8, 34, 29, 12, 10, 7, 5, 19, 5, 15, 30, 20, 2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 2, 3, 1, 5, 7, 4, 9, 8, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4, 8, 34, 29, 12, 10, 7, 5, 19, 5, 15, 30, 20] 1000));
    putStrLn "";
    putStrLn "----FIN Comparar, a continuación se ejecutaran los benchmark-------------------------------";

    putStrLn "----EJECUTANDO BENCHMARK [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4] 22---------------------------------------------------------------------------------";
    benchmark;
    putStrLn "----EJECUTANDO BENCHMARK [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4] 100--------------------------------------------------------------------------------";
    benchmark2;
    putStrLn "----EJECUTANDO BENCHMARK [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4, 8, 34, 12, 10, 7, 5, 45, 5, 15, 40, 20, 2, 3, 1, 5, 7, 4] 100----------------------";
    benchmark3;
    putStrLn "----EJECUTANDO BENCHMARK [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4, 8, 34, 12, 10, 7, 5, 45, 5, 15, 40, 20, 2, 3, 1, 5, 7, 4] 700----------------------";
    benchmark4;
    putStrLn "----EJECUTANDO BENCHMARK [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4, 8, 34, 54, 12, 10, 7, 5, 45, 5, 15, 40, 80, 2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 2, 3, 1, 5, 7, 4, 9, 8, 3, 10] 700----------------------";
    benchmark5;
    putStrLn "----EJECUTANDO BENCHMARK [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4, 8, 34, 29, 12, 10, 7, 5, 19, 5, 15, 30, 20, 2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 2, 3, 1, 5, 7, 4, 9, 8, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4, 8, 34, 29, 12, 10, 7, 5, 19, 5, 15, 30, 20]) 1000----------------------";
    benchmark6;
}

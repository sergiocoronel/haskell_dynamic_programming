import System.Environment
import Data.Array
import Criterion.Main
import Data.Bool
import Data.Char

-- En este caso no seria ordenado
knapsack _ 0  = 0
knapsack [] w = 0
knapsack ((vi,wi):xs) w
        | w < wi = knapsack xs w
        | otherwise = max usarItem noUsarItem
            where usarItem   = vi + knapsack xs (w - wi)
                  noUsarItem = knapsack xs w

knapsack_ind :: [(Int,Int)] -> Int -> Int
knapsack_ind items w = knps n w
  where
    n = length items
    knps _ 0  = 0
    knps 0 _  = 0
    knps i w
      | w < wi = noUsarItem
      | otherwise       = max usarItem noUsarItem
          where (vi,wi) = items!! (i - 1)
                usarItem = vi + knps (i-1) (w-wi)
                noUsarItem = knps (i-1) w

memoKnapsack_array :: [(Int, Int)] -> Int -> Int
memoKnapsack_array items w = table ! (n, w)
    where
        n = length items
        bnds  = ((0,0),(n,w))
        table = array bnds [((i,w'), knps i w') | (i,w') <- range bnds]

        knps _ 0 = 0
        knps 0 _ = 0
        knps i j
            | j < wi = noUsarItem
            | otherwise = max usarItem noUsarItem
                where (vi,wi) = items !! (i - 1)
                      usarItem   = vi + table ! (i - 1, j - wi)
                      noUsarItem = table ! (i -1, j)

memoKnapsack_list :: [(Int, Int)] -> Int -> Int
memoKnapsack_list items maxW = m (numItems*off + maxW)
  where numItems = length items
        off = (maxW+1)
        m = (map (knapsack7) [0 ..] !!)

        knapsack7 a
          | i <= 0 = 0
          | w == 0 = 0
          | snd(items !! j) > w  = m (j * off + w)
          | otherwise = max (m (j * off + w))
                            (m (j * off + (w - snd(items !! j))) + fst(items !! j))
          where i = a `div` off
                j = i -1
                w = a `mod` off

memoKnapsack_list_2 :: [(Int,Int)] -> Int -> Int
memoKnapsack_list_2 items w = (kTab !! n) !! w
  where
    n = length items
    kTab = [[ knps i w' | w' <- [0..w]] | i <- [0..n]]
    knps _ 0 = 0
    knps 0 _ = 0
    knps i w
      | w < wi = noUsarItem
      | otherwise = max usarItem noUsarItem
        where (vi,wi) = items!!(i-1)
              usarItem   = vi + (kTab !! (i-1)) !! (w-wi)
              noUsarItem = (kTab !! (i-1)) !! w

benchmark = defaultMain [
    bgroup "Mochila" [
                 bench "knapsack" $ whnf (knapsack [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4)]) 15
                 , bench "knapsack_ind" $ whnf (knapsack_ind [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4)]) 15
                 , bench "memoKnapsack_array" $ whnf (memoKnapsack_array [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4)]) 15
                 , bench "memoKnapsack_list" $ whnf (memoKnapsack_list [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4)]) 15
                 , bench "memoKnapsack_list_2" $ whnf (memoKnapsack_list_2 [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4)]) 15
         ]
    ]

benchmark2 = defaultMain [
    bgroup "Mochila" [
              bench "knapsack" $ whnf (knapsack [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (4, 12), (2, 2), (2, 1), (1, 1), (10, 4)]) 50
              , bench "knapsack_ind" $ whnf (knapsack_ind [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (4, 12), (2, 2), (2, 1), (1, 1), (10, 4)]) 50
              , bench "memoKnapsack_array" $ whnf (memoKnapsack_array [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (4, 12), (2, 2), (2, 1), (1, 1), (10, 4)]) 50
              , bench "memoKnapsack_list" $ whnf (memoKnapsack_list [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (4, 12), (2, 2), (2, 1), (1, 1), (10, 4)]) 50
              , bench "memoKnapsack_list_2" $ whnf (memoKnapsack_list_2 [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (4, 12), (2, 2), (2, 1), (1, 1), (10, 4)]) 50
          ]
    ]

benchmark3 = defaultMain [
    bgroup "Mochila" [
              bench "knapsack" $ whnf (knapsack [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (5, 5), (3,8), (1,1), (9,1), (7, 3), (5, 5), (3,8), (1,1), (9,1), (7, 3)]) 100
              , bench "knapsack_ind" $ whnf (knapsack_ind [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (5, 5), (3,8), (1,1), (9,1), (7, 3), (5, 5), (3,8), (1,1), (9,1), (7, 3)]) 100
              , bench "memoKnapsack_array" $ whnf (memoKnapsack_array [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (5, 5), (3,8), (1,1), (9,1), (7, 3), (5, 5), (3,8), (1,1), (9,1), (7, 3)]) 100
              , bench "memoKnapsack_list" $ whnf (memoKnapsack_list [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (5, 5), (3,8), (1,1), (9,1), (7, 3), (5, 5), (3,8), (1,1), (9,1), (7, 3)]) 100
              , bench "memoKnapsack_list_2" $ whnf (memoKnapsack_list_2 [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (5, 5), (3,8), (1,1), (9,1), (7, 3), (5, 5), (3,8), (1,1), (9,1), (7, 3)]) 100
          ]
    ]

benchmark4 = defaultMain [
    bgroup "Mochila" [
              bench "memoKnapsack_array" $ whnf (memoKnapsack_array [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (5, 5), (3,8), (1,1), (9,1), (7, 3), (5, 5), (3,8), (1,1), (9,1), (7, 3), (10, 4), (5, 5), (3,8), (1,1), (9,1), (7, 3), (5, 5), (3,8), (1,1), (9,1), (7, 3)]) 140
              , bench "memoKnapsack_list" $ whnf (memoKnapsack_list [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (5, 5), (3,8), (1,1), (9,1), (7, 3), (5, 5), (3,8), (1,1), (9,1), (7, 3), (10, 4), (5, 5), (3,8), (1,1), (9,1), (7, 3), (5, 5), (3,8), (1,1), (9,1), (7, 3)]) 140
              , bench "memoKnapsack_list_2" $ whnf (memoKnapsack_list_2 [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (5, 5), (3,8), (1,1), (9,1), (7, 3), (5, 5), (3,8), (1,1), (9,1), (7, 3), (10, 4), (5, 5), (3,8), (1,1), (9,1), (7, 3), (5, 5), (3,8), (1,1), (9,1), (7, 3)]) 140
          ]
    ]

main = do
{
    putStrLn "----EJECUTANDO BENCHMARK [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4)] 15--------------------------------------------------------------------------------------";
    benchmark;
    putStrLn "----EJECUTANDO BENCHMARK [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (4, 12), (2, 2), (2, 1), (1, 1), (10, 4)] 50--";
    benchmark2;
    putStrLn "----EJECUTANDO BENCHMARK [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (5, 5), (3,8), (1,1), (9,1), (7, 3), (5, 5), (3,8), (1,1), (9,1), (7, 3)] 100-";
    benchmark3;
    putStrLn "----EJECUTANDO BENCHMARK [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (5, 5), (3,8), (1,1), (9,1), (7, 3), (5, 5), (3,8), (1,1), (9,1), (7, 3), (10, 4), (5, 5), (3,8), (1,1), (9,1), (7, 3), (5, 5), (3,8), (1,1), (9,1), (7, 3)]) 140-";
    benchmark4;
}

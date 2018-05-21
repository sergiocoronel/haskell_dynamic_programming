import System.Environment
import Data.Vector as V
import Data.Matrix as M
import Data.Array as A
import Criterion.Main


memoKnapsack_matrix :: [(Int, Int)] -> Int -> Int
memoKnapsack_matrix items w = m M.! (n, maxw1)
    where
        n = Prelude.length items
        m = M.fromList n maxw1 $
              [knps i j | (i, j) <- range bnds]
        bnds  = ((1,1),(n, maxw1))
        maxw1 = w + 1
        knps i j
            | i == 1 && wi <= j = vi
            | i == 1 && wi > j = 0
            | wi > j  = noUsarItem
            | otherwise = max usarItem noUsarItem
                where i' = i - 1
                      (vi,wi) = items !! i'
                      usarItem   = vi + m M.! (i', j-wi)
                      noUsarItem = m M.! (i', j)

memoKnapsack_matrix2 :: [(Int, Int)] -> Int -> Int
memoKnapsack_matrix2 items w = unsafeGet n maxw1 m
    where
        n = Prelude.length items
        m = M.fromList n maxw1 $
              [knps i j | (i, j) <- range bnds]
        bnds  = ((1,1),(n, maxw1))
        maxw1 = w + 1
        knps i j
            | i == 1 && wi <= j = vi
            | i == 1 && wi > j = 0
            | wi > j  = noUsarItem
            | otherwise = max usarItem noUsarItem
                where i' = i - 1
                      (vi,wi) = items !! i'
                      usarItem   = vi + unsafeGet i' (j-wi) m
                      noUsarItem = unsafeGet i' j m

memoKnapsack_matrix3 :: [(Int, Int)] -> Int -> Int
memoKnapsack_matrix3 items w = unsafeGet n maxw1 m
    where
        n = Prelude.length items
        m = M.matrix n maxw1 $
              \(i,j) -> knps i j
        bnds  = ((1,1),(n, maxw1))
        maxw1 = w + 1
        knps i j
            | i == 1 && wi <= j = vi
            | i == 1 && wi > j = 0
            | wi > j  = noUsarItem
            | otherwise = max usarItem noUsarItem
                where i' = i - 1
                      (vi,wi) = items !! i'
                      usarItem   = vi + unsafeGet i' (j-wi) m
                      noUsarItem = unsafeGet i' j m

memoKnapsack_matrix4 :: [(Int, Int)] -> Int -> Int
memoKnapsack_matrix4 items w = unsafeGet n maxw1 m
    where
        n = Prelude.length items
        itemVector = V.unsafeInit $ V.fromList items
        m = M.matrix n maxw1 $
              \(i,j) -> knps i j
        bnds  = ((1,1),(n, maxw1))
        maxw1 = w + 1
        knps i j
            | i == 1 && wi <= j = vi
            | i == 1 && wi > j = 0
            | wi > j  = noUsarItem
            | otherwise = max usarItem noUsarItem
                where i' = i - 1
                      (vi,wi) = unsafeIndex itemVector i'
                      usarItem   = vi + unsafeGet i' (j-wi) m
                      noUsarItem = unsafeGet i' j m

memoKnapsack_matrix5 :: [(Int, Int)] -> Int -> Int
memoKnapsack_matrix5 items w = unsafeGet n maxw1 m
    where
        n = Prelude.length items
        m = M.matrix n maxw1 $
              \(i,j) -> knps (items !! (i - 1)) i j
        bnds  = ((1,1),(n, maxw1))
        maxw1 = w + 1
        knps (vi,wi) i j
            | i == 1 && wi <= j = vi
            | i == 1 && wi > j = 0
            | wi > j  = noUsarItem
            | otherwise = max usarItem noUsarItem
                where i' = i - 1
                      usarItem   = vi + unsafeGet i' (j-wi) m
                      noUsarItem = unsafeGet i' j m


benchmark xs n = defaultMain [
    bgroup "Mochila" [
                 bench "memoKnapsack_matrix" $ whnf (memoKnapsack_matrix xs) n
                 , bench "memoKnapsack_matrix2" $ whnf (memoKnapsack_matrix2 xs) n
                 , bench "memoKnapsack_matrix3" $ whnf (memoKnapsack_matrix3 xs) n
--                 , bench "memoKnapsack_matrix4" $ whnf (memoKnapsack_matrix4 xs) n
                 , bench "memoKnapsack_matrix5" $ whnf (memoKnapsack_matrix5 xs) n
         ]
    ]

main = do
{
    putStrLn "----EJECUTANDO BENCHMARK [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4)] 15--------------------------------------------------------------------------------------";
    benchmark [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4)] 15;
    putStrLn "----EJECUTANDO BENCHMARK [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (4, 12), (2, 2), (2, 1), (1, 1), (10, 4)] 50--";
    benchmark [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (4, 12), (2, 2), (2, 1), (1, 1), (10, 4)] 50;
    putStrLn "----EJECUTANDO BENCHMARK [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (5, 5), (3,8), (1,1), (9,1), (7, 3), (5, 5), (3,8), (1,1), (9,1), (7, 3)] 100-";
    benchmark [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (5, 5), (3,8), (1,1), (9,1), (7, 3), (5, 5), (3,8), (1,1), (9,1), (7, 3)] 100;
    putStrLn "----EJECUTANDO BENCHMARK [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (5, 5), (3,8), (1,1), (9,1), (7, 3), (5, 5), (3,8), (1,1), (9,1), (7, 3), (10, 4), (5, 5), (3,8), (1,1), (9,1), (7, 3), (5, 5), (3,8), (1,1), (9,1), (7, 3)]) 140-";
    benchmark [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4),(4, 12), (2, 2), (2, 1), (1, 1), (10, 4), (5, 5), (3,8), (1,1), (9,1), (7, 3), (5, 5), (3,8), (1,1), (9,1), (7, 3), (10, 4), (5, 5), (3,8), (1,1), (9,1), (7, 3), (5, 5), (3,8), (1,1), (9,1), (7, 3)] 140;
}

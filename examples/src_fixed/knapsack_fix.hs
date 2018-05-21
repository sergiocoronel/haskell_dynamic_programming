import System.Environment
import Data.Array
import Criterion.Main
import Criterion.Main.Options


-- | fixpoint

fix :: (a -> a) -> a
fix f = let x = f x in x

-- | Tabla con listas

data Table_L i a = T [a] (i,i)

mkTable_L :: Ix i => (i,i) -> (i -> a) -> Table_L i a
mkTable_L bnds f = T [f i | i <- range bnds] bnds

(.!!.) :: Ix i => Table_L i a -> i -> a
(T t bnds) .!!. i = t !! (index bnds i)

-- Tabla con arrays

type Table_A i a = Array i a

mkTable_A :: Ix i => (i,i) -> (i -> a) -> Table_A i a
mkTable_A bnds f = listArray bnds [f i | i <- range bnds]

(.!.) :: Ix i => Table_A i a -> i -> a
t .!. i = t ! i

-- Tabla con lista de listas

data Table_LL i a = LL [[a]] (i,i)
     -- en las funciones los indices van a ser pares
     -- en el tipo guardamos los limites del intervalo
     -- esto es necesario para el lookup

mkTable_LL :: (Ix i,Ix i')
           => ((i,i'),(i,i')) -> ((i,i') -> a) -> Table_LL (i,i') a
mkTable_LL bnds@((i,j),(k,l)) f
    = LL [[(f (x,y)) | y <- range (j,l)] | x <- range (i,k)] bnds

(.!!!.) :: (Ix i,Ix i') => Table_LL (i,i') a -> (i,i') -> a
(LL t ((i,j),(k,l))) .!!!. (x,y) = t !! index (i,k) x !! index (j,l) y


-- | Knapsack

knapsack :: [(Int,Int)] -> Int -> Int
knapsack _ 0  = 0
knapsack [] w = 0
knapsack ((vi,wi):is) w
   | w < wi    = knapsack is w
   | otherwise = max (vi + knapsack is (w-wi)) (knapsack is w)

-- version modificada con indices

knapsack' :: [(Int,Int)] -> Int -> Int
knapsack' is w = knps 0 w
  where
    m = length is
    knps _ 0          = 0
    knps i _ | i == m = 0
    knps i w
      | w < snd (is!!i) = knps (i+1) w
      | otherwise       = let (vi,wi) = is!!i
                          in max (vi + knps (i+1) (w-wi)) (knps (i+1) w)

-- Definicion en terminos de fixpoint
-- Debemos hacer que la definicion local sea uncurry para poder
-- aplicar despues tabulacion usando el funcional phiK. Notar que la
-- funcion que aplica mkTable para armar la tabla es uncurry.

knapsack'' :: [(Int,Int)] -> Int -> Int
knapsack'' is w = knps (0,w)
  where
    m = length is
    knps = fix (phiK is m)

phiK is m f (_,0)          = 0
phiK is m f (i,_) | i == m = 0
phiK is m f (i,w)
      | w < snd (is!!i) = f (i+1,w)
      | otherwise       = let (vi,wi) = is!!i
                          in max (vi + f (i+1,w-wi)) (f (i+1,w))

-- Tabulacion con listas

knapsackT_LL :: [(Int,Int)] -> Int -> Int
knapsackT_LL is w = t .!!!. (0,w)
  where
    m = length is
    t = mkTable_LL ((0,0),(m,w)) (phiK is m (t .!!!.))

knapsackT_A :: [(Int,Int)] -> Int -> Int
knapsackT_A is w = t .!. (0,w)
  where
    m = length is
    t = mkTable_A ((0,0),(m,w)) (phiK is m (t .!.))


benchmark xs n= defaultMain [
    bgroup "Mochila" [
                 bench "knapsackT_LL" $ whnf (knapsackT_LL xs) n
                 , bench "knapsackT_A" $ whnf (knapsackT_A xs) n
         ]
    ]

benchmark2 xs n= defaultMain [
    bgroup "Mochila" [
               bench "knapsackT_A" $ whnf (knapsackT_A xs) n
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

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


-- | Subset sum

subsetSum :: [Int] -> Int -> Bool
subsetSum _ 0  = True
subsetSum [] _ = False
subsetSum (x:xs) s | s >= x    = subsetSum xs (s-x) || subsetSum xs s
                   | otherwise = subsetSum xs s

-- version modificada con indices

subsetSum' :: [Int] -> Int -> Bool
subsetSum' xs sum = ssS 0 sum
  where
    m = length xs
    ssS _ 0              = True
    ssS i _ | i == m     = False
    ssS i s | s >= xs!!i = ssS (i+1) (s - xs!!i) || ssS (i+1) s
            | otherwise  = ssS (i+1) s

-- Definicion en terminos de fixpoint
-- Debemos hacer que la definicion local sea uncurry para poder
-- aplicar despues tabulacion usando el funcional phiS. Notar que la
-- funcion que aplica mkTable para armar la tabla es uncurry.

subsetSum'' :: [Int] -> Int -> Bool
subsetSum'' xs sum = ssS (0,sum)
  where
    m = length xs
    ssS = fix (phiS xs m)

-- Funcional

phiS xs m f (_,0)              = True
phiS xs m f (i,_) | i == m     = False
phiS xs m f (i,s) | s >= xs!!i = f (i+1,s-xs!!i) || f (i+1,s)
                  | otherwise  = f (i+1,s)

-- Definicion con tabulacion

subsetSumT_LL :: [Int] -> Int -> Bool
subsetSumT_LL xs sum = t .!!!. (0,sum)
  where
    m = length xs
    t = mkTable_LL ((0,0),(m,sum)) (phiS xs m (t .!!!.))

subsetSumT_A :: [Int] -> Int -> Bool
subsetSumT_A xs sum = t .!. (0,sum)
  where
    m = length xs
    t = mkTable_A ((0,0),(m,sum)) (phiS xs m (t .!.))


benchmark xs n = defaultMain [
    bgroup "SubsetSum" [
                bench "subsetSumT_LL" $  whnf (subsetSumT_LL xs) n
                , bench "subsetSumT_A" $ whnf (subsetSumT_A xs ) n
         ]
    ]

main = do
{
    putStrLn "----EJECUTANDO BENCHMARK [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4] 22---------------------------------------------------------------------------------";
    benchmark [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4] 22;
    putStrLn "----EJECUTANDO BENCHMARK [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4] 100--------------------------------------------------------------------------------";
    benchmark [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4] 100;
    putStrLn "----EJECUTANDO BENCHMARK [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4, 8, 34, 12, 10, 7, 5, 45, 5, 15, 40, 20, 2, 3, 1, 5, 7, 4] 100----------------------";
    benchmark [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4, 8, 34, 12, 10, 7, 5, 45, 5, 15, 40, 20, 2, 3, 1, 5, 7, 4] 100;
    putStrLn "----EJECUTANDO BENCHMARK [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4, 8, 34, 12, 10, 7, 5, 45, 5, 15, 40, 20, 2, 3, 1, 5, 7, 4] 700----------------------";
    benchmark [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4, 8, 34, 12, 10, 7, 5, 45, 5, 15, 40, 20, 2, 3, 1, 5, 7, 4] 700;
    putStrLn "----EJECUTANDO BENCHMARK [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4, 8, 34, 54, 12, 10, 7, 5, 45, 5, 15, 40, 80, 2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 2, 3, 1, 5, 7, 4, 9, 8, 3, 10] 700----------------------";
    benchmark [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4, 8, 34, 54, 12, 10, 7, 5, 45, 5, 15, 40, 80, 2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 2, 3, 1, 5, 7, 4, 9, 8, 3, 10] 700;
    putStrLn "----EJECUTANDO BENCHMARK [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4, 8, 34, 29, 12, 10, 7, 5, 19, 5, 15, 30, 20, 2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 2, 3, 1, 5, 7, 4, 9, 8, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4, 8, 34, 29, 12, 10, 7, 5, 19, 5, 15, 30, 20]) 1000----------------------";
    benchmark [2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4, 8, 34, 29, 12, 10, 7, 5, 19, 5, 15, 30, 20, 2, 3, 1, 5, 7, 4, 9, 8, 3, 10, 2, 3, 1, 5, 7, 4, 9, 8, 3, 1, 5, 7, 4, 9, 8, 3, 10, 4, 8, 34, 29, 12, 10, 7, 5, 19, 5, 15, 30, 20] 1000;
}

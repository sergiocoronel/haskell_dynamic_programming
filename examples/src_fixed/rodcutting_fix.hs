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


-- | Rod cutting

rodCutting :: [Int] -> Int -> Int
rodCutting ps = rodC
  where
    rodC 0 = 0
    rodC n = maximum [ps !! (p-1) + rodC (n-p) | p <- [1..n]]

-- Definicion en terminos de fixpoint

rodCuttingF ps = fix (phiRC ps)

-- Funcional

phiRC :: [Int] -> (Int -> Int) -> (Int -> Int)
phiRC ps f 0 = 0
phiRC ps f n = maximum [ps !! (p-1) + f (n-p) | p <- [1..n]]

-- Definicion con tabulacion

rodCuttingT_L ps n = t .!!. n
  where t = mkTable_L (0,n) (phiRC ps (t .!!.))

rodCuttingT_A ps n = t .!. n
  where t = mkTable_A (0,n) (phiRC ps (t .!.))


benchmark xs n = defaultMain [
    bgroup "rodCutting" [ bench "rodCuttingT_L" $ whnf (rodCuttingT_L xs) n
                        , bench "rodCuttingT_L" $ whnf (rodCuttingT_A xs) n
                 ]
                ]

main = do
{
    putStrLn "----EJECUTANDO BENCHMARK [1,5,8,9,10,17,17,20] 8------------------------------------------------------------------------------";
    benchmark [1,5,8,9,10,17,17,20] 8 ;
    putStrLn "----EJECUTANDO BENCHMARK [1,5,8,9,10,17,17,20,22,24,30,28,31,34,36,40] 16-----------------------------------------------------";
    benchmark [1,5,8,9,10,17,17,20,22,24,30,28,31,34,36,40] 16 ;
    putStrLn "----EJECUTANDO BENCHMARK [1,5,8,9,10,17,17,20,22,24,30,28,31,34,36,40,42,44,46,48,50,48,50,52,57,60]) 26----------------------";
    benchmark [1,5,8,9,10,17,17,20,22,24,30,28,31,34,36,40,42,44,46,48,50,48,50,52,57,60] 26;
    putStrLn "----EJECUTANDO BENCHMARK [1,5,8,9,10,17,17,20,22,24,30,28,31,34,36,40,42,44,46,48,50,48,50,52,57,60,63,66,69,72,80,87,56,89,43,10,100,102,106,108,11,156,190,12,13,45,170,199,208,208,209,240] 52----------------------";
    benchmark [1,5,8,9,10,17,17,20,22,24,30,28,31,34,36,40,42,44,46,48,50,48,50,52,57,60,63,66,69,72,80,87,56,89,43,10,100,102,106,108,11,156,190,12,13,45,170,199,208,208,209,240] 52;
}

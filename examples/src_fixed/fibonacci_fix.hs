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

-- | Fibonacci

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- Definicion por fixpoint

fibF = fix phiF

-- Funcional de fibonacci

phiF :: (Integer -> Integer) -> (Integer -> Integer)
phiF f 0 = 0
phiF f 1 = 1
phiF f n = f (n-1) + f (n-2)

-- Primer alternativa
-- Esta solucion es ineficiente, genera una nueva tabla en cada llamada

-- listas
fibT0_L :: Integer -> Integer
fibT0_L n = fix (table_L (0,n) . phiF) n

table_L bnds f i = (t .!!. i)
  where t = mkTable_L bnds f

-- arrays
fibT0_A :: Integer -> Integer
fibT0_A n = fix (table_A (0,n) . phiF) n

table_A bnds f i = (t .!. i)
  where t = mkTable_A bnds f

-- Forma alternativa (el fixpoint debe estar focalizado en la tabla)

-- Introduciendo la tabla

fibT1_L n = t .!!. n
  where
    t = mkTable_L (0,n)
                  (\i -> case i of
                           0 -> 0
                           1 -> 1
                           n -> fibT1_L (n-1) + fibT1_L (n-2))

fibT1_A n = t .!. n
  where
    t = mkTable_A (0,n)
                  (\i -> case i of
                           0 -> 0
                           1 -> 1
                           n -> fibT1_A (n-1) + fibT1_A (n-2))

-- Sustituir la funcion directamente por el funcional phiF

fibT2_L n = t .!!. n
  where
    t = mkTable_L (0,n) (phiF fibT2_L)

fibT2_A n = t .!. n
  where
    t = mkTable_A (0,n) (phiF fibT2_A)

-- Referenciando directamente la tabla (es mucho mas rapido)

fibT3_L n = t .!!. n
  where
    t = mkTable_L (0,n) (phiF (t .!!.))

fibT3_A n = t .!. n
  where
    t = mkTable_A (0,n) (phiF (t .!.))

-- Definiendo la tabla con el operador de fixpoint

fibT4_L n = t .!!. n
  where
    t = fix (\u -> mkTable_L (0,n) (phiF (u .!!.)))

fibT4_A n = t .!. n
  where
    t = fix (\u -> mkTable_A (0,n) (phiF (u .!.)))


-- Benchmarking
benchmark n = defaultMain [
    bgroup "fibonacci" [
                   bench "fibT4_L" $ whnf (fibT4_L) n
                  , bench "fibT4_A" $ whnf (fibT4_A) n
                 ]
                ]

benchmark2 n = defaultMain [
    bgroup "fibonacci" [
                    bench "fibT4_A" $ whnf (fibT4_A) n
                 ]
                ]

main = do
{
    putStrLn "-----------------------------------------EJECUTANDO BENCHMARK N=40-----------------------------------------";
    benchmark 40;
    putStrLn "-----------------------------------------EJECUTANDO BENCHMARK N=15000-----------------------------------------";
    benchmark 15000;
    putStrLn "-----------------------------------------EJECUTANDO BENCHMARK N=50000-----------------------------------------";
    benchmark2 50000;
    putStrLn "-----------------------------------------EJECUTANDO BENCHMARK N=100000-----------------------------------------";
    benchmark2 100000;
    putStrLn "-----------------------------------------EJECUTANDO BENCHMARK N=130000-----------------------------------------";
    benchmark2 130000;
--    benchmark3
}

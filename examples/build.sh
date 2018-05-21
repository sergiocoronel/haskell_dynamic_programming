ghc -O2 -funfolding-use-threshold=16 -outputdir executables/tmp_fib -o executables/fibonacci --make src/fibonacci.hs

ghc -O2 -funfolding-use-threshold=16 -outputdir executables/tmp_rodc -o executables/rodcutting --make src/rodcutting.hs

ghc -O2 -funfolding-use-threshold=16 -outputdir executables/tmp_subsets -o executables/subsetsum --make src/subsetsum.hs

ghc -O2 -funfolding-use-threshold=16 -outputdir executables/tmp_knaps -o executables/knapsack --make src/knapsack.hs

ghc -O2 -funfolding-use-threshold=16 -outputdir executables/tmp_editDist -o executables/editdistance --make src/editdistance.hs

ghc -O2 -funfolding-use-threshold=16 -outputdir executables/tmp_fibOpt -o executables/fibonacci_optimizaciones --make src/fibonacci_optimizaciones.hs

ghc -O2 -funfolding-use-threshold=16 -outputdir executables/tmp_knapsOpt -o executables/knapsack_optimizaciones --make src/knapsack_optimizaciones.hs

ghc -O2 -funfolding-use-threshold=16 -outputdir executables/tmp_knapsOpt2 -o executables/knapsack_optimizaciones_parte2 --make src/knapsack_optimizaciones_parte2.hs

ghc -O2 -funfolding-use-threshold=16 -outputdir executables/tmp_fibfix -o executables/fibonacci_fix --make src_fixed/fibonacci_fix.hs

ghc -O2 -funfolding-use-threshold=16 -outputdir executables/tmp_knapfix -o executables/knapsack_fix --make src_fixed/knapsack_fix.hs

ghc -O2 -funfolding-use-threshold=16 -outputdir executables/tmp_rodcfix -o executables/rodcutting_fix --make src_fixed/rodcutting_fix.hs

ghc -O2 -funfolding-use-threshold=16 -outputdir executables/tmp_subssfix -o executables/subsetsum_fix --make src_fixed/subsetsum_fix.hs

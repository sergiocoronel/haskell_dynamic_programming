echo Compilando fibonacci...

ghc -O2 -outputdir executables/tmp_fib_control_monad_memo -o executables/fibonacci_control_monad_memo --make src/fibonacci_control_monad_memo.hs

ghc -O2 -outputdir executables/tmp_fib_data_function_memo -o executables/fibonacci_data_function_memo --make src/fibonacci_data_function_memo.hs

ghc -O2 -outputdir executables/tmp_fib_data_memoTrie -o executables/fibonacci_data_memoTrie --make src/fibonacci_data_memoTrie.hs

ghc -O2 -outputdir executables/tmp_fib_memo_mixins -o executables/fibonacci_memo_mixins --make src/fibonacci_memo_mixins.hs

echo Compilando knapsack...

ghc -O2 -outputdir executables/tmp_knaps_control_monad_memo -o executables/knapsack_control_monad_memo --make src/knapsack_control_monad_memo.hs

ghc -O2 -outputdir executables/tmp_knaps_data_function_memo -o executables/knapsack_data_function_memo --make src/knapsack_data_function_memo.hs

ghc -O2 -outputdir executables/tmp_knaps_data_memoTrie -o executables/knapsack_data_memoTrie --make src/knapsack_data_memoTrie.hs

ghc -O2 -outputdir executables/tmp_knaps_memo_mixins -o executables/knapsack_memo_mixins --make src/knapsack_memo_mixins.hs

echo fin...

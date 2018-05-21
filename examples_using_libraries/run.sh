if [ $1 = "fibonacci" ]; then
      ./executables/fibonacci_control_monad_memo.exe > ./output/fibonacci_control_monad_memo.log
      ./executables/fibonacci_data_function_memo.exe > ./output/fibonacci_data_function_memo.log
      ./executables/fibonacci_data_memoTrie.exe > ./output/fibonacci_data_memoTrie.log
      ./executables/fibonacci_memo_mixins.exe > ./output/fibonacci_memo_mixins.log
elif [ $1 = "knapsack" ]; then
      ./executables/knapsack_control_monad_memo.exe > ./output/knapsack_control_monad_memo.log
      ./executables/knapsack_data_function_memo.exe > ./output/knapsack_data_function_memo.log
      ./executables/knapsack_data_memoTrie.exe > ./output/knapsack_data_memoTrie.log
      ./executables/knapsack_memo_mixins.exe > ./output/knapsack_memo_mixins.log
elif [ $1 = "all" ]; then
      ./executables/fibonacci_control_monad_memo.exe > ./output/fibonacci_control_monad_memo.log
      ./executables/fibonacci_data_function_memo.exe > ./output/fibonacci_data_function_memo.log
      ./executables/fibonacci_data_memoTrie.exe > ./output/fibonacci_data_memoTrie.log
      ./executables/fibonacci_memo_mixins.exe > ./output/fibonacci_memo_mixins.log
      ./executables/knapsack_control_monad_memo.exe > ./output/knapsack_control_monad_memo.log
      ./executables/knapsack_data_function_memo.exe > ./output/knapsack_data_function_memo.log
      ./executables/knapsack_data_memoTrie.exe > ./output/knapsack_data_memoTrie.log
      ./executables/knapsack_memo_mixins.exe > ./output/knapsack_memo_mixins.log
else
  echo "Parametro de entrada no valido [fibonacci|knapsack|all]"
fi

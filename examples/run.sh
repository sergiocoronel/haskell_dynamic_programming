if [ $1 = "fibonacci" ]; then
      ./executables/fibonacci.exe > ./output/fibonacci_output.log
elif [ $1 = "rodcutting" ]; then
      ./executables/rodcutting.exe > ./output/rodcutting_output.log
elif [ $1 = "subsetsum" ]; then
      ./executables/subsetsum.exe > ./output/subsetsum_output.log
elif [ $1 = "knapsack" ]; then
      ./executables/knapsack.exe > ./output/knapsack_output.log
elif [ $1 = "editdistance" ]; then
      ./executables/editdistance.exe > ./output/editdistance_output.log
elif [ $1 = "all_parte1" ]; then
      ./executables/fibonacci.exe > ./output/fibonacci_output.log
      ./executables/rodcutting.exe > ./output/rodcutting_output.log
      ./executables/subsetsum.exe > ./output/subsetsum_output.log
      ./executables/knapsack.exe > ./output/knapsack_output.log
      ./executables/editdistance.exe > ./output/editdistance_output.log
elif [ $1 = "all" ]; then
      ./executables/fibonacci.exe > ./output/fibonacci_output.log
      ./executables/rodcutting.exe > ./output/rodcutting_output.log
      ./executables/subsetsum.exe > ./output/subsetsum_output.log
      ./executables/knapsack.exe > ./output/knapsack_output.log
      ./executables/editdistance.exe > ./output/editdistance_output.log
      ./executables/fibonacci_optimizaciones.exe > ./output/fibonacci_optimizaciones_output.log
      ./executables/knapsack_optimizaciones.exe > ./output/knapsack_optimizaciones_output.log
      ./executables/knapsack_optimizaciones_parte2.exe > ./output/knapsack_optimizaciones_parte2_output.log
elif [ $1 = "optimizaciones" ]; then
	  ./executables/editdistance.exe > ./output/editdistance_output.log
      ./executables/fibonacci_optimizaciones.exe > ./output/fibonacci_optimizaciones_output.log
      ./executables/knapsack_optimizaciones.exe > ./output/knapsack_optimizaciones_output.log
      ./executables/knapsack_optimizaciones_parte2.exe > ./output/knapsack_optimizaciones_parte2_output.log
elif [ $1 = "all_fix" ]; then
      ./executables/fibonacci_fix.exe > ./output/fix/fibonacci_fix.log
      ./executables/knapsack_fix.exe > ./output/fix/knapsack_fix.log
      ./executables/rodcutting_fix.exe > ./output/fix/rodcutting_fix.log
      ./executables/subsetsum_fix.exe > ./output/fix/subsetsum_fix.log
else
  echo "Parametro de entrada no valido [fibonacci|rodcutting|subsetsum|knapsack|editdistance|all_parte1|all_fix|optimizaciones|all]"
fi

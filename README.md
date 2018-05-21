# haskell_dynamic_programming
Haskell examples in dynamic programming

This project consists of multiple example implementations of Memoization for Dynamic Programming problems using Haskell:

Fibonacci

Rod Cutting

Subset Sum

Knapsack

Edit Distance


-----------------
Project Structure:

--examples/      -- Custom Implementation without libraries to resolve 

executables/     -- In this folder are placed the executables generated with build.sh

output/          -- In this folder are placed the output generated with run.sh

fix/             -- In this folder are placed the logs generated with the version that uses fixed_point

src/             -- Source Code

src_fixed/       -- Source Code that uses fixed point

build.sh         -- script that compiles and creates the executables, no parameters received

clean.sh         -- script que deteletes all the logs and executables

run.sh           -- script that runs the executables and saves the results in the output folder



--examples_using_libraries/

executables/

output/

fix/

src/

src_fixed/

build.sh

clean.sh

run.sh


The libraries used are Data.MemoTrie, Data.Function.Memoize, Control.Monad.Memo, Custom implementation of Memoization Mixins

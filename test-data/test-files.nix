# Subset of benchmark files used for testing and profiling. We want to cover as
# many cases and edge-cases as possible, but we also want to keep down the total
# number of files for speed.
[
  # Contains equation
  "isaplanner/prop_84.smt2"

  # Contains non-equation
  "grammars/packrat_unambigPackrat.smt2"

  # Contains higher-order types
  "tip2015/list_PairEvens.smt2"

  # Has mutual recursion
  "tip2015/sort_StoogeSort2IsSort.smt2"

  # Contains quote in filename
  "tip2015/tree_sort_SortPermutes'.smt2"

  # Contain canonical or edge-case definitions
  "grammars/simp_expr_unambig4.smt2"
  "isaplanner/prop_01.smt2"
  "isaplanner/prop_15.smt2"
  "isaplanner/prop_35.smt2"
  "isaplanner/prop_44.smt2"
  "prod/prop_35.smt2"
  "tip2015/heap_SortPermutes'.smt2"
  "tip2015/list_SelectPermutations.smt2"
  "tip2015/propositional_AndCommutative.smt2"
  "tip2015/sort_NStoogeSort2Count.smt2"
  "tip2015/sort_NStoogeSort2Permutes.smt2"
]

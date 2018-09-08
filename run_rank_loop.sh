#!/bin/zsh        

  for i in {1..8}
  do
    echo "Rscript rank_special_case.R $i" #check call to r script is as expected
    Rscript rank_special_case.R $i
  done

#!/bin/zsh        

  for i in `seq 1 3 8`
  do
    echo "Rscript special_case_shell.r $i" #check call to r script is as expected
    Rscript special_case_shell.r $i
  done
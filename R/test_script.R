
test_script <- function() {

  library(data.table)

  t <- as.data.table(list(
    col1 = rep(1,100),
    col2 = rep(2,100),
    col3 = c(rep("a",50),rep("b",50)),
    col4 = c(rep("d",25),rep("e",25),rep("f",25),rep("g",25))
  ))
  
  pivot_table_gadget(t)
  
}


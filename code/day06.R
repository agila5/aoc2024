# First part --------------------------------------------------
library(Rcpp)
input <- readLines("data/input06")
input <- strsplit(input, "") |> do.call(rbind, args = _)
starting_point <- which(input == "^", arr.ind = TRUE)
sourceCpp("code/day06.cpp", verbose = TRUE)

go_to_exit(
  input = input, 
  starting_point = starting_point
) |> 
  do.call(cbind, args = _) |> 
  unique() |> 
  nrow()

# Second part -------------------------------------------------------------
# ... 
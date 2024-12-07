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
counter_loops <- 0L
for (i in seq_len(nrow(input))) {
  for (j in seq_len(ncol(input))) {
    # Test whether we are at the starting point or at a boulder
    if ((i == starting_point[1] && j == starting_point[2]) || input[i, j] == "#") {
      next
    }
    
    # Temporarily add a boulder
    input[i, j] <- "#"
    
    # Check for a loop
    if (check_for_loop(input, starting_point)) {
      counter_loops <- counter_loops + 1L
      print(paste0("Detected loop at i = ", i, " and j = ", j))
    }
    
    # Restore matrix
    input[i, j] <- "."
  }
}
counter_loops

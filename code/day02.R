# First part --------------------------------------------------------------
input <- readLines("data/input02")
test_safe <- function(x) {
  if (all(diff(x) %in% c(-1L, -2L, -3L))) {
    return(TRUE)
  } else if (all(diff(x) %in% c(1L, 2L, 3L))) {
    return(TRUE)
  }
  FALSE
}
vapply(
  input, 
  \(x) {
    x <- regmatches(x, gregexpr("[[:digit:]]+", x))[[1]]
    x <- as.integer(x)
    test_safe(x)
  }, 
  logical(1)
) |> sum()

# Second part -------------------------------------------------------------
vapply(
  input, 
  \(x) {
    x <- regmatches(x, gregexpr("[[:digit:]]+", x))[[1]]
    x <- as.integer(x)
    out <- test_safe(x)
    if (out) {
      return(TRUE)
    }
    out_removals <- vapply(
      seq_along(x), 
      \(x, i) test_safe(x[-i]), 
      logical(1), 
      x = x
    )
    any(out_removals)
  }, 
  logical(1)
) |> sum()


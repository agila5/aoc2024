# First part --------------------------------------------------------------
library(igraph)
input <- readLines("data/input05")
idx_blank <- which(input == "")
ordering_rules <- strsplit(input[1:(idx_blank - 1L)], split = "\\|") |> do.call(rbind, args = _)
ordering_rules <- ordering_rules |> graph_from_edgelist(directed = TRUE)
page_numbers <- input[(idx_blank + 1L):length(input)] |> strsplit(",")

compute_distances <- function(x) {
  n <- length(x)
  lapply(
    X = seq_len(n - 1L), 
    FUN = \(i) {
      distances(
        graph = ordering_rules, 
        v = x[i], 
        to = x[(i + 1):n], 
        mode = "out"
      )
    }
  )
}
is_valid_update <- function(x) {
  distances <- compute_distances(x)
  tests <- vapply(distances, \(x) any(x != 1L), logical(1))
  !any(tests)
}

counter <- 0
for (update in page_numbers) {
  n <- length(update)
  if (!is_valid_update(update)) {
    next
  }
  counter <- counter + as.numeric(update[(n + 1) / 2])
}
counter

# Second part -------------------------------------------------------------
swap <- function(x, i, j) {
  tmp <- x[i]
  x[i] <- x[j]
  x[j] <- tmp
  x
}
make_valid <- function(x) {
  while(!is_valid_update(x)) {
    distances <- compute_distances(x)
    for (i in seq_along(distances)) {
      if (any(distances[[i]] != 1L)) {
        which_diff <- which(distances[[i]] != 1L)[1L]
        x <- swap(x, i, which_diff + i)
        break
      }
    }
  }
  x
}
i <- 1L
counter <- 0
for (update in page_numbers) {
  n <- length(update)
  if (is_valid_update(update)) {
    i <- i + 1L
    next
  }
  print(paste0("i = ", i))
  cat(update, "\n")
  update <- make_valid(update)
  counter <- counter + as.numeric(update[(n + 1) / 2])
  i <- i + 1L
}
counter

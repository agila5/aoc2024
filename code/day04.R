# First part --------------------------------------------------------------
input <- readLines("data/input04")

# Organise data as a matrix
input <- strsplit(input, "") |> do.call(rbind, args = _)
check_XMAS <- \(input, i, j) {
  subtext <- input[i, j]
  # When length(i) > 1L & length(j), then input[i, j] returns a matrix and I
  # just want the diagonal
  if (length(i) > 1L & length(j) > 1L) {
    subtext <- diag(subtext)
  }
  subtext <- paste(subtext, collapse = "")
  if (subtext == "XMAS") {
    print(paste0("i = ", i, "; j = ", j))
    counter <<- counter + 1L
  }
}

# Scan through the matrix
counter <- 0L
for (i in seq_len(nrow(input))) {
  for (j in seq_len(ncol(input))) {
    # Check horizontal
    if (j + 3 <= ncol(input)) {
      check_XMAS(input, i, j + 0:3)
    }
    # Check horizontal backward
    if (j - 3 >= 0) {
      check_XMAS(input, i, j - 0:3)
    }
    
    # Check vertical
    if (i + 3 <= nrow(input)) {
      check_XMAS(input, i + 0:3, j)
    }

    # Check vertical backward
    if (i - 3 >= 0) {
      check_XMAS(input, i - 0:3, j)
    }

    # Check main diagonal
    if ((j + 3 <= ncol(input)) && (i + 3 <= nrow(input))) {
      check_XMAS(input, i + 0:3, j + 0:3)
    }

    # Check main diagonal backward
    if ((j - 3 >= 0) && (i - 3 >= 0)) {
      check_XMAS(input, i - 0:3, j - 0:3)
    }
    
    # Check other diagonal
    if ((j + 3 <= ncol(input)) && (i - 3 >= 0)) {
      check_XMAS(input, i - 0:3, j + 0:3)
    }
    
    # Check other diagonal
    if ((j - 3 >= 0) && (i + 3 <= nrow(input))) {
      check_XMAS(input, i + 0:3, j - 0:3)
    }
  }
}

# Second part -------------------------------------------------------------
counter <- 0L
check_MAS <- function(input, i, j) {
  text <- input[i, j]
  diag1 <- paste(diag(text), collapse = "")
  diag2 <- paste(c(text[1, 3], text[2, 2], text[3, 1]), collapse = "")
  possibilities <- c("MAS", "SAM")
  if (diag1 %in% possibilities && diag2 %in% possibilities) {
    counter <<- counter + 1L
  }
}
for (i in seq_len(nrow(input) - 2)) {
  for (j in seq_len(ncol(input) - 2)) {
    # Check the 3 x 3 square
    if ((j + 2 <= ncol(input)) && (i + 2 <= nrow(input))) {
      check_MAS(input, i + 0:2, j + 0:2)
    }
  }
}

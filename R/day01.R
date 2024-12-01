# First part --------------------------------------------------------------
input <- scan("data/input01", what = integer())
first_list <- input[seq(1, length(input) - 1, by = 2)]
second_list <- input[seq(2, length(input), by = 2)]
sum(abs(sort(first_list) - sort(second_list)))

# Part Two ----------------------------------------------------------------
similarity_score <- 0L
for (number in first_list) {
  times <- sum(second_list == number)
  similarity_score <- similarity_score + times * number
}
similarity_score

# First part --------------------------------------------------------------
input <- readLines("data/input03")
input <- paste(input, collapse = "")
extract_mul <- function(x) {
  regmatches(
    x, 
    gregexpr(
      pattern = "mul\\(\\d{1,3}\\,\\d{1,3}\\)", 
      text = x, 
      perl = TRUE
    )
  )
}
regmatches(
  extract_mul(input)[[1]],
  gregexpr(
    pattern = "\\d{1,3}",
    text = extract_mul(input)[[1]],
    perl = TRUE
  )
) |>
  vapply(\(x) prod(as.numeric(x)), numeric(1)) |>
  sum()

# Second part -------------------------------------------------------------
# Detect position of do(s) and don't(s)
id_dont <- gregexpr(
  patter = "don't\\(\\)",
  text = input,
  perl = TRUE
)
id_do <- gregexpr(
  patter = "do\\(\\)",
  text = input,
  perl = TRUE
)

# Replace invalid text
text_placeholder <- paste0(rep("a", 1e5), collapse = "")
substr(input, 116, 212) <- text_placeholder
substr(input, 511, 865) <- text_placeholder
substr(input, 1206, 1765) <- text_placeholder
substr(input, 2569, 3228) <- text_placeholder
substr(input, 5556, 6329) <- text_placeholder
substr(input, 6597, 6757) <- text_placeholder
substr(input, 7788, 9390) <- text_placeholder
substr(input, 10303, 11856) <- text_placeholder
substr(input, 12020, 13370) <- text_placeholder
substr(input, 13549, 13874) <- text_placeholder
substr(input, 14080, 16570) <- text_placeholder
substr(input, 17334, 18628) <- text_placeholder

regmatches(
  extract_mul(input)[[1]],
  gregexpr(
    pattern = "\\d{1,3}",
    text = extract_mul(input)[[1]],
    perl = TRUE
  )
) |>
  vapply(\(x) prod(as.numeric(x)), numeric(1)) |>
  sum()

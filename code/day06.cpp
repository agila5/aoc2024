#include <Rcpp.h>
using namespace Rcpp;

// Helper function to determine the next step
// [[Rcpp::export]]
int get_next_step(int current, const char direction) {
  switch(direction) {
  case 'L':
  case 'U':
    return current - 1;
  case 'R':
  case 'D':
    return current + 1;
  default:
    return current;
  }
}

// Helper function to turn right
// [[Rcpp::export]]
char turn_right(char direction) {
  switch (direction) {
  case 'U': 
    return 'R'; 
  case 'R':
    return 'D'; 
  case 'D': 
    return 'L'; 
  case 'L': 
    return 'U'; 
  }
  stop("Detected impossible input"); 
  return direction; 
}

// Helper function to check for a boulder
// [[Rcpp::export]]
bool check_boulder(const CharacterMatrix input, const int x, const int y) {
  return input(x - 1, y - 1) == "#";
}

// Helper function to check out-of-bounds
// [[Rcpp::export]]
bool check_oob(const CharacterMatrix input, const int x, const int y) {
  return (x - 1) < 0 || 
    (x - 1) >= input.nrow() || 
    (y - 1) < 0 || 
    (y - 1) >= input.ncol();
}

// Main function for the first part
// [[Rcpp::export]]
Rcpp::List go_to_exit(
    const CharacterMatrix input, 
    const IntegerVector starting_point
) {
  // Initialize directions and positions
  std::vector<char> directions = {'U'};
  std::vector<int> positions_x = {starting_point[0]};
  std::vector<int> positions_y = {starting_point[1]};
  int next_x {0}; 
  int next_y {0}; 
  int counter {0}; 
  
  while (true) {
    if (directions[counter] == 'U' || directions[counter] == 'D') {
      next_x = get_next_step(positions_x[counter], directions[counter]); 
      next_y = positions_y[counter]; 
    } else {
      next_x = positions_x[counter]; 
      next_y = get_next_step(positions_y[counter], directions[counter]); 
    }
    if (check_oob(input, next_x, next_y)) break;
    if (check_boulder(input, next_x, next_y)) {
      directions[counter] = turn_right(directions[counter]); 
      continue; 
    }
    counter++; 
    directions.push_back(directions[counter - 1]); 
    positions_x.push_back(next_x); 
    positions_y.push_back(next_y); 
  }
  
  return Rcpp::List::create(
    Named("x") = positions_x, 
    Named("y") = positions_y
  ); 
}

/*** R
stopifnot(get_next_step(5L, "L") == 4L)
stopifnot(get_next_step(5L, "R") == 6L)
stopifnot(get_next_step(5L, "U") == 4L)
stopifnot(get_next_step(5L, "D") == 6L)

stopifnot(identical(turn_right("U"), "R"))
stopifnot(identical(turn_right("R"), "D"))
stopifnot(identical(turn_right("D"), "L"))
stopifnot(identical(turn_right("L"), "U"))

tmp_input <- matrix(c("#"), nrow = 1, ncol = 1)

stopifnot(check_boulder(tmp_input, 1L, 1L))

tmp_input <- matrix(c(".", "#", ".", "."), nrow = 2, ncol = 2)

stopifnot(check_boulder(tmp_input, 2L, 1L))
stopifnot(!check_boulder(tmp_input, 1L, 1L))

stopifnot(check_oob(tmp_input, 0L, 0L))
stopifnot(check_oob(tmp_input, 3L, 1L))
stopifnot(check_oob(tmp_input, 3L, 3L))
stopifnot(check_oob(tmp_input, 1L, 3L))
stopifnot(!check_oob(tmp_input, 1L, 1L))

rm(tmp_input)
*/

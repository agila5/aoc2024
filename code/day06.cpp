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
bool check_boulder(const CharacterMatrix& input, const int x, const int y) {
  return input(x - 1, y - 1) == "#";
}

// Main function for the first part
// [[Rcpp::export]]
Rcpp::List go_to_exit(
    const CharacterMatrix& input, 
    const IntegerVector starting_point
) {
  // Initialize directions and positions
  char direction = 'U'; 
  const std::size_t max_n = input.nrow() * input.ncol(); 
  std::vector<int> positions_x (2 * max_n); 
  positions_x[0] = starting_point[0]; 
  std::vector<int> positions_y (2 * max_n);
  positions_y[0] = starting_point[1]; 
  int next_x {}; 
  int next_y {}; 
  int counter {0}; 
  
  while (true) {
    if (direction == 'U' || direction == 'D') {
      next_x = get_next_step(positions_x[counter], direction); 
      next_y = positions_y[counter]; 
    } else {
      next_x = positions_x[counter]; 
      next_y = get_next_step(positions_y[counter], direction); 
    }
    // Check out of bounds
    if ((next_x - 1) < 0 || (next_x - 1) >= input.nrow() || (next_y - 1) < 0 || (next_y - 1) >= input.ncol()) break;
    
    if (check_boulder(input, next_x, next_y)) {
      direction = turn_right(direction); 
      continue; 
    }
    counter++; 
    positions_x[counter] = next_x; 
    positions_y[counter] = next_y; 
  }
  
  positions_x.resize(counter + 1); 
  positions_y.resize(counter + 1);
  
  return Rcpp::List::create(
    Named("x") = positions_x, 
    Named("y") = positions_y
  ); 
}

// Main function for the second part
// [[Rcpp::export]]
bool check_for_loop(
    const CharacterMatrix& input, 
    const IntegerVector starting_point, 
    const bool debug = false
) {
  bool detected_loop = false; 
  
  // Initialize directions and positions
  char direction = 'U'; 
  // I think this is the maximum number of different positions (???)
  const std::size_t max_n = input.nrow() * input.ncol(); 
  int position_x = starting_point[0]; 
  int position_y = starting_point[1]; 
  int next_x {}; 
  int next_y {}; 
  std::size_t counter {0};
  
  if (debug) {
    Rcpp::Rcout << "Printing debugging information" << std::endl; 
    Rcpp::Rcout << "Starting point is (" << position_x << ", " << position_y << ")" << std::endl; 
  }
  
  while (true) {
    if (direction == 'U' || direction == 'D') {
      next_x = get_next_step(position_x, direction); 
      next_y = position_y; 
    } else {
      next_x = position_x; 
      next_y = get_next_step(position_y, direction); 
    }
    // Check out of bounds
    if ((next_x - 1) < 0 || (next_x - 1) >= input.nrow() || (next_y - 1) < 0 || (next_y - 1) >= input.ncol()) break;
    
    if (check_boulder(input, next_x, next_y)) {
      direction = turn_right(direction); 
      continue; 
    }
    counter++; 
    position_x = next_x; 
    position_y = next_y; 
    
    if (debug) {
      Rcout << "Counter = " << counter << "; next_x = " << next_x << "; next_y = " << next_y << std::endl; 
    }
    
    if (counter > max_n) {
      if (debug) {
        Rcout << "Detected loop" << std::endl;
      }
      detected_loop = true; 
      break; 
    }
  }
  
  return detected_loop; 
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

rm(tmp_input)
*/

#' This function takes a matrix as input and returns a new matrix that has all of the original columns 
#' plus additional columns containing the squares of the values in the original columns. 
#' The new columns are added only for columns that have more than two unique values in the original matrix.

input_square_matrix <- function(input_matrix) {
  # Identify the columns of the input matrix that have more than two unique values
  indices <- which(sapply(input_matrix, function(x) length(unique(x)) > 2))
  
  # Calculate the squares of the values in the identified columns
  square_matrix <- input_matrix[, indices]^2
  
  # Set the column names of the square matrix to the names of the original columns with ".2" appended
  colnames(square_matrix) <- paste0(colnames(input_matrix)[indices], "^2")
  
  # Combine the input matrix with the square matrix
  out <- cbind(input_matrix, square_matrix)
  
  # Return the resulting matrix
  return(out)
}

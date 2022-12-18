#' This function takes a matrix as input and returns a new matrix that has all of the original columns 
#' plus additional columns containing all possible products of pairs of values from the original columns 
#' as well as the squares of the values.

input_combined_matrix <- function(input_matrix) {
  # Identify the columns of the input matrix that have more than two unique values
  indices <- which(sapply(input_matrix, function(x) length(unique(x)) > 2))
  
  # Calculate the product of the input matrix and its transpose
  product_matrix <- input_matrix %*% t(input_matrix)
  # Select only the lower triangular elements of the product matrix
  lower_tri <- lower.tri(product_matrix)
  product_matrix <- product_matrix[lower_tri]
    
  # Calculate the squares of the values in the identified columns
  square_matrix <- input_matrix[, indices]^2
  # Set the column names of the square matrix to the names of the original columns with "^2" appended
  colnames(square_matrix) <- paste0(colnames(input_matrix)[indices], "^2")
  
  # Combine the product matrix and the square matrix
  combined_matrix <- cbind(product_matrix, square_matrix)
  # Combine the input matrix with the combined matrix
  out <- cbind(input_matrix, combined_matrix)
  
  return(out)
}

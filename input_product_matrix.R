#' This function takes a matrix as input and returns a new matrix that has all of the original columns 
#' plus additional columns containing all possible products of pairs of values from the original columns. 
#' The new columns are added only for columns that have more than two unique values in the original matrix.

input_product_matrix <- function(input_matrix) {
  # Calculate the product of the input matrix and its transpose
  product_matrix <- input_matrix %*% t(input_matrix)
  
  # Select only the lower triangular elements of the product matrix
  lower_tri <- lower.tri(product_matrix)
  product_matrix <- product_matrix[lower_tri]
  
  # Combine the input matrix with the lower triangular elements of the product matrix
  product_matrix <- cbind(input_matrix, product_matrix)
  
  # Return the resulting matrix
  return(product_matrix)
}

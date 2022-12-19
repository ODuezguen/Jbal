#' This function takes a matrix as input and returns a new matrix that has all of the original columns 
#' plus additional columns containing all possible products of pairs of values from the original columns. 
#' The new columns are added only for columns that have more than two unique values in the original matrix.

input_product_matrix <- function(input_matrix) {
  
  # Adding some checks
  if (!is.matrix(input_matrix) || !is.numeric(input_matrix)) {
    stop("Error: input_matrix must be a numeric matrix.")
  }
  if (nrow(input_matrix) < 2) {
    stop("Error: input_matrix must have at least 2 rows.")
  }
  
  # Calculating the product of the input matrix and its transpose
  tryCatch({
    product_matrix <- input_matrix %*% t(input_matrix)
  }, error = function(e) {
    stop("Error: An error occurred while attempting to calculate the product of input_matrix and its transpose:", e)
  })
  # Selecting the lower triangular elements of the product matrix
  lower_tri <- lower.tri(product_matrix)
  product_matrix <- product_matrix[lower_tri]
  
  # Combining the input matrix with the lower triangular elements of the product matrix
  tryCatch({
    product_matrix <- cbind(input_matrix, product_matrix)
  }, error = function(e) {
    stop("Error: An error occurred while attempting to bind input_matrix and product_matrix:", e)
  })

  return(product_matrix)
}

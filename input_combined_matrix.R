#' This function takes a matrix as input and returns a new matrix that has all of the original columns 
#' plus additional columns containing all possible products of pairs of values from the original columns 
#' as well as the squares of the values.

input_combined_matrix <- function(input_matrix) {
  # Adding some checks
  if (!is.matrix(input_matrix) || !is.numeric(input_matrix)) {
    stop("Error: input_matrix must be a numeric matrix.")
  }
  if (nrow(input_matrix) < 2) {
    stop("Error: input_matrix must have at least 2 rows.")
  }
  
  # Identifying the columns of the input matrix that have more than two unique values
  indices <- which(sapply(input_matrix, function(x) length(unique(x)) > 2))
  
  # Calculating the product of the input matrix and its transpose
  tryCatch({
    product_matrix <- input_matrix %*% t(input_matrix)
  }, error = function(e) {
    stop("Error: An error occurred while attempting to calculate the product of input_matrix and its transpose:", e)
  })                       
  # Selecting only the lower triangular elements of the product matrix
  lower_tri <- lower.tri(product_matrix)
  product_matrix <- product_matrix[lower_tri]
    
  # Calculating the squares of the values in the identified columns
  square_matrix <- input_matrix[, indices]^2
  # Set the column names of the square matrix to the names of the original columns with "^2" appended
  colnames(square_matrix) <- paste0(colnames(input_matrix)[indices], "^2")
  
  # Verifying that the square_matrix has at least one column
  if (ncol(square_matrix) == 0) {
    stop("Error: square_matrix must have at least one column.")
  }
  
  # Combining the product matrix and the square matrix
  tryCatch({
    combined_matrix <- cbind(product_matrix, square_matrix)
  }, error = function(e) {
    stop("Error: An error occurred while attempting to bind product_matrix and square_matrix:", e)
  })
  # Combining the input matrix with the combined matrix
  out <- cbind(input_matrix, combined_matrix)
  
  return(out)
}

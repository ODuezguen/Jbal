#' This function takes a matrix as input and returns a new matrix that has all of the original columns 
#' plus additional columns containing the squares of the values in the original columns. 
#' The new columns are added only for columns that have more than two unique values in the original matrix.

input_square_matrix <- function(input_matrix) {
  
  # Adding some checks
  if (!is.matrix(input_matrix) || !is.numeric(input_matrix)) {
    stop("Error: input_matrix must be a numeric matrix.")
  }
  if (nrow(input_matrix) < 2) {
    stop("Error: input_matrix must have at least 2 rows.")
  }
  
  # Identifying the columns of the input matrix that have more than two unique values
  indices <- which(sapply(input_matrix, function(x) length(unique(x)) > 2))
  # Calculating the squares of the values in the identified columns
  square_matrix <- input_matrix[, indices]^2
  colnames(square_matrix) <- paste0(colnames(input_matrix)[indices], "^2")
  
  # Combining the input matrix with the square matrix
  tryCatch({
    out <- cbind(input_matrix, square_matrix)
  }, error = function(e) {
    stop("Error: An error occurred while attempting to bind input_matrix and square_matrix:", e)
  })
  
  return(out)
}

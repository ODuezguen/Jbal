#' This function takes a matrix as input and returns a new matrix that has all of the original columns 
#' plus additional columns containing all possible products of pairs of values ("crossproduct") from the original columns 
#' as well as the squares of the values ("square") or the combination of both ("combined"). Default is "crossproduct".
#'
#' The new columns are added only for columns that have more than two unique values in the original matrix.
#'
#' @author Onur Düzgün

cov.specifier <- function(cov_matrix, 
                          method = "crossproduct") {
    # Adding some checks
    if (!is.matrix(cov_matrix) || !is.numeric(cov_matrix)) {
      stop("Error: Covariate matrix must be a numeric matrix.")
    }
    if (nrow(cov_matrix) < 2) {
      stop("Error: covariate matrix must have at least 2 rows.")
    }

  if (method == "crossproduct) {
    # Function 1: Calculate cross product matrix
    # Calculating the cross product of the covariate matrix and its transpose
    tryCatch({
      product_matrix <- cov_matrix %*% t(cov_matrix)
    }, error = function(e) {
      stop("Error: An error occurred while attempting to calculate the cross product of the covariate matrix and its transpose:", e)
    })
    # Selecting the lower triangular elements of the product matrix
    lower_tri <- lower.tri(product_matrix)
    product_matrix <- product_matrix[lower_tri]

    # Combining the input matrix with the lower triangular elements of the product matrix
    tryCatch({
      out <- cbind(cov_matrix, product_matrix)
    }, error = function(e) {
      stop("Error: An error occurred while attempting to bind cov_matrix and product_matrix:", e)
    })
    return(out)
    
  } else if (method == "square") {
    # Function 2: Calculate square matrix
    # Identifying the columns of the input matrix that have more than two unique values
    indices <- which(sapply(cov_matrix, function(x) length(unique(x)) > 2))
    # Calculating the squares of the values in the identified columns
    square_matrix <- cov_matrix[, indices]^2
    colnames(square_matrix) <- paste0(colnames(cov_matrix)[indices], "^2")

    # Combining the input matrix with the square matrix
    tryCatch({
      out <- cbind(cov_matrix, square_matrix)
    }, error = function(e) {
      stop("Error: An error occurred while attempting to bind cov_matrix and square_matrix:", e)
    })
    return(out)
    
  } else if (method == "combined") {
    # Function 3: Calculate combined matrix
    # Identifying the columns of the covariate matrix that have more than two unique values
    indices <- which(sapply(cov_matrix, function(x) length(unique(x)) > 2))
  
    # Calculating the cross product of the covariate matrix and its transpose
    tryCatch({
      product_matrix <- cov_matrix %*% t(cov_matrix)
    }, error = function(e) {
      stop("Error: An error occurred while attempting to calculate the cross product of the covariate matrix and its transpose:", e)
    })                       
    # Selecting only the lower triangular elements of the product matrix
    lower_tri <- lower.tri(product_matrix)
    product_matrix <- product_matrix[lower_tri]
    
    # Calculating the squares of the values in the identified columns
    square_matrix <- cov_matrix[, indices]^2
    colnames(square_matrix) <- paste0(colnames(cov_matrix)[indices], "^2")
  
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
    out <- cbind(cov_matrix, combined_matrix)
    return(out)
    
    } else {
    # Handling invalid options
    stop("Invalid option")
    }
  }

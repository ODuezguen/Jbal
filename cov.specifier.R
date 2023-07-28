#' The function allows the user to choose from three different options (crossproduct, square, or combined) 
#' to specify the type of covariance matrix to be returned. Default is "crossproduct".
#'
#' The function now includes input validation to ensure that the covariate matrix is valid and suitable for analysis.
#'
#' @author Onur Düzgün

cov.specifier <- function(cov_matrix, method = "crossproduct") {
    
  # Input validation
  if (!is.matrix(cov_matrix) || nrow(cov_matrix) == 0 || ncol(cov_matrix) == 0) {
    stop("Input must be a non-empty matrix")
  }
  if (any(!is.numeric(cov_matrix))) {
    stop("Covariate matrix must contain only numeric values")
  }
  if (length(unique(colnames(cov_matrix))) != ncol(cov_matrix)) {
    stop("Covariate matrix must have unique column names")
  }
  if (nrow(cov_matrix) < 2) {
    stop("Covariate matrix must have at least two rows")
  }
  if (any(is.na(cov_matrix))) {
    stop("Covariate matrix must not contain missing values")
  }
  if (any(apply(cov_matrix, 2, var) == 0)) {
    stop("Covariate matrix must not have zero variance columns")
  }
    
  # Option 1: Returns original matrix and cross product matrix
  if (method == "crossproduct") {
    # Calculating the product matrix of each combination of columns
    k <- ncol(cov_matrix)
    n <- nrow(cov_matrix)
    product_matrix <- matrix(NA, n, (k*(k+1))/2 + 1)
    colnames(product_matrix)[((k*(k+1))/2 + 1)] <- "dummy"
    if (n * k > .Machine$integer.max) {
       stop("Output matrix is too large to be computed")
       }
    count <- 1
    for (i in 1:(k-1)) {
        product_matrix[,count:(count+k-i-1)] <- outer(cov_matrix[,i], cov_matrix[, (i+1):k])
        colnames(product_matrix)[count:(count+k-i-1)] <- paste0(colnames(cov_matrix)[i], ".", colnames(cov_matrix)[(i+1):k])
        count <- count + k - i
        }
     
    # Combining the original matrix with the new matrix
    out <- cbind(cov_matrix, product_matrix)

    # Removing columns with all zero values
    zero_cols <- apply(out, 2, function(x) all(x == 0))
    out <- out[, !zero_cols]
                       
    return(out)   
    
  # Option 2: Returns original matrix and squares of each column
  } else if (method == "square") {
    # Adding squared terms to output matrix
    cov_matrix.sq <- apply(cov_matrix, 2, function(x) if (length(unique(x)) > 1) x^2 else NULL)
    cov_matrix.sq <- cov_matrix.sq[, !is.null(cov_matrix.sq)]
    cov_matrix.sq.names <- paste0(colnames(cov_matrix.sq), "^2")
   
    # Combining the original matrix with the new matrix
    out <- cbind(cov_matrix, cov_matrix.sq)
    colnames(out)[(ncol(cov_matrix)+1):ncol(out)] <- cov_matrix.sq.names
                           
    return(out)

  # Option 3: Calculate combined matrix
  } else if (method == "combined") {
    # Calculating the product matrix of each combination of columns
    k <- ncol(cov_matrix)
    n <- nrow(cov_matrix)
    product_matrix <- matrix(NA, n, (k*(k+1))/2 + 1)
    colnames(product_matrix)[((k*(k+1))/2 + 1)] <- "dummy"
    if (n * k > .Machine$integer.max) {
       stop("Output matrix is too large to be computed")
       }
    count <- 1
    for (i in 1:(k-1)) {
        product_matrix[,count:(count+k-i-1)] <- outer(cov_matrix[,i], cov_matrix[, (i+1):k])
        colnames(product_matrix)[count:(count+k-i-1)] <- paste0(colnames(cov_matrix)[i], ".", colnames(cov_matrix)[(i+1):k])
        count <- count + k - i
        }
      
    # Adding squared terms to output matrix
    cov_matrix.sq <- apply(cov_matrix, 2, function(x) if (length(unique(x)) > 1) x^2 else NULL)
    cov_matrix.sq <- cov_matrix.sq[, !is.null(cov_matrix.sq)]
    cov_matrix.sq.names <- paste0(colnames(cov_matrix.sq), "^2")
    
    # Combining the original matrix with the new matrices
    out <- cbind(cov_matrix, product_matrix, cov_matrix.sq)
    colnames(out)[(ncol(cov_matrix)+ncol(product_matrix)):ncol(out)] <- cov_matrix.sq.names

    # Removing columns with all zero values
    zero_cols <- apply(out, 2, function(x) all(x == 0))
    out <- out[, !zero_cols]
    
    return(out)
    
  # Lastly, handling invalid options
  } else {
    stop("Invalid option")
  }
}

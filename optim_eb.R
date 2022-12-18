#' This code uses the L-BFGS-B optimization algorithm to implement entropy balancing, as proposed by Hainmueller (2012).
#'
#' The function takes the following input arguments:
#' tr.total: A vector of target values.
#' co.x: A matrix of values used to compute the weights in the optimization procedure.
#' base.weight: A scalar value used to compute the weights.
#' control: A list of control parameters for the L-BFGS-B optimizer, such as the maximum number of iterations or the tolerance for convergence.

# Define the optimization function
optim_eb <- function(tr.total, co.x, base.weight, control=list()) {
  # Pre-allocate memory for the coefficients
  lambda <- rep(0, ncol(co.x))
  
  # Define the function to be minimized
  loss_fun <- function(lambda) {
    # Compute the weights
    weights <- exp(co.x %*% lambda) * base.weight
    # Aggregate the values in co.x
    co.x.agg <- t(weights) %*% co.x
    # Compute the deviation from the target values
    deviation <- co.x.agg - tr.total
    # Return the sum of squared deviations as the loss function
    return(sum(deviation^2))
  }
  
  # Use L-BFGS-B to minimize the loss function
  result <- optim(par=lambda, fn=loss_fun, tr.total=tr.total, co.x=co.x, base.weight=base.weight,
                  method="L-BFGS-B", control=list(maxit=1000, reltol=1e-6))
  # Compute the final weights
  weights <- exp(co.x %*% result$par) * base.weight
  # Extract the optimized coefficients, final weights, and loss from the result
  lambda <- result$lambda
  weights <- result$weights
  loss <- result$loss
  # Check if the optimization converged within the tolerance
  converged <- max(abs(co.x.agg - tr.total)) < control$constraint.tolerance
  if(converged){cat("Converged within tolerance \n")}
  # Return the results as a list
  return(list(maxdiff=max(abs(co.x.agg - tr.total)), coefs=lambda, weights.ebal=weights, converged=converged))
}

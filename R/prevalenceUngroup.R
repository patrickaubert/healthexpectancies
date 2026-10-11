# Internal functions used by prevalenceApprox() to estimate prevalences by single year of age
# from prevalences by age brackets.

# matrix (ages x brackets) of weights, normalised so that they sum to 1 within each bracket
ungroup_weights_matrix <- function(agecuts, agemin, agemax, weight) {
  ages <- agemin:agemax
  bracket <- findInterval(ages, agecuts)
  W <- matrix(0, nrow = length(ages), ncol = length(agecuts))
  for (b in seq_along(agecuts)) {
    inb <- (bracket == b)
    if (!any(inb)) {
      stop(paste0("No age between 'agemin' and 'agemax' in the age bracket starting at ",agecuts[b],
                  ": check 'agecuts', 'agemin' and 'agemax'."), call. = FALSE)
    }
    if (!(sum(weight[inb]) > 0)) {
      stop(paste0("The weights of the age bracket starting at ",agecuts[b]," are not strictly positive."), call. = FALSE)
    }
    W[inb, b] <- weight[inb] / sum(weight[inb])
  }
  W
}

# matrix of second differences : (D %*% x)[i] = x[i] - 2 x[i+1] + x[i+2]
second_differences_matrix <- function(n) {
  D <- matrix(0, nrow = n - 2, ncol = n)
  for (i in seq_len(n - 2)) { D[i, i:(i + 2)] <- c(1, -2, 1) }
  D
}

# Smoothing on the logit scale. Finds the vector of prevalences by age p that minimises the sum of squared
# second differences of logit(p), under the exact constraint that the weighted average of p in each age bracket
# equals the observed prevalence of the bracket (sequential quadratic programming).
prevalence_ungroup_logit <- function(prevalence, agecuts, agemin, agemax, weight,
                                     eps = 1e-6, maxit = 100, tol = 1e-10) {

  if (anyNA(prevalence) | anyNA(weight)) { stop("Missing values (NA) are not allowed in 'prevalence' and 'weight'.", call. = FALSE) }
  if (any(prevalence < 0 | prevalence > 1)) {
    stop("With method = 'logit', prevalences must be between 0 and 1 (are they expressed in percent?).", call. = FALSE)
  }
  if (is.unsorted(agecuts, strictly = TRUE)) { stop("'agecuts' must be strictly increasing.", call. = FALSE) }
  if (agemin < agecuts[1]) { stop("'agemin' must not be lower than the first value of 'agecuts'.", call. = FALSE) }
  if ((agemax - agemin + 1) < 3) { stop("At least 3 ages are needed.", call. = FALSE) }

  # a prevalence of exactly 0 or 1 cannot be matched on the logit scale
  if (any(prevalence < eps | prevalence > 1 - eps)) {
    warning(paste0("Prevalences below ",eps," or above 1 - ",eps," have been replaced by these bounds."), call. = FALSE)
    prevalence <- pmin(pmax(prevalence, eps), 1 - eps)
  }

  expit <- function(x) 1 / (1 + exp(-x))
  W <- ungroup_weights_matrix(agecuts, agemin, agemax, weight)
  n <- nrow(W); k <- ncol(W)
  D <- second_differences_matrix(n)
  H <- 2 * crossprod(D) + 1e-9 * diag(n)

  # starting point : prevalence constant within each bracket
  theta <- stats::qlogis(prevalence[findInterval(agemin:agemax, agecuts)])

  converged <- FALSE
  for (it in seq_len(maxit)) {
    p <- expit(theta)
    cons <- as.vector(crossprod(W, p)) - prevalence                 # constraint violation (k values)
    J <- t(W) * rep(p * (1 - p), each = k)                           # jacobian (k x n)
    K <- rbind(cbind(H, t(J)), cbind(J, matrix(0, k, k)))
    sol <- tryCatch(solve(K, c(-as.vector(H %*% theta), -cons)),
                    error = function(e) stop("The smoothing problem could not be solved (singular system).", call. = FALSE))
    step <- sol[1:n]
    step <- step * min(1, 5 / max(abs(step)))                         # limits the step (in logit units)
    theta <- theta + step
    if (max(abs(cons)) < tol & max(abs(step)) < sqrt(tol)) { converged <- TRUE; break }
  }

  p <- expit(theta)
  err <- max(abs(as.vector(crossprod(W, p)) - prevalence))
  if (!converged & err > 1e-8) {
    warning(paste0("The smoothing algorithm did not fully converge (maximum gap with observed prevalences: ",
                   signif(err, 2),")."), call. = FALSE)
  }
  p
}

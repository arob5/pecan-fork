MultivariateNormal <- R6Class(
  classname = "MultivariateNormal",
  inherit = Distribution,

  public = list(
    mean = NULL,
    cov = NULL,
    chol = NULL, # Lower Cholesky factor.

    initialize = function(mean, cov, batch_shape=NULL) {
      d <- length(mean)
      super$initialize(support_shape=d, batch_shape=batch_shape)

      self$mean <- mean
      self$cov <- cov
      self$chol <- t(chol(cov))
    },

    log_density = function(x) {
      x <- as.matrix(x)
      if (ncol(x) != self$support_shape) stop("x has wrong dimension")
      xc <- t(x) - self$mean
      # Mahalanobis term using Cholesky factor
      sol <- backsolve(self$chol, xc, transpose = TRUE)
      quad_form <- colSums(sol^2)
      logdet <- 2 * sum(log(diag(self$chol)))
      -0.5 * (self$support_shape * log(2 * pi) + logdet + quad_form)
    },

    sample = function(n = 1) {
      Z <- matrix(rnorm(n * self$support_shape), nrow = self$support_shape)
      samples <- self$mean + self$chol %*% Z
      t(samples)
    }
  )
)
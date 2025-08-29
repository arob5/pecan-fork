NormalDistribution <- R6Class(
  classname = "NormalDistribution",
  inherit = Distribution,
  
  public = list(
    mean = NULL,
    sd = NULL,
    
    initialize = function(mean=0, sd=1, name=NULL, scalar_names=NULL) {
      self$validate_dist_params(mean, sd)
      super$initialize(shape=1L, name=name, scalar_names=scalar_names)
      self$mean <- mean
      self$sd <- sd
    },
    
    validate_dist_params = function(mean, sd) {
      if(length(mean) != 1L) stop("`NormalDistribution` requires length 1 `mean`.")
      if(length(sd) != 1L) stop("`NormalDistribution` requires length 1 `sd`.")
      if(sd < 0) stop("`NormalDistribution` requires positive value for `sd`.")
    }
  ), 
  
  private = list(
    .log_density = function(x_arr) {
      dnorm(x_arr, mean=self$mean, sd=self$sd, log=TRUE)
    },
    
    .sample = function(n=1L) {
      matrix(rnorm(n, mean=self$mean, sd=self$sd), ncol=1L)
    }, 
  )
)


# MultivariateNormal <- R6Class(
#   "MultivariateNormal",
#   inherit = Distribution,
#   
#   public = list(
#     mean = NULL,
#     cov = NULL,
#     chol = NULL, # Lower Cholesky factor.
#     
#     initialize = function(mean, cov, batch_shape=NULL) {
#       d <- length(mean)
#       super$initialize(support_shape=d, batch_shape=batch_shape)
#       
#       self$mean <- mean
#       self$cov <- cov
#       self$chol <- t(chol(cov))
#     },
#     
#     log_density = function(x) {
#       x <- as.matrix(x)
#       if (ncol(x) != self$support_shape) stop("x has wrong dimension")
#       xc <- t(x) - self$mean
#       # Mahalanobis term using Cholesky factor
#       sol <- backsolve(self$chol, xc, transpose = TRUE)
#       quad_form <- colSums(sol^2)
#       logdet <- 2 * sum(log(diag(self$chol)))
#       -0.5 * (self$support_shape * log(2 * pi) + logdet + quad_form)
#     },
#     
#     sample = function(n = 1) {
#       Z <- matrix(rnorm(n * self$support_shape), nrow = self$support_shape)
#       samples <- self$mean + self$chol %*% Z
#       t(samples)
#     }
#   )
# )


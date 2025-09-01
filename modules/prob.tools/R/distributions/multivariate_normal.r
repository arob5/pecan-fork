
# Constants
LOG_TWO_PI <- log(2.0 * pi)

MultivariateNormal <- R6Class(
  classname = "MultivariateNormal",
  inherit = Distribution,

  public = list(
    mean = NULL,

    initialize = function(mean=NULL, cov=NULL, chol_lower=NULL, check_pos_def=FALSE, ...) {
      private$.validate_dist_params(mean, cov, chol_lower, check_pos_def)
      d <- ifelse(!is.null(mean), length(drop(mean)),
                  ifelse(!is.null(cov), dim(cov)[1], dim(chol_lower)[1]))
      
      # Mean defaults to zero and cov defaults to identity.
      if(is.null(mean)) mean <- rep(0, d)
      if(is.null(cov) && is.null(chol_lower)) chol_lower <- diag(1.0, nrow=d, ncol=d)
      
      super$initialize(shape=d, ...)
      self$mean <- mean
      private$.cov <- cov
      private$.chol_lower <- chol_lower
    }
  ), 
  
  private = list(
    .cov = NULL,
    .chol_lower = NULL, # Lower Cholesky factor.
    .precision = NULL, # Precision (inverse covariance) matrix.
    .constraint = "None",
    
    .log_density = function(x) {
      # In: (n,d) matrix
      # Out: (n,1) vector of log-density evaluations.
      
      # solve L z = y to obtain z = L^{-1} y, where y=x-mean.
      z <- backsolve(self$chol_lower, t(x-self$mean))
      
      # Quadratic term.
      quad_term <- colSums(z^2)
      
      # Log determinant term.
      logdet_term <- 2 * sum(log(diag(self$chol_lower)))
      
      -0.5 * (quad_term + logdet_term + self$shape * LOG_TWO_PI)
    },
    
    .sample = function(n=1L) {
      # Returns (n,self$shape) matrix of samples.
      Z <- matrix(rnorm(n*self$shape), nrow=self$shape)
      x <- self$mean + self$chol_lower %*% Z
      t(x)
    }, 
    
    .validate_dist_params = function(mean=NULL, cov=NULL, chol_lower=NULL, check_pos_def=FALSE) {
      # At least one of the three args must be provided in order to infer 
      # the dimension. At most one of `cov` and `chol_lower` can be provided.
      
      d_mean <- d_cov <- d_chol_lower <- NA
      
      if(all(is.null(mean), is.null(cov), is.null(chol_lower))) {
        stop("MultivariateNormal requires at least one of `mean`, `cov`, `chol_lower`.")
      }
      
      if(!is.null(cov) && !is.null(chol_lower)) {
        stop("MultivariateNormal requires at most one of `cov` and `chol_lower`.")
      }
      
      if(!is.null(mean)) {
        if(!is.vector(drop(mean))) stop("MultivariateNormal requires 1d `mean` that can be coerced to vector.")
        d_mean <- length(drop(mean))
      }
      
      if(!is.null(cov)) {
        if(!is.matrix(cov)) stop("MultivariateNormal requires `cov` to be a matrix.")
        if(nrow(cov) != ncol(cov)) stop("MultivariateNormal requires `cov` to be a square matrix.")
        if(!isSymmetric(unname(cov))) stop("MultivariateNormal requires `cov` to be a symmetric matrix.")
        
        if(check_pos_def) {
          chol_results <- is_positive_definite(cov)
          if(chol_results$is_pos_def) { # Cache Cholesky computation.
            private$.chol_lower <- t(chol_results$chol_upper)
          } else {
            stop("MultivariateNormal requires `cov` to be positive definite. Error in `chol(cov)`.")
          }
        }
        
        d_cov <- nrow(cov)
      }
      
      if(!is.null(chol_lower)) {
        if(!is.matrix(chol_lower)) stop("MultivariateNormal requires `chol_lower` to be a matrix.")
        if(nrow(chol_lower) != ncol(chol_lower)) stop("MultivariateNormal requires `chol_lower` to be a square matrix.")
        if(!is_lower_tri(chol_lower)) stop("MultivariateNormal requires `chol_lower` to be lower triangular.")
        d_chol_lower <- nrow(chol_lower)
      }
      
      # Ensure dimensions are consistent.
      dims <- as.vector(na.omit(unique(c(d_mean, d_cov, d_chol_lower))))
      if(length(dims) > 1L) {
        stop("MultivariateNormal dimension mismatch between `mean`, `cov`, `chol_lower`.")
      }
    }
  ), 
  
  active = list(
    chol_lower = function(value) {
      if(missing(value)) { # Cache Cholesky factor if not previously computed.
        if(is.null(private$.chol_lower)) private$.chol_lower <- t(chol(private$.cov))
        return(private$.chol_lower)
      } else {
        stop("MultivariateNormal cov/chol_lower are immutable.")
      }
    }, 
    
    cov = function(value) {
      if(missing(value)) { # Cache covariance if not previously computed.
        if(is.null(private$.cov)) private$.cov <- tcrossprod(self$chol_lower)
        return(private$.cov)
      } else {
        stop("MultivariateNormal cov/chol_lower are immutable.")
      }
    }, 
    
    precision = function(value) {
      if(missing(value)) { # Cache precision if not previously computed.
        if(is.null(private$.precision)) private$.precision <- chol2inv(t(self$chol_lower))
        return(private$.precision)
      } else {
        stop("MultivariateNormal precision matrix cannot be set.")
      }
    }
  )
)
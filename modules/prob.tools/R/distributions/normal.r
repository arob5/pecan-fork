Normal <- R6Class(
  classname = "Normal",
  inherit = Distribution,
  
  public = list(
    mean = NULL,
    sd = NULL,
    
    initialize = function(mean=0, sd=1, ...) {
      self$validate_dist_params(mean, sd)
      super$initialize(shape=1L, ...)
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
    }
  )
)

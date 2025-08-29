
Distribution <- R6Class(
  classname = "Distribution",
  public = list(
    shape = NULL, # Shape of a single draw from this distribution
    
    initialize = function(shape, name=NULL, scalar_names=NULL) {
      if (class(self)[1] == "Distribution") {
        stop("Distribution is an abstract class and cannot be instantiated directly.")
      }
      self$shape <- shape
      self$name <- name
      self$scalar_names <- scalar_names # TODO: write setter for this to handle shape/validation
    },
    
    length = function() {
      prod(self$shape)
    },
    
    n_dims = function() {
      length(self$shape)
    },

    #' Convert flattened input to distribution shape.
    #' 
    #' The input `x` is required to be a vector with length equal to 
    #' `self$length()` or a matrix of shape `(n, self$length)`. In the former 
    #' case the input is converted to an array of shape `c(1,self$shape)`
    #' (if `simplify=FALSE`)  or `self$shape` (if `simplify=TRUE`), and in 
    #' the latter case the input is converted to an array of shape 
    #' `c(n, self$shape)`. Subclasses may override this default method is
    #' specialty shaping is required. The reshaping is done in row major 
    #' order (i.e., "C" style). In general, returns array of shape 
    #' `c(n, self$shape)`. If `simplify=TRUE` and `n=1` then squashes to 
    #' `self$shape`.
    from_flat = function(x, simplify=TRUE) {
      
      if(!self$.input_is_flat(x)) {
        stop("`from_flat(x)` requires `x` in valid flattened format.")
      }
      
      # Handle single value case.
      if(is.vector(x)) x <- matrix(x, nrow=1L)

      # R stores in column-major order (columns are contiguous in memory).
      # We thus transpose `x` so that each value becomes a column. Values
      # are assigned to array in column-major order, then permuted afterwards
      # to maintain the convention that the first dimension is the number 
      # of values.
      n <- nrow(x)
      arr <- array(t(x), dim=c(self$shape, n)) # c(self$shape, n)
      arr <- aperm(arr, c(self$n_dims + 1L, seq_along(self$shape))) # c(n, self$shape)
      
      # Optionally simplify if there is only a single value.
      if((n == 1L) && (simplify)) array(arr, dim = dim(arr)[-1L])
      else arr
    },
    
    #' Convert values with distribution shape to their flattened vector format.
    #' The input `x` must either be an array of shape `self$shape` or an 
    #' array of shape `c(n, self$shape)` (a batch of values). The input will 
    #' be converted to a matrix of shape `(n,self$length)` (where `n=1` in
    #' the former case). If `n=1` and `simplify=TRUE` then squashes to 
    #' `self$length` vector. This method exactly reverses the steps of 
    #' `from_flat()` to ensure consistent and reproducible conversion.
    #' 
    to_flat = function(x, simplify=TRUE) {
      
      if(!self$.input_is_array(x)) {
        stop("`to_flat(x)` requires `x` in valid array format.")
      }
      
      # Handle the case where `x` is a single value.
      if(length(dim(x)) == self$n_dims) x <- array(x, dim=c(1L, dim(x)))
      
      n <- dim(x)[1]
      x <- aperm(x, c(2:length(dim(x)), 1L)) # c(self$shape, n)
      x <- matrix(x, nrow=self$length(), ncol=n) # (self$length(), n)
      x <- t(x) # (n, self$length())
      
      # Optionally simplify if there is only a single value.
      if((n == 1L) && (simplify)) x[1,]
      else x
    },

    #' Evaluate log-density on a batch of points
    #' @param x n input points (in flattened or array format)
    #' @return numeric vector of length n
    log_density = function(x) {
      x <- self$.transform_to_array(x)
      self$.log_density(x)
    },
    
    #' Generate samples
    #' @param n number of samples
    #' @return n samples in array or flattened format.
    sample = function(n=1L, flatten=FALSE) {
      x_arr <- self$.sample(n=n)
      
      if(flatten) self$.transform_to_flat(x_arr)
      else x_arr
    }, 
    
    print = function() {
      cat("Distribution: ", class(self)[1], "\n")
      cat("Shape ", self$shape, " / Length ", self$length())
    }, 
    
    validate_dist_params = function(...) {
      stop("validate_dist_params() must be implemented by subclasses.")
    }
  ), 
  
  private = list(
    
    .input_is_array = function(x) {
      if(!(is.vector(x) || is.array(x))) return(FALSE)
      
      shape_x <- dim(x)
      n_dims_x <- length(shape_x)
      
      # Single value in array format.
      if((n_dims_x == self$n_dims) && all(shape_x == self$shape)) return(TRUE)
      
      # Multiple values in array format.
      if((n_dims_x == self$n_dims+1) && all(shape_x[-1] == self$shape)) return(TRUE)
      
      return(FALSE)
    },
    
    .input_is_flat = function(x) {
      if(!(is.vector(x) || is.array(x))) return(FALSE)
      
      shape_x <- dim(x)
      n_dims_x <- length(shape_x)
      len_x <- prod(shape_x)
      
      # Single value in flat format.
      if(is.vector(x) && (len_x == self$length())) return(TRUE)
      
      # Multiple values in flat format.
      if(is.matrix(x) && (ncol(x) == self$length())) return(TRUE)
      
      return(FALSE)
    },
    
    .transform_to_array = function(x) {
      # In: input in array or flattened format.
      # Out: input in array format.
      
      if(self$.input_is_array(x)) x
      else if(self$.input_is_flat(x)) self$from_flat(x)
      else {
        stop("Input `x` is not in valid array or flattened format.")
      }
    },
    
    .transform_to_flat = function(x) {
      # In: input in array or flattened format.
      # Out: input in flattened format.
      
      if(self$.input_is_flat(x)) x
      else if(self$.input_is_array(x)) self$to_flat(x)
      else {
        stop("Input `x` is not in valid array or flattened format.")
      }
    },
    
    .log_density = function(x_arr) {
      # In: `x_arr` in array format; c(n,self$shape) or self$shape vector.
      # Out: numeric(n)
      stop(".log_density() must be implemented by subclasses.")
    }, 
    
    .sample = function(n=1L) {
      # Out: n samples in array format; c(n,self$shape)
      stop(".sample() must be implemented by subclasses.")
    }
    
  )
)


# ProductDistribution <- R6Class(
#   classname = "ProductDistribution",
#   inherit = Distribution,
#   
#   public = list(
#     components = NULL,
#     
#     initialize = function(components) {
#       if (!all(sapply(components, function(x) "Distribution" %in% class(x)))) {
#         stop("All components must inherit from Distribution")
#       }
#       self$components <- components
#       
#       # Product has no single support shape: use NULL here
#       super$initialize(support_shape = NULL, batch_shape = length(components))
#     },
#     
#     log_density = function(x_list) {
#       # x_list: list of inputs, one per component
#       if (length(x_list) != length(self$components)) {
#         stop("x_list length must match number of components")
#       }
#       sum(sapply(seq_along(self$components), function(i) {
#         self$components[[i]]$log_density(x_list[[i]])
#       }))
#     },
#     
#     sample = function(n = 1) {
#       # Returns a list of samples, one per component
#       lapply(self$components, function(comp) comp$sample(n))
#     },
#     
#     to_array = function(samples) {
#       # samples: list of length len(components), each is (n, dim)
#       mats <- lapply(samples, function(s) {
#         if (is.matrix(s)) return(s)
#         if (is.vector(s)) return(matrix(s, ncol = 1))
#         stop("Each component sample must be a matrix or vector")
#       })
#       do.call(cbind, mats) # combine into one matrix (n, sum of dims)
#     }
#   )
# )





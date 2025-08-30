# base_distribution.r
#
# Contains the base distribution classes from which specific distribution
# implementations subclass. This includes an abstract base `Distribution`
# class, and a `ProductDistribution` class that acts as a container for 
# multiple distribution objects.


Distribution <- R6Class(
  classname = "Distribution",
  public = list(
    rv_name = NULL,
    rv_scalar_names = NULL,
    
    initialize = function(shape, rv_name=NULL, rv_scalar_names=NULL) {
      if (class(self)[1] == "Distribution") {
        stop("Distribution is an abstract class and cannot be instantiated directly.")
      }
      private$.shape <- shape
      self$rv_name <- rv_name
      self$rv_scalar_names <- rv_scalar_names # TODO: write setter for this to handle shape/validation
    },
    
    input_is_array = function(x) {
      if(!(is.vector(x) || is.array(x))) return(FALSE)
      
      shape_x <- ifelse(is.vector(x), length(x), dim(x))
      n_dims_x <- length(shape_x)

      # Single value in array format.
      if((n_dims_x == self$n_dims) && all(shape_x == self$shape)) return(TRUE)
      
      # Multiple values in array format.
      if((n_dims_x == self$n_dims+1) && all(shape_x[-1] == self$shape)) return(TRUE)
      
      return(FALSE)
    },
    
    input_is_flat = function(x) {
      if(!(is.vector(x) || is.array(x))) return(FALSE)
      
      shape_x <- ifelse(is.vector(x), length(x), dim(x))
      len_x <- prod(shape_x)
      
      # Single value in flat format.
      if(is.vector(x) && (len_x == self$length)) return(TRUE)
      
      # Multiple values in flat format.
      if(is.matrix(x) && (ncol(x) == self$length)) return(TRUE)
      
      return(FALSE)
    },
    
    transform_to_array = function(x, simplify=FALSE) {
      # In: input in array or flattened format.
      # Out: input in array format.
      # If `x` is already simple, then this function will not un-simplify.
      
      if(self$input_is_array(x)) {
        if(simplify) private$.simplify_array(x)
        else x
      } else if(self$input_is_flat(x)) {
        private$.flat_to_array(x, simplify)
      } else {
        stop("Input `x` is not in valid array or flattened format.")
      }
    },
    
    transform_to_flat = function(x, simplify=FALSE) {
      # In: input in array or flattened format.
      # Out: input in flattened format.
      # If `x` is already simple, then this function will not un-simplify.
      
      if(self$input_is_flat(x)) {
        if(simplify) private$.simplify_flat(x)
        else x
      } else if(self$input_is_array(x)) {
        private$.array_to_flat(x, simplify)
      } else {
        stop("Input `x` is not in valid array or flattened format.")
      }
    },

    #' Evaluate log-density on a batch of points
    #' @param x n input points (in flattened or array format)
    #' @return numeric vector of length n
    log_density = function(x) {
      x <- self$transform_to_array(x, simplify=FALSE)
      if(is.vector(x)) x <- matrix(x, nrow=1L)
      drop(private$.log_density(x))
    },
    
    #' Generate samples
    #' @param n number of samples
    #' @return n samples in array or flattened format.
    sample = function(n=1L, flat=FALSE, simplify=FALSE) {
      x_arr <- private$.sample(n=n)
      
      if(flat) {
        self$transform_to_flat(x_arr, simplify)
      } else {
        if(simplify) private$.simplify_array(x_arr)
        else x_arr
      }
    }, 
    
    print = function() {
      cat("Distribution: ", class(self)[1])
      cat("\nShape ", self$shape, " | Length ", self$length)
      cat("\nConstraint: ", self$constraint)
      
      if(!is.null(self$rv_name)) cat("\nName: ", self$rv_name)
      if(!is.null(self$rv_scalar_names)) cat("\nScalar names: ", self$scalar_names)
      cat("\n")
    }, 
    
    simplify_array = function(x, check_type=TRUE) {
      # Public facing interface to expose functionality of private 
      # `.simplify_array()` method (which does not do type checking). This is 
      # primarily defined so that `ProductDistribution` can access the 
      # simplify array functionality of component distributions.
      
      if(check_type) {
        if(!self$input_is_array(x)) stop("`simplify_array(x)` requires `x` to be in array format.")
      }
      
      private$.simplify_array(x)
    }
  ),
  
  active = list(
    shape = function(value) {
      if(missing(value)) private$.shape
      else stop("Cannot set `shape` of `Distribution` after initialization.")
    },
    
    length = function(value) {
      if(missing(value)) prod(self$shape)
      else stop("Cannot set `length` of `Distribution`.")
    },
    
    n_dims = function(value) {
      if(missing(value)) length(self$shape)
      else stop("Cannot set `n_dims` of `Distribution`.")
    }, 
    
    constraint = function(value) {
      if(missing(value)) private$.constraint
      else stop("Cannot set `constraint` of `Distribution`.")
    }
  ),
  
  private = list(
    
    .shape = NULL, # e.g., c(1) for scalar, c(2) for vector, c(1,3,5) for 3d array.
    .constraint = NULL, # Note: NULL means missing, "None" means unconstrained.
    
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
    #' 
    #' No input checking here; checking is done in public interface `transform_to_array()`.
    .flat_to_array = function(x_flat, simplify=FALSE) {
      
      # Handle single value case.
      if(is.vector(x_flat)) x_flat <- matrix(x_flat, nrow=1L)
      
      # R stores in column-major order (columns are contiguous in memory).
      # We thus transpose `x_flat` so that each value becomes a column. Values
      # are assigned to array in column-major order, then permuted afterwards
      # to maintain the convention that the first dimension is the number 
      # of values.
      n <- nrow(x_flat)
      arr <- array(t(x_flat), dim=c(self$shape, n)) # c(self$shape, n)
      arr <- aperm(arr, c(self$n_dims + 1L, seq_along(self$shape))) # c(n, self$shape)
      
      # Optionally simplify if there is only a single value.
      if(simplify) private$.simplify_array(arr)
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
    #' No input checking here; checking is done in public interface `transform_to_flat()`.
    .array_to_flat = function(x_arr, simplify=FALSE) {
      
      # Handle the case where `x` is a single value.
      if(length(dim(x_arr)) == self$n_dims) x_arr <- array(x_arr, dim=c(1L, dim(x_arr)))
      
      n <- dim(x_arr)[1]
      x_arr <- aperm(x_arr, c(2:length(dim(x_arr)), 1L)) # c(self$shape, n)
      x_flat <- matrix(x_arr, nrow=self$length, ncol=n) # (self$length(), n)
      x_flat <- t(x_flat) # (n, self$length())
      
      # Optionally simplify if there is only a single value.
      if(simplify) private$.simplify_flat(x_flat)
      else x_flat
    },
    
    .simplify_array = function(x_arr) {
      if(dim(x_arr)[1] == 1L) array(x_arr, dim = dim(x_arr)[-1L])
      else x_arr
    },
    
    .simplify_flat = function(x_flat) {
      if(nrow(x_flat) == 1L) x_flat[1,]
      else x_flat
    },
    
    .log_density = function(x_arr) {
      # In: `x_arr` in array format; c(n,self$shape) or self$shape vector.
      # Out: numeric(n)
      stop(".log_density() must be implemented by subclasses.")
    }, 
    
    .sample = function(n=1L) {
      # Out: n samples in array format; c(n,self$shape)
      stop(".sample() must be implemented by subclasses.")
    }, 
    
    .validate_dist_params = function(...) {
      # This method is intended to be called in the `initialize()` method.
      # This is currently not enforced in any way; if desired, could be 
      # enforced by having sub-classes pass list of distribution parameters
      # to super, and then call this method within super initialize.
      stop(".validate_dist_params() must be implemented by subclasses.")
    }
    
  )
)

#' Container holding `Distribution` objects used to define a product distribution.
#' 
#' This class is effectively a wrapper around a list of `Distribution` objects.
#' `ProductDistribution` is itself a `Distribution`. Its log-density method 
#' is defined as the sum of the log-densities of its component `Distribution`
#' objects. Its `sampling` method either returns a list (one element per
#' `Distribution`) or appends the samples from each `Distribution` into 
#' a single matrix (the flat data format). Thus, the array vs flat data 
#' formats for a `Distribution` are replaced with list of array vs flat 
#' data formats for a `ProductDistribution`. For consistency, we still refer 
#' to the former as the "array" format. To convert between the two formats,
#' the ordering of the component list used to initialize the `ProductDistribution`
#' is preserved. The constructor accepts other `ProductDistribution` objects
#' in the constructor, but no nesting is done under the hood. The constructor
#' will simply extract the components and append them to the component list.
#' 
ProductDistribution <- R6Class(
  classname = "ProductDistribution",
  inherit = Distribution,

  public = list(
    components = NULL,

    initialize = function(...) {
      components <- list(...)
      if(length(components) == 0L) {
        stop("`ProductDistribution` cannot be initialized without components.")
      }
      
      if(!all(sapply(components, function(comp) is_distribution(comp)))) {
        stop("All `ProductDistribution` components must inherit from `Distribution`")
      }
      
      self$components <- list()
      for(comp in components) {
        if(is_product_distribution(comp)) self$components <- c(self$components, comp$components)
        else self$components <- c(self$components, list(comp))
      }

      # Product has no single shape, set to NULL.
      super$initialize(shape=NULL)
    },
    
    length = function() {
      # Total number of scalars composing the product distribution, NOT the 
      # component length. See `n_components` for the latter.
      as.integer(sum(self$apply_component_field("length", "sapply")))
    },
    
    log_density = function(x, sum_components=TRUE) {
      # In: array or flat format containing n input values.
      # Out: By default (`sum_components=TRUE`) n vector containing n log-density evals.
      #      If `sum_components=FALSE` returns (n,n_component) matrix, where the 
      #      log-density evaluations for each component are stacked in the rows.
      
      x <- self$transform_to_array(x) # List of c(n, shp_i) arrays.
      ldens_components <- self$apply_component_method("log_density", x, "sapply") # (n,n_components)

      if(sum_components) rowSums(ldens_components)
      else ldens_components
    },

    sample = function(n=1L, flat=FALSE, simplify=FALSE) {
      # List of component samples.
      samp <- self$apply_component_method("sample", NULL, "lapply", 
                                          n=n, flat=flat, simplify=simplify)

      if(flat) {
        samp <- do.call(cbind, samp)
        if(simplify) private$.simplify_flat(samp)
        else samp
      } else {
        samp
      } 
    },
    
    input_is_array = function(x) {
      # TRUE if `x` is a list of component values, each of which is a valid array.
      # Note that `input_is_flat()` is inherited from `Distribution` unchanged.
      
      if(!is.list(x)) return(FALSE)
      if(length(x) != self$n_components) return(FALSE)
      
      all(self$apply_component_method("input_is_array", x, "sapply"))
    },

    apply_component_method = function(method_name, args_list=NULL, apply_func="lapply", ...) {
      # Applies the method `method_name` to each component, where `args_list`
      # is a list of length `n_component` containing arguments for each component
      # method call. Only works for public methods. Additional arguments that 
      # are constant across method calls can be forwarded via `...`.
      # TODO: need to generalize this to handle multiple named arguments.
      
      include_extra_args <- (length(list(...)) > 0L)
      
      # No arguments.
      if(!is.null(args_list)) {
        if(include_extra_args) match.fun(apply_func)(self$components, function(comp) comp[[method_name]](...))
        else match.fun(apply_func)(self$components, function(comp) comp[[method_name]]()) 
      }
      
      # Has arguments.
      if(length(args_list) != self$n_components) {
        stop("`args_list` does not have length equal to number of components.")
      }
      
      if(include_extra_args) {
        match.fun(apply_func)(seq_len(self$n_components), 
                              function(i) self$components[[i]][[method_name]](args_list[[i]]))
      } else {
        match.fun(apply_func)(seq_len(self$n_components), 
                              function(i) self$components[[i]][[method_name]](args_list[[i]]), ...)
      }
    }, 
    
    apply_component_field = function(field_name, apply_func="lapply") {
      # Only works for public fields.
      match.fun(apply_func)(self$components, function(comp) comp[[field_name]]) 
    }
  ), 
  
  active = list(
    n_components = function(value) {
      if(missing(value)) length(self$components)
      else stop("Cannot set `n_components` in ProductDistribution.")
    } 
  ), 
  
  private = list(
    .simplify_array = function(x_arr) {
      self$apply_component_method("simplify_array", x_arr, "lapply", check_type=FALSE)
    }, 
    
    .flat_to_array = function(x_flat, simplify=FALSE) {
      
      if(is.vector(x_flat)) x_flat <- matrix(x_flat, nrow=1L)
      
      x_arr <- vector(mode="list", length=self$n_components)
      col_idx_start <- 1L
      for(i in seq_len(self$n_components)) {
        col_idx_end <- col_idx_start + self$components[[i]]$length - 1L
        x_arr[[i]] <- self$components[[i]]$transform_to_array(x_flat[,col_idx_start:col_idx_end, drop=FALSE], simplify)
        col_idx_start <- col_idx_end + 1L
      }
      
      return(x_arr)
    }, 
    
    .array_to_flat = function(x_arr, simplify=FALSE) {
      mat_list <- self$apply_component_method("transform_to_flat", x_arr, "lapply", simplify=simplify)
      mat <- do.call(mat_list, cbind)
      
      if(simplify) private$.simplify_flat(mat)
      else mat
    }
  )
)





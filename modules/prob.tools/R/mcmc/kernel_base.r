# mcmc/kernel_base.r

#' Markov kernel abstract base class
#'
#' An abstract R6 class defining the interface for Markov kernels used in MCMC 
#' algorithms. A Markov kernel defines a transition mechanism via its 
#' \code{step} method, which simply outputs a new state given the current state.
#' A Markov kernel can optionally be adapted via the \code{update} method.
#'
#' Subclasses must implement the \code{step} method to advance the Markov chain,
#' and optionally the \code{update} method for adaptive behavior.
#'
#' This class should not be instantiated directly; instead, provide a function to
#' \code{make_markov_kernel()} or define a subclass.
#'
#' @section Public methods:
#' \describe{
#'   \item{\code{step(state)}}{Performs a single MCMC step, returning the next state.
#'     This method must be implemented in subclasses.}
#'   \item{\code{update(...)}}{Updates the kernel's internal state for adaptation.
#'   The default creates a non-adaptive kernel.}
#' }
#'
#' @examples
#' # Example subclass implementing a simple random-walk kernel
#' RandomWalkKernel <- R6::R6Class(
#'   classname = "RandomWalkKernel",
#'   inherit = MarkovKernel,
#'   public = list(
#'     step = function(state) state + rnorm(1),
#'     update = function(...) NULL
#'   )
#' )
#' kernel <- RandomWalkKernel$new()
#' kernel$step(0)  # Advance one step
#'
#' @seealso \code{\link{make_markov_kernel}}
#'
#' @docType class
#' @name MarkovKernel
MarkovKernel <- R6::R6Class(
  classname = "Kernel",
  public = list(
    step = function(state) stop("Abstract base MarkovKernel class cannot be instantiated."),
    update = function(...) NULL
  )
)


#' Create a Markov kernel object for MCMC
#'
#' Constructs a \code{MarkovKernel} R6 object from either a function or an 
#' existing \code{MarkovKernel} object. This allows both simple functions and 
#' stateful kernels to be used uniformly in MCMC algorithms.
#'
#' If a function is provided, it must take a state as input and return a new 
#' state; the function is used to define the \code{step} method of a new 
#' \code{MarkovKernel} object. If an object of class \code{MarkovKernel} is 
#' provided, it is returned unmodified.
#'
#' @param obj Either a function implementing a Markov kernel step, or an existing
#'   \code{MarkovKernel} R6 object.
#'
#' @return A \code{MarkovKernel} R6 object suitable for use in MCMC routines.
#'
#' @seealso \code{\link{MarkovKernel}}
#'
#' @examples
#' # Example 1: Create a MarkovKernel from a simple random walk function
#' rw_step <- function(x) x + rnorm(1)
#' kernel1 <- make_markov_kernel(rw_step)
#'
#' # Example 2: Use an existing MarkovKernel object
#' kernel2 <- make_markov_kernel(kernel1)
#'
#' # Example 3: Error from an unsupported type
#' try(make_markov_kernel("not a function"))
#' @export
make_markov_kernel <- function(obj) {
  if (is.function(obj)) {
    MarkovKernel$new(
      step = obj,
      update = function(...) NULL
    )
  } else if (inherits(obj, "MarkovKernel")) {
    obj
  } else {
    stop("Must provide a function or Kernel object")
  }
}

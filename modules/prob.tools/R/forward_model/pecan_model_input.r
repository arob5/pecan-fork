# forward_model/pecan_model_input.r
#
# A ModelInput designed to interface with PEcAn's model run workflow.
# Depends: model_input.r


PECAN_BRANCH_KEY <- "pecan"


#' PEcAn ModelInput Constructor
#' 
#' Generate a PEcAn \code{ModelInput} by combining PEcAn \code{Settings} and 
#' other model inputs.
#' 
#' @details
#' A PEcAn \code{ModelInput} is simply a \code{ModelInput} tree with a special
#' branch/sub-tree located at key path \code{PECAN_BRANCH_KEY}. This sub-tree
#' is intended to store a PEcAn \code{Settings} object (or a subset of the
#' information in a \code{Settings} object). The names of the nodes in this 
#' sub-tree should follow those of the PEcAn settings exactly. For example, the 
#' PEcAn settings stored in \code{settings$run$inputs$met} would be found at the 
#' \code{ModelInput} key \code{PECAN_BRANCH_KEY/run/inputs/met}.
#' 
#' All input slots not contained
#' in this branch are unconstrained, but will typically store inputs that 
#' are directly passed to the write config functions when running models.
#' See \code{\link{run_model.Settings}} for more details.
#' 
#' If \code{x} already contains the PEcAn branch, then \code{pecan_settings}
#' should be \code{NULL}. If it does not contain the branch already, then
#' \code{pecan_settings} must be provided and will be inserted within \code{x}
#' at the key path \code{PECAN_BRANCH_KEY}.
#' 
#' @param x A named list or \code{ModelInput}.
#' @param pecan_settings A named list, PEcAn \code{Settings} object, or \code{NULL}.
#' @param ... Additional arguments passed to \code{.new_pecan_model_input()}.
#' 
#' @returns A \code{ModelInput} object.
#' 
#' @author Andrew Roberts
#' @export
PecanModelInput <- function(x, pecan_settings=NULL, ...) {
  x <- .new_pecan_model_input(x, pecan_settings, ...)
  validate_pecan_model_input(x)
  
  return(x)
}


#' Low level helper to generate a PEcAn model input object
.new_pecan_model_input <- function(x, pecan_settings, ...) {
  x <- as_model_input(x, ...)
  
  if(!is.null(pecan_settings)) {
    x <- set_model_input_value(x, PECAN_BRANCH_KEY, pecan_settings, 
                               untagged_is_input=TRUE, allow_overwrite=FALSE)
  }
  
  return(x)
}


#' Determine if an object is a PEcAn \code{ModelInput}
#'
#' A PEcAn \code{ModelInput} does not formally subclass \code{ModelInput}.
#' Rather, it is simply a \code{ModelInput} with the additional requirement
#' that there be a subtree located at key \code{PECAN_BRANCH_KEY}.
#' 
#' @param x An R object
#' @returns logical(1), \code{TRUE} if \code{x} is a PEcAn \code{ModelInput}.
#'
#' @author Andrew Roberts
#' @export
is_pecan_model_input <- function(x) {
  is_model_input(x) && is_model_input(x[[PECAN_BRANCH_KEY]])
}


#' Throw error if object is not PEcAn \code{ModelInput}
#' 
#' @param x An object
#' @returns Invisibly returns \code{TRUE} if \code{x} is a PEcAn \code{ModelInput}.
#'  Otherwise throws an error.
#' 
#' @seealso \code{\link{is_pecan_model_input}}
#' @author Andrew Roberts
#' @export
.check_pecan_model_input_type <- function(x) {
  if(!is_pecan_model_input(x)) {
    stop("Object is not a PEcAn model input.")
  }
}


#' Validate a PEcAn ModelInput object
#'
#' Currently an alias for \code{\link{.check_pecan_model_input_type}}.
#' Included so that additional validation can easily be included in the future.
#'
#' @export
validate_pecan_model_input <- function(x) {
  .check_pecan_model_input_type(x)
}


#' Return the PEcAn settings sub-tree from a PEcAn \code{ModelInput}
#'
#' @param x A PEcAn \code{ModelInput}.
#' @returns A \code{ModelInput}, the PEcAn setting branch from \code{x}.
#' 
#' @author Andrew Roberts
#' @export
pecan_subtree <- function(x) {
  .check_pecan_model_input_type(x)
  x[[PECAN_BRANCH_KEY]]
}


#' Return input slots from PEcAn sub-tree
#'
#' @param x A PEcAn \code{ModelInput}
#' @returns list, flat list containing input slots from the PEcAn sub-tree.
#'  Names are set to key paths of the sub-tree (not key paths of \code{x}).
#'  
#' @author Andrew Roberts
#' @export
pecan_inputs <- function(x) {
  input_slots(pecan_subtree(x))
}


#' Overwrite values in PEcAn \code{Settings} object
#' 
#' Settings from the PEcAn sub-tree of a PEcAn \code{ModelInput} are used to
#' overwrite (or add) values in a PEcAn \code{Settings} object. A common use
#' case is where the \code{Settings} object contains defaults that should
#' be overwritted by values in \code{model_input}.
#' 
#' @details
#' Values are set by key path. For example, a \code{model_input} value at 
#' \code{PECAN_BRANCH_KEY/run/inputs/met} will set a value at
#' \code{settings$run$inputs$met}. If a key exists only in \code{settings}
#' it will be untouched. If a key exists in both objects, then the value in 
#' \code{settings} will be overwritten by the value in \code{model_input}.
#' If a key only exists in \code{model_input}, it will be added to \code{settings}.
#' Note that values outside of the PEcAn branch in the \code{model_input}
#' object will be ignored.
#'
#' @param settings named list or PEcAn \code{Settings} object.
#' @param model_input A PEcAn \code{ModelInput}.
#'
#' @returns A PEcAn \code{Settings} object containing the updated settings.
#'
#' @author Andrew Roberts
#' @export
update_pecan_settings <- function(settings, model_input) {
  
  pecan_tree <- pecan_subtree(model_input)
  
  for(key in input_keys(pecan_tree)) {
    val <- .resolve_model_input_path(pecan_tree$.data, key, error_if_missing=FALSE)$value
    settings <-.assign_value_at_path(settings, key, val, allow_overwrite=TRUE)
  }
  
  PEcAn.settings::as.Settings(settings)
}

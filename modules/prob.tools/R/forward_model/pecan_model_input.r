# forward_model/pecan_model_input.r
#
# A ModelInput subclass that encodes an input to a PEcAn model.
# Depends: model_input.r

SETTINGS_TEMPLATE <- list(outdir = character(1),
                          modeloutdir = character(1),
                          rundir = character(1),
                          run = list(
                            site = list(),
                            start.date = character(1),
                            end.date = character(1),
                            inputs = list()
                          ))

PECAN_BRANCH_KEY <- "pecan"


#' PEcAn ModelInput Constructor
#' 
#' @details
#' \code{PecanModelInput} subclasses \code{ModelInput}, adding additional 
#' PEcAn-specific structure that serves to interface with PEcAn's model run
#' workflow, which is based around the PEcAn \code{Settings} object.
#' 
#' As with all \code{ModelInput}s, a \code{PecanModelInput} is a tree, with the
#' leaves specifying different model inputs and optional metadata. 
#' \code{PecanModelInput} adds a special branch named \code{pecan}, which
#' stores a sub-tree that follows the nested structure of a PEcAn \code{Settings}
#' list. The names of the nodes in this sub-tree should follow those of the 
#' PEcAn settings exactly. For example, the PEcAn settings stored in
#' \code{settings$run$inputs$met} would be found at the \code{ModelInput} key
#' \code{pecan/run/inputs/met}.
#' 
#'
#'
#'
PecanModelInput <- function(x, pecan_settings=NULL, ...) {
  x <- .new_pecan_model_input(x, pecan_settings, ...)
  # validate_pecan_model_input(x)
  
  return(x)
}


.new_pecan_model_input <- function(x, pecan_settings, ...) {
  x <- as_model_input(x, ...)
  
  if(!is.null(pecan_settings)) {
    pecan_settings <- as_model_input(pecan_settings)
    x <- .assign_value_at_path(x, PECAN_BRANCH_KEY, pecan_settings, allow_overwrite=FALSE)
  }
  
  return(x)
}


is_pecan_model_input <- function(x) {
  is_model_input(x) && is_model_input(x[[PECAN_BRANCH_KEY]])
}


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


pecan_subtree <- function(x) {
  .check_pecan_model_input_type(x)
  x[[PECAN_BRANCH_KEY]]
}


pecan_config_inputs <- function(x) {
  input_slots(pecan_subtree(x))
}


#' Recursively update a nested settings list with new values
#'
#' This function updates a nested tree-like list of settings with values from
#' another list. The update proceeds recursively:
#' \itemize{
#'   \item If a key exists in both \code{settings} and \code{new_settings}:
#'     \itemize{
#'       \item If both values are lists, they are merged recursively.
#'       \item Otherwise, the value in \code{settings} is replaced with the one from \code{new_settings}.
#'     }
#'   \item If a key exists only in \code{new_settings}, it is added to \code{settings}.
#'   \item If a key exists only in \code{settings}, it is left untouched.
#' }
#'
#' @param settings A named list (possibly nested) representing the base settings.
#' @param new_settings A named list (possibly nested) of updates to apply.
#'
#' @return A new list representing \code{settings} updated with values from \code{new_settings}.
#' @examples
#' settings <- list(a = list(b = 1, c = 2), d = 3)
#' new_settings <- list(a = list(b = 10, e = 5), f = 7)
#' updated <- update_settings(settings, new_settings)
#' str(updated)
#' # List of 3
#' #  $ a:List of 3
#' #   ..$ b: num 10
#' #   ..$ c: num 2
#' #   ..$ e: num 5
#' #  $ d: num 3
#' #  $ f: num 7
update_pecan_settings <- function(settings, model_input) {
  
  pecan_tree <- pecan_subtree(model_input)
  
  for(key in input_keys(pecan_tree)) {
    val <- .resolve_model_input_path(pecan_tree, key, error_if_missing=FALSE)
    settings <-.assign_value_at_path(settings, key, val, allow_overwrite=TRUE)
  }
  
  settings
  # PEcAn.settings::as.Settings(settings)
}
















#' Set a value in a nested PEcAn settings list
#'
#' Recursively sets the element of \code{settings} identified by a path of keys.
#' Creates intermediate lists if they do not exist.
#'
#' @param settings A named list, possibly nested. For example, a PEcAn
#'  \code{Settings} object. 
#' @param key_path A character vector of keys giving the path to the value.
#' @param value The value to set at the given path.
#'
#' @return The updated \code{settings} list.
#' @author Andrew Roberts
set_pecan_settings_value <- function(settings, key_path, value) {
  if (length(key_path) == 1L) {
    settings[[key_path]] <- value
  } else {
    key <- key_path[1]
    if (is.null(settings[[key]]) || !is.list(settings[[key]])) {
      settings[[key]] <- list()
    }
    settings[[key]] <- set_settings_value(settings[[key]], key_path[-1], value)
  }
  settings
}


#' Get a value from a nested settings list
#'
#' Recursively retrieves the element of \code{settings} identified by a path of keys.
#' If the path does not exist, returns \code{default}.
#'
#' @param settings A named list (possibly nested).
#' @param key_path A character vector of keys giving the path to the value.
#' @param default A value to return if the path does not exist.
#'
#' @return The value at the specified path, or \code{default}.
get_settings_value <- function(settings, key_path, default = NULL) {
  if (length(key_path) == 0) {
    return(settings)
  }
  key <- key_path[1]
  if (!is.list(settings) || is.null(settings[[key]])) {
    return(default)
  }
  get_settings_value(settings[[key]], key_path[-1], default)
}

# Internal helper: collect all key paths from a nested list,
# including paths to empty sublists.
collect_paths <- function(lst, prefix = character()) {
  paths <- list()
  for (nm in names(lst)) {
    path <- c(prefix, nm)
    if (is.list(lst[[nm]])) {
      if (length(lst[[nm]]) == 0) {
        # Include empty sublist as a path
        paths <- c(paths, list(path))
      } else {
        # Recurse into non-empty sublist
        sub_paths <- collect_paths(lst[[nm]], path)
        # Always include the sublist itself
        paths <- c(paths, list(path), sub_paths)
      }
    } else {
      # Leaf value
      paths <- c(paths, list(path))
    }
  }
  paths
}

#' Update a settings list with values from another
#'
#' Recursively updates \code{settings} with values from \code{new_settings}.
#' Existing values in \code{settings} are overwritten if they appear in \code{new_settings}.
#' Values present only in \code{settings} remain unchanged.
#' Values present only in \code{new_settings} are added.
#' Empty sublists in \code{new_settings} are also created in \code{settings}.
#'
#' @param settings A named list (possibly nested).
#' @param new_settings A named list (possibly nested) of updates.
#'
#' @return An updated \code{settings} list.
#'
#' @examples
#' settings <- list(a = list(b = 1, c = 2), d = 3)
#' new_settings <- list(a = list(b = 10, e = 5, f = list()), g = list(h = list()))
#' updated <- update_settings(settings, new_settings)
#' str(updated)
#' # List of 4
#' #  $ a:List of 4
#' #   ..$ b: num 10
#' #   ..$ c: num 2
#' #   ..$ e: num 5
#' #   ..$ f: list()
#' #  $ d: num 3
#' #  $ g:List of 1
#' #   ..$ h: list()
#' #  $ f: num 7
update_settings <- function(settings, new_settings) {
  paths <- collect_paths(new_settings)
  for (path in paths) {
    val <- get_settings_value(new_settings, path)
    settings <- set_settings_value(settings, path, val)
  }
  settings
}

# forward/ensemble_input_table.r

#' Construct ensemble model input in table format
#' 
#' Constructs an object of class \code{EnsembleInputTable}, which is a 
#' \code{(n_runs, n_slots)} tibble with each row defining the inputs for 
#' a single run.
#' 
#' @details
#' The column names are set to \code{slot_names(ens_input)}, where 
#' \code{ens_input} is an \code{EnsembleInput} object. The prefix \code{slot_}
#' is prepended to these column names. If different inputs
#' have different slots, then this will result in \code{NA} values in the
#' table. Objects of the \code{EnsembleInputTable} class also inherit from 
#' \code{EnsembleInput}. The column name \code{run_id} is a special reserved
#' name that will not be treated as a slot. By default, all columns other than
#' \code{run_id} will be interpreted as slots. To include metadata columns 
#' that are not slots or \code{run_id}, the slot columns can be explicitly
#' be provided with the \code{slot_names} argument. In this case, all columns
#' other than \code{c(run_id, slot_names)} will be treated as metadata.
#' The prefix \code{metadata_} will be appended to all of these columns. 
#' The table is required to be a \code{tibble} due to its support of list 
#' columns; input slots are not required to be atomic, so the table columns
#' in this representation must be able to store arbitrary structured objects.
#'
#' @param input_table A tibble (\code{df_tbl}). \code{NA} values are
#'  interpreted as the respective run not having a slot for that respective column.
#' @param slot_names Optional, character vector specifying the column names of 
#'  \code{input_table} that correspond to input slots.
#' @return An object of class \code{EnsembleInputTable}, inheriting from 
#'  \code{EnsembleInput}.
#' @export
EnsembleInput.tbl_df <- function(input_table, slot_names=NULL) {

  # Default run IDs.
  if(!("run_id" %in% names(input_table))) {
    input_table <- input_table %>% mutate(run_id=paste0("run_", dplyr::row_number()))
  }
  
  metadata_names <- setdiff(names(input_table), c("run_id", slot_names))
  
  # Tag slot and metadata columns
  input_table <- input_table %>% rename_with(~paste0("slot_", .x), all_of(slot_names))
  input_table <- input_table %>% rename_with(~paste0("metadata_", .x), all_of(metadata_names))
  
  obj <- structure(input_table,
                   class=c("EnsembleInputTable", "EnsembleInput"))
  validate_ensemble_input_table(obj)
  
  return(obj)
}


is_ensemble_input_table <- function(x) {
  is_ensemble_input(x) && inherits(x, "EnsembleInputTable")
}


validate_ensemble_input_table <- function(obj) {
  if(!is_ensemble_input_table(obj)) {
    stop("`obj` is not an `EnsembleInputTable` object.")
  }
  
  if(!tibble::is.tibble(obj)) {
    stop("EnsembleInputTable must be a tibble.")
  }
  
  if(!("run_id" %in% names(obj))) {
    stop("`EnsembleInputTable$run_id` column is missing.")
  }
  
  tagged_cols <- c("run_id", 
                   .get_col_block(x, "slot_", strip_prefix=FALSE),
                   .get_col_block(x, "metadata_", strip_prefix=FALSE))
  invalid_cols <- setdiff(names(obj), tagged_cols)
    
  if(length(invalid_cols) > 0L) {
    stop("EnsembleInputTable columns must be `run_id` or tagged with ",
         "`slot_` or `metadata_`. Tibble has extra columns: ",
         paste(invalid_cols, collapse=", "))
  }
  
  invisible(obj)
}


# Returns character(0) if there are no slot columns.
slot_names.EnsembleInputTable <- function(x, ...) {
  .get_col_block(x, "slot_", strip_prefix=TRUE)
}

# Returns character(0) if there are no slot columns.
metadata_names.EnsembleInputTable <- function(x, ...) {
  .get_col_block(x, "metadata_", strip_prefix=TRUE)
}


as_ensemble_input_table <- function(x, ...) {
  UseMethod("as_ensemble_input_table")
}


#' @export
as_ensemble_input_table.default <- function(x, ...) {
  stop("as_ensemble_input_table() is not implemented for objects of class ", 
       paste(class(x), collapse = "/"))
}


as_ensemble_input_table.EnsembleInputTable <- function(x, ...) {
  x
}


as_ensemble_input_table.EnsembleInputList <- function(x, ...) {
  x
}


# Number of runs
length.EnsembleInputTable <- function(x, ...) {
  nrow(x)
}


run_ids.EnsembleInputTable <- function(x, ...) {
  x$run_id
}


#' #' @export
#' print.EnsembleInputList <- function(x, ...) {
#'   cat("<EnsembleInputList>\n")
#'   cat(" Number of runs:", length(x), "\n")
#'   
#'   slot_nm <- slot_names(x)
#'   if(length(slot_nm) == 0L) {
#'     cat("  (no slots)\n")
#'   } else {
#'     cat(" slots:", paste(slot_nm, collapse = ", "), "\n")
#'   }
#'   
#'   invisible(x)
#' }


# x: data.frame/tibble
# prefix: character(1)
# Optionally strips the prefix before returning.
# Returns character(0) if there are no matches.
.get_col_block <- function(x, prefix, strip_prefix=FALSE) {
  nm <- names(x)
  col_block <- nm[startsWith(nm, prefix)]
  
  if(strip_prefix) sub(paste0("^", prefix), "", col_block)
  else col_block
}


.make_ensemble_input_table_row <- function(run_id, slot_names, metadata_names,
                                           input_list, metadata_list) {
  
  row_slots <- lapply(slot_names, 
                  function(nm) {
                    if(nm %in% names(input_list)) input_list[[nm]] else NA
                  })
  
  row_metadata <- lapply(metadata_names, 
                    function(nm) {
                      if(nm %in% names(metadata_list)) metadata_list[[nm]] else NA
                    })
  
  names(row_slots) <- slot_names
  names(row_metadata) <- metadata_names
  c(list(run_id=run_id), slots, meta) # Return list
}





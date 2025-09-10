# forward_model/broadcast_rules.r


#' Cartesian product broadcast rule
#' 
#' Full cartesian product of slot values.
#'
#' @param lens Integer vector of dimension/slot lengths.
#' @return A matrix of dimension (prod(lens), length(lens)).
#'
#' @examples
#' # A Cartesian product of three dimensions of size 2, 3 and 2.
#' # Outputs a matrix with 12 rows and 3 columns.
#' rule_cartesian(c(2, 3, 2))
#' 
#' @author Andrew Roberts
#' @export
rule_cartesian <- function(lens) {
  .check_lens(lens)
  grids <- do.call(expand.grid, lapply(lens, seq_len))
  as.matrix(grids)
}


#' Match-by-position rule
#'
#' Each run combines elements at the same position across slots.
#' Slots must all have the same length.
#'
#' @param lens Integer vector of slot lengths.
#' @return A matrix of dimension \code{(n, length(lens))}.
#' # Three slots, each of length 4.
#' # Output: a 4 x 3 matrix, with each row corresponding to positions 1, 2, 3, 4.
#' rule_match(c(4, 4, 4))
#'
#' @author Andrew Roberts
#' @export
rule_match <- function(lens) {
  .check_lens(lens)
  
  if(length(unique(lens)) != 1L) {
    stop("`rule_match()` requires dimension lengths in `lens` to all be equal.")
  }
  
  n <- lens[1]
  idx <- matrix(rep(seq_len(n), each=length(lens)),
                ncol=length(lens), byrow=TRUE)
  return(idx)
}


#' Rule to match and broadcast slot indices
#'
#' Generates a matrix of slot indices for broadcasting, similar to array broadcasting
#' in Python's NumPy, by matching slot lengths. For each slot (dimension), if the
#' corresponding length in \code{lens} is 1, the index is recycled to match the maximal
#' dimension size; if it matches the maximal size, indices range from 1 to that size.
#' Throws an error if a slot length is not 1 or maximal.
#'
#' @param lens An integer vector specifying the lengths of each slot (dimension).
#'   Each element should be either 1 (scalar, to be broadcast) or max(\code{lens}),
#'   representing the dimension size for that slot.
#' @return An integer matrix with \code{max(lens)} rows and \code{length(lens)}
#'   columns. Each row represents one combination of broadcasted slot indices,
#'   with scalars repeated as needed.
#' @examples
#' rule_match(c(1, 3, 1))
#' #      [,1] [,2] [,3]
#' # [1,]    1    1    1
#' # [2,]    1    2    1
#' # [3,]    1    3    1
#'
#' rule_match(c(3, 1, 3))
#' #      [,1] [,2] [,3]
#' # [1,]    1    1    1
#' # [2,]    2    1    2
#' # [3,]    3    1    3
#'
#' # Invalid usage:
#' # rule_match(c(2, 3))
#' # Error: Lengths in `lens` must be either 1 or max(lens) for broadcasting.
#'
#' @seealso For general information on broadcasting in array programming,
#'   see NumPy or rray documentation.
#'
#' @author Andrew Roberts
#' @export
rule_broadcast <- function(lens) {
  .check_lens(lens)
  
  n <- max(lens)
  idx <- matrix(NA_integer_, nrow=n, ncol=length(lens))
  
  for(j in seq_along(lens)) {
    if(lens[j] == 1L) {
      idx[,j] <- 1L # broadcast the scalar index 1
    } else if(lens[j] == n) {
      idx[,j] <- seq_len(n)
    } else {
      stop("Lengths in `lens` must be either 1 or max(lens) for broadcasting.")
    }
  }
  
  return(idx)
}


#' Create recycled slot indices for broadcasting along specific axes
#'
#' Generates a matrix of indices for each slot (dimension), recycling shorter axes
#' so their indices repeat until the longest length is reached. Throws an error if
#' the lengths are not compatible for recycling (i.e., all lens must divide the maximal length).
#'
#' @param lens Integer vector of lengths for each slot (dimension).
#' @return An integer matrix of shape (\code{max_len}, \code{length(lens)}), where
#'   each column is the recycled sequence of indices for that axis.
#' @examples
#' # Recycles: first axis (length 3) recycled to match second axis (length 6)
#' recycle_indices(c(3, 6))
#' # returns:
#' #      [,1] [,2]
#' # [1,]    1    1
#' # [2,]    2    2
#' # [3,]    3    3
#' # [4,]    1    4
#' # [5,]    2    5
#' # [6,]    3    6
#'
#' # Error: incompatible recycling
#' # recycle_indices(c(4, 6))
#' # Error: All dimensions in 'lens' must evenly divide the maximal length for recycling.
#'
#' @author Andrew Roberts
#' @export
rule_recycle <- function(lens) {
  .check_lens(lens)

  max_len <- max(lens)
  if(any(max_len %% lens != 0)) {
    stop("All dimensions in `lens` must evenly divide the maximal length for recycling.")
  }
  
  # Create recycled indices for each slot
  idx_mat <- vapply(lens, function(l) rep(seq_len(l), length.out=max_len), integer(max_len))

  # Ensure matrix return value even if `lens` is length 1
  idx_mat <- as.matrix(idx_mat)
  colnames(idx_mat) <- NULL
  idx_mat
}


#' Combine rules by grouping slots
#'
#' Constructs a new broadcasting rule function by combining existing rule
#' functions. The function applies the sub-rules to "slot groups", and then 
#' takes the cartesian product of the resulting input matrices.
#'
#' @param groups A list of integer vectors, each giving the positions
#'   of slots handled by one sub-rule.
#' @param rules A list of rule functions, one per group.
#' @returns A new rule function.
#' 
#' @examples
#' # Rule A handles slots 1:2, rule B handles slots 3:4, then cartesian them together
#' lens <- c(2, 2, 4, 1)
#' rule_composite <- get_composite_rule(groups = list(1:2, 3:4), 
#'                                      rules = list(rule_match, rule_broadcast))
#' rule_composite(lens) # Outputs 8 by 4 matrix.
#' 
#' @author Andrew Roberts
#' @export
get_composite_rule <- function(groups, rules) {
  
  if(length(groups) != length(rules)) {
    stop("`get_composite_rule()` requires `groups` and `rules` to be lists of equal length.")
  }
  
  # Composite rule function
  function(lens) {

    # Apply each rule to its group
    group_mats <- vector("list", length(groups))
    for(k in seq_along(groups)) {
      group_slots <- groups[[k]]
      rule_func <- rules[[k]]
      group_mats[[k]] <- rule_func(lens[group_slots])
    }
    
    # Cartesian product of the sub-results
    group_idx_lists <- lapply(group_mats, function(x) seq_len(nrow(x)))
    index_grid <- do.call(expand.grid, group_idx_lists)
    out <- matrix(NA_integer_, nrow=nrow(index_grid), ncol=length(lens))
    
    for(row_idx in seq_len(nrow(index_grid))) {
      for(group_idx in seq_along(groups)) {
        result_row <- index_grid[row_idx, group_idx]
        output_slots <- groups[[group_idx]]
        out[row_idx, output_slots] <- group_mats[[group_idx]][result_row,]
      }
    }
    
    return(out)
  }
}


#' Visualize an index matrix by labeling each slot
#'
#' Returns a character matrix the same shape as \code{idx_mat},
#' where each entry is 
#' \code{paste(slot_names[column], idx_mat[row, column], sep = "_")}.
#'
#' @param idx_mat Integer matrix of indices, with one column per slot.
#' @param slot_names Character vector of slot names (length must match \code{ncol(idx_mat)}).
#' @return Character matrix of same shape as idx_mat.
#' @examples
#' idx <- recycle_indices(c(2, 3))
#' visualize_slot_grid(idx, c("A", "B"))
#' #      [,1]  [,2]
#' # [1,] "A_1" "B_1"
#' # [2,] "A_2" "B_2"
#' # [3,] "A_1" "B_3"
#'
#' @author Andrew Roberts
#' @export
visualize_slot_grid <- function(idx_mat, slot_names) {
  
  if(!is.matrix(idx_mat) || !is_integer_like(idx_mat)) {
    stop("`idx_mat` must be a matrix with integer entries.")
  }
  
  if(length(slot_names) != ncol(idx_mat)) {
    stop("Length of `slot_names` must match number of columns of `idx_mat`.")
  }
  
  name_map <- function(col_vals, slot_name) paste(slot_name, col_vals, sep="_")
  char_mat <- Map(name_map, as.data.frame(idx_mat), slot_names)
  name_mat <- do.call(cbind, char_mat)
  dim(name_mat) <- dim(idx_mat) # In case result was flattened to vector
  colnames(name_mat) <- slot_names
  
  
  return(name_mat)
}


#' Fill Index Vector with Slot Values
#'
#' Given an index vector \code{idx}, produces a list of the same length.
#' The values in this list are taken from \code{slots}. The names of 
#' \code{idx} specify which slot to pull from, and the values of \code{idx}
#' specify which value to pull from that slot (by index).
#' 
#' @details
#' The names attribute of \code{idx} contain slot names, so that 
#' \code{nm <- names(idx)[j]} is the slot corresponding to element \code{j}
#' of \code{idx}. Element \code{j} of the returned list is then set to
#' the value \code{slots[[nm]][idx[j]]}. Note that \code{idx} is allowed to 
#' be of different length than \code{slots}; i.e., slots may be repeated or 
#' not used at all. If any names of \code{idx} are not valid slots in \code{slot},
#' an error is thrown. An error is also thrown if a value in \code{slot} is outside
#' the index range of a particular slot.
#' 
#' @param idx named integer vector.
#' @param slots named list of slots, with names defining the slot names. Each 
#' element is a list or vector of possible values for that slot.
#'
#' @return A list of length equal to the length of \code{idx} (see details).
#'
#' @author Andrew Roberts
#' @export
instantiate_slots <- function(idx, slots) {
  stopifnot(is_named_integer_vector(idx, check_unique_names=FALSE))
  stopifnot(is_named_list(slots, check_unique_names=TRUE))
  
  slot_names <- names(idx)
  
  # All slot names must exist in slots
  missing_slots <- setdiff(slot_names, names(slots))
  if(length(missing_slots) > 0L) {
    stop("Slot name(s) not found in `slots`: ", paste(missing_slots, collapse=", "))
  }
  
  slot_vals <- Map(function(nm, i) .get_slot_value(nm, i, slots),
                   slot_names, idx)
  names(slot_vals) <- slot_names
  
  return(slot_vals)
}


#' Fill Index Matrix with Slot Values
#'
#' For each row of idx_mat, uses indices to pull values from slots, constructing
#' a tibble (one column per slot). Handles structured (list-like) values as list 
#' columns.
#'
#' @param idx_mat Integer matrix, with each row an index tuple for the slots.
#' @param slots List of length ncol(idx_mat), each element is a list or vector 
#'    of possible values for that slot.
#' @param include_rownames logical, if \code{TRUE} and \code{idx_mat} has 
#'  rownames, then these rownames are added as a column with name specified by
#'  the argument \code{rownames_col}. If \code{FALSE}, this column is not added.
#'
#' @return A tibble where each row is a combination of slot values, and list-like 
#'    slot values become list columns. The tibble has the same number of rows
#'    as \code{idx_mat}.
#' 
#' @examples
#' library(tibble)
#' idx <- rule_recycle(c(4, 2))
#' slots <- list(
#'   c("A", "B", "C", "D"),
#'   list(1:2, 3:4) # slot 2 as a list column
#' )
#' instantiate_slot_grid(idx, slots)
#'
#' @author Andrew Roberts
#' @export
instantiate_slot_grid <- function(idx_mat, slots, include_rownames=FALSE,
                                  rownames_col="row_names") {
  stopifnot(is.matrix(idx_mat))
  stopifnot(is_integer_like(idx_mat))
  stopifnot(is.list(slots))
  stopifnot(ncol(idx_mat) == length(slots))
  stopifnot(is_character_scalar(rownames_col))
  
  n_slots <- ncol(idx_mat)
  n_combs <- nrow(idx_mat)
  slot_colnames <- names(slots)
  if(is.null(slot_colnames)) slot_colnames <- paste0("slot", seq_len(n_slots))
  
  create_column <- function(j) {
    vals <- slots[[j]]
    idxs <- idx_mat[,j]
    
    # If vals is a list, always create a list column
    if (is.list(vals)) {
      lapply(idxs, function(idx) vals[[idx]]) # allow for slots of length 0
    } else {
      # Vector - extract using standard vector indexing
      vals[idxs]
    }
  }
  
  cols <- lapply(seq_len(n_slots), create_column)
  names(cols) <- slot_colnames
  tbl <- tibble::as_tibble(cols)
  
  # Optionally add a column storing the rownames of `idx_mat`.
  if(include_rownames && !is.null(rownames(idx_mat))) {
    tbl <- tbl %>% dplyr::mutate(!!rownames_col := rownames(idx_mat))
  }
  
  return(tbl)
}


#' Extract Slot Value By Index
#'
#' Helper to extract a value from a particular slot. The slot is selected by
#' name, and the value in that slot is selected by index. No validation is 
#' done in this low-level helper. 
#' 
#' @param slot_name character(1), a string name in \code{names(slots)}
#' @param slot_idx integer(1), an index of the vector/list \code{slots[[slot_name]]}
#' @param slots named list, with list or vector elements
#' 
#' @returns The value \code{slots[[slot_name]][[slot_idx]]}.
#' @author Andrew Roberts
.get_slot_value <- function(slot_name, slot_idx, slots) {
  slots[[slot_name]][[slot_idx]]
}


#' Validation for vector containing dimension lengths
#'
#' Broadcast rule functions operate on integer vectors, where the jth entry
#' contains the length of the jth dimension. This function checks that 
#' this integer vector is valid.
#'
#' @param lens An R object
#' 
#' @returns logical(1), \code{TRUE} is \code{lens} satisfies the requirements to
#'  be considered proper array dimension lengths.
#'  
#' @author Andrew Roberts
.check_lens <- function(lens) {
  
  if(!is_nonneg_integer_vector(lens) && all(lens > 0)) {
    stop("Slot dimension lengths `lens` must be vector of positive integers.")
  }
  
  if(length(lens) == 0L) {
    stop("Slot dimension lengths `lens` has length zero.")
  }
}

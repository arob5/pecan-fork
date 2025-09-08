# forward_model/ensemble_input_broadcast.r

#' Construct ensemble model input in roadcast format
#' 
#' Constructs an object of class \code{EnsembleInputBroadcast}, which consists
#' of a set of unique values for each slot, along with a broadcast rule.
#' 
#' @details
#'
#' @param slots A named list of slot value sets.
#' @param rule A rule function that defines broadcasting,
#'   returning an index matrix.
#' @return An object of class \code{EnsembleInputBroadcast}, inheriting from 
#'  \code{EnsembleInput}.
#' @export
EnsembleInput.function <- function(broadcast_rule, slots) {
  
  x <- .make_ensemble_input_broadcast(broadcast_rule, slots)
  validate_ensemble_input_broadcast(x)
  
  return(x)
}
  

.make_ensemble_input_broadcast <- function() {
  stopifnot(is.list(slots), is.function(rule))
  lens <- vapply(slots, length, integer(1))
  idx <- rule(lens)
  
  
  structure(
    list(slots=slots, rule=rule, index=idx),
    class = c("EnsembleInputBroadcast", "EnsembleInput")
  )
}








#' #' Visualize the run index or values of a broadcast ensemble
#' #'
#' #' Shows how slots are combined into runs, either as indices or as
#' #' actual slot values. This is useful for debugging broadcast rules.
#' #'
#' #' @param emb An `ensemble_model_input_broadcast` object.
#' #' @param show Character, either "index" (default) to show integer indices
#' #'   or "value" to show the actual slot values.
#' #' @param as_tibble Logical; if TRUE and tibble is available, return a tibble.
#' #' @return A data.frame (or tibble) where each column corresponds to a slot
#' #'   and each row corresponds to one run.
#' #' @export
#' visualize_broadcast <- function(emb, show = c("index", "value"), as_tibble = TRUE) {
#'   stopifnot(inherits(emb, "ensemble_model_input_broadcast"))
#'   show <- match.arg(show)
#'   slot_names <- names(emb$slots)
#'   
#'   if (show == "index") {
#'     df <- as.data.frame(emb$index, stringsAsFactors = FALSE)
#'   } else {
#'     df <- as.data.frame(
#'       lapply(seq_along(slot_names), function(k) {
#'         vapply(seq_len(nrow(emb$index)),
#'                function(i) emb$slots[[k]][[emb$index[i, k]]],
#'                FUN.VALUE = emb$slots[[k]][[1]])
#'       }),
#'       stringsAsFactors = FALSE
#'     )
#'   }
#'   names(df) <- slot_names
#'   
#'   if (as_tibble && requireNamespace("tibble", quietly = TRUE)) {
#'     df <- tibble::as_tibble(df)
#'   }
#'   df
#' }









  
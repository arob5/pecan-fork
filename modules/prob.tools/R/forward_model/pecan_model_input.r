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

# Say we want "slot" and "inputs" (the whole inputs block of the XML) to be
# considered "slots". We mark these with "slot_" prefix. If no slot prefixes
# are provided, marks all tree leaves as slots.


PecanModelInput <- function(x, ...) {
  UseMethod("PecanModelInput")
}


PecanModelInput.list <- function(input_list, ...) {
  
}



PecanModelInput <- function(rundir=NULL, modeloutdir=NULL,
                            settings_inputs=list(), runtime_inputs=list()) {
  
  x <- .new_pecan_model_input(run_dir, modelout_dir, settings_inputs, runtime_inputs)
  validate_pecan_model_input(x)
  
  return(x)
}


.new_pecan_model_input <- function(run_dir, modelout_dir, site,
                                   settings_inputs, runtime_inputs) {

}


validate_pecan_model_input <- function(x) {
  
  validate_model_input(x)

}


#' Check if object inherits from \code{PecanModelInput}
#' 
#' @param x An object
#' @returns Logical, whether or not the object inherits from \code{PecanModelInput}.
#' 
#' @seealso \code{\link{PecanModelInput}}
#' @author Andrew Roberts
#' @export
is_pecan_model_input <- function(x) {
  inherits(x, "PecanModelInput")
}


#' Throw error if object is not \code{PecanModelInput}
#' 
#' @param x An object
#' @returns Invisibly returns \code{TRUE} if \code{x} is a \code{PecanModelInput}.
#'  Otherwise throws an error.
#' 
#' @seealso \code{\link{PecanModelInput}}
#' @author Andrew Roberts
#' @export
check_pecan_model_input_type <- function(x) {
  if(!is_pecan_model_input(x)) stop("`x` is not a PecanModelInput object.")
  
  invisible(TRUE)
}


overwrite_settings_defaults <- function(model_input, settings) {
  
  check_pecan_model_input_type(model_input)
  assertthat::assert_that(PEcAn.settings::is.Settings(settings))

  settings <- update_settings_value(settings)
  
  
  if(!is.null(model_input$rundir)) settings$rundir <- model_input$rundir
  if(!is.null(model_input$modeloutdir)) settings$modeloutdir <- model_input$modeloutdir

}








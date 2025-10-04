# forward_model/run_model_api.r
#
# Depends: PEcAn.settings, PEcAn.utils

# Implements standardized protocols for running PEcAn models, ensuring a 
# consistent interface for specifying outputs and returning outputs in a standard
# format. The lowest-level building block consists of an API for prepping a 
# model run (i.e., writing configs). Higher level protocols define interfaces
# for running models (prepping then running) and running models and reading 
# their outputs (prepping, running, and reading). APIs are provided for both
# single runs and ensemble model runs. Ensemble model runs are defined as 
# a set of model runs of a model, with each run using (potentially) different inputs.
#
# APIs for single model run:
#   run_model(), prep_pecan_model_run(), start_pecan_model_run()    
# 
# APIs for ensemble model run:
#   run_model_ensemble(), prep_pecan_ensemble_run(), start_pecan_ensemble_run()
#
# `run_model.Settings()` and `run_model_ensemble.Settings()` are the PEcAn methods
# for the run model and run model ensemble generics.


# ------------------------------------------------------------------------------
# Single Model Run
# ------------------------------------------------------------------------------


#' Run a PEcAn model and return outputs
#'
#' A standardized API for a single PEcAn model run, which entails writing a
#' model config, starting the model run, and reading outputs from file.
#' 
#' @details
#' At present, this API is defined so that PEcAn's standard model run workflow
#' can be executed without modification. A PEcAn model execution requires 
#' the inputs \code{trait.values}, \code{run.id}, and \code{settings}. 
#' PEcAn \code{Settings} objects contain settings beyond those necessary to run the
#' model. Thus, to be more specific, the \code{settings} input is typically used
#' to specify required driver (meteorology) and initial condition (IC) inputs
#' via the fields \code{settings$run$inputs$met, settings$run$inputs$poolinitcond}.
#' Beyond model inputs, the settings object defines the model to be run via
#' \code{settings$model}, as well as the filepaths:
#' \itemize{
#'  \item \code{settings$outdir}: the highest level output directory for the run.
#'  \item \code{settings$modeloutdir}: directory to which model outputs will be written.
#'  \item \code{settings$rundir}: directory to which config files will be saved.
#' }
#'     
#' The PEcAn workflow allows users to manually pass in model parameters via
#' the \code{trait.values} argument. Some models allow runtime specification
#' of other model inputs (e.g., initial conditions).
#' 
#' To respect the PEcAn workflow structure, the run model API is structured as follows:  
#' \itemize{
#'  \item \code{settings$outdir}: \code{settings}
#'  \item \code{settings$modeloutdir}: directory to which model outputs will be written.
#'  \item \code{settings$rundir}: directory to which config files will be saved.
#' }
#'
#' @param settings A PEcAn \code{Settings} object.
#' @param model_input A \code{ModelInput} object.
#' @param run_id character(1), a unique string identifier for the run. Will be 
#'  auto-generated if not provided.
#' @param overwrite_runs_file logical(1), if \code{TRUE}, overwrites any existing
#'  \code{runs.txt} file in the run directory. Otherwise, appends to this file.
#'  If appending, note that any runs executed from this directory using
#'  \code{PEcAn.workflow::start_model_runs()} will run both the newly appended
#'  and old runs. Even in this case, this method will still only return the 
#'  results associated with \code{run_id}, the current run. 
#' @param ... Additional arguments passed to \code{PEcAn.utils::read.output()}.
#'
#' @returns The model outputs corresponding to the model run with ID \code{run_id}.
#'  The outputs are formatted as returned by \code{PEcAn.utils::read.output()}.
#'  An attribute \code{run_id} is attached to the output object.
#'  
#' @author Andrew Roberts
#' @export
run_model.Settings <- function(settings, model_input, run_id=NULL, 
                               overwrite_runs_file=FALSE, ...) {
  
  # Write configs, start model run, write outputs to file.
  run_id <- start_pecan_model_run(settings=settings,
                                  model_input=model_input,
                                  run_id=run_id,
                                  overwrite_runs_file=overwrite_runs_file)
  
  # Read outputs from file.
  run_output_path <- file.path(settings$modeloutdir, run_id)
  model_output <- PEcAn.utils::read.output(run_id, outdir=run_output_path, ...)
  attr(model_output, "run_id") <- run_id
  
  return(model_output)
}


start_pecan_model_run <- function(settings, model_input, run_id=NULL,
                                  overwrite_runs_file=FALSE) {
  
  run_id <- prep_pecan_model_run(settings=settings, 
                                 model_input=model_input, 
                                 run_id=run_id, 
                                 overwrite_runs_file=overwrite_runs_file)
  
  PEcAn.workflow::start_model_runs(settings, write=FALSE)
  
  return(invisible(run_id))
  
}


#' Write Configuration Files to Disk
#' 
#' Writes inputs for a PEcAn model to file, and writes the \code{run_id} to the
#' \code{runs.txt} file. Does not actually run the model - prepares the model
#' for execution using \code{PEcAn.workflow::start_model_runs()}.
#'
#'  
#' @returns Invisibly returns the \code{run_id}. Loads the PEcAn model package
#'  and uses the write config function from this package to write configuration
#'  files to disk.
prep_pecan_model_run <- function(settings, model_input, run_id=NULL, 
                                 overwrite_runs_file=FALSE) {

  .check_pecan_model_input_type(model_input)
  
  # Overwrite defaults in `settings` with values specified in `model_input`.
  settings <- update_pecan_settings(settings, model_input)
  
  # If run ID is not provided, randomly generate one.
  if(is.null(run_id) || is.na(run_id)) run_id <- uuid::UUIDgenerate()

  # Load model package.
  model_type <- settings$model$type
  PEcAn.utils::load.modelpkg(model_type)
  
  # Create run and output directories.
  dir.create(file.path(settings$rundir, run_id), recursive=TRUE)
  dir.create(file.path(settings$modeloutdir, run_id), recursive=TRUE)
  
  # Write model config to file.
  model_write_config <- paste0("write.config.", model_type)
  config_args <- list(settings=settings, defaults=settings$pfts, run.id=run_id)
  config_args <- c(config_args, config_args(model_input))
  do.call(model_write_config, args=config_args)
  
  # Either append to or overwrite existing "runs.txt" file.
  cat(as.character(run_id),
      file=file.path(settings$rundir, "runs.txt"),
      sep="\n",
      append=!overwrite_runs_file)
  
  return(invisible(run_id))
}


# ------------------------------------------------------------------------------
# Model Ensemble Run
# ------------------------------------------------------------------------------


run_model_ensemble.Settings <- function(settings, ens_input,
                                        overwrite_runs_file=FALSE, ...) {

  run_ids <- start_pecan_model_ensemble_run(settings, ens_input,
                                            overwrite_runs_file=overwrite_runs_file)
  
  read_single_run_output <- function(run_id) {
    PEcAn.utils::read.output(run_id,
                             outdir = file.path(settings$modeloutdir, run_id),
                             ...)
  }
  
  # Read list of model output from each run
  model_ens_output <- lapply(run_ids, read_single_run_output)
  setNames(model_ens_output, run_ids)
}


start_pecan_model_ensemble_run <- function(settings, ens_input,
                                           overwrite_runs_file=FALSE) {
  
  run_ids <- prep_pecan_model_ensemble_run(settings, ens_input, 
                                           overwrite_runs_file=overwrite_runs_file)
  
  # TODO: need to enforce requirement that rundir is constant across all runs.
  # Need to identify what components of settings is used by start_model_runs().
  # If it is just rundir then we can create a settings object with the 
  # constant rundir value and pass that here.
  PEcAn.workflow::start_model_runs(settings, write=FALSE)
  
  return(invisible(run_ids))
}


prep_pecan_model_ensemble_run <- function(settings, ens_input, 
                                          overwrite_runs_file=FALSE) {
  
  # TODO: need to fix `run_ids` method to avoid confusion with common variable name.
  r_ids <- run_ids(ens_input)
  
  prep_single_run <- function(i) {
    run_id <- r_ids[i]
    overwrite <- (i == 1) && overwrite_runs_file

    prep_pecan_model_run(settings = settings, 
                         model_input = get_run_input(ens_input, run_id),
                         run_id = run_id,
                         overwrite_runs_file = overwrite)
  }
  
  r_ids <- vapply(seq_along(r_ids), prep_single_run, character(1))
  
  invisible(r_ids)
}


#' Determine modeloutdir for a particular run in the ensemble
#'
#' Given the \code{run_id}, determines the `modeloutdir` used for that run.
#' This value will be taken from `ens_input` if it is overwritten; otherwise,
#' it will be the default in `settings`.
#' 
#' @param settings PEcAn \code{Settings} object containing defaults.
#' @param ens_input An \code{EnsembleInput} object.
#' @param run_id character(1), the run ID.
#' 
#' @returns The `modeloutdir` value for the run.
#' 
#' @author Andrew Roberts
#' @export
get_run_modeloutdir <- function(settings, ens_input, run_id) {
  model_input <- get_run_input(ens_input, run_id)
  resolve_pecan_settings_value(settings, model_input, "modeloutdir")
}

## ======================================================================================================================= ##
## Script:    MODELING TASK DATA
## ======================================================================================================================= ##
## Authors:   Lukas J. Gunschera
## Date:      Wed Feb 12 12:26:30 2025
## ======================================================================================================================= ##
##
## ======================================================================================================================= ##

## FUNCTION =================================================================================================================

fit_stan_models <- function(models_to_run = NULL, suffix = "") {
  ### Setup -----------------------------------------------------------------------------------------------------------------

  start.time <- Sys.time()

  library(cmdstanr)
  library(here)
  library(tidyr)
  library(dplyr)
  library(purrr)

  # Setup parallel processing
  options(mc.cores = parallel::detectCores(logical = FALSE)) # Use physical cores

  #### Load data ------------------------------------------------------------------------------------------------------------

  # rename list
  names(data)[names(data) == "game"] <- "task_data"
  data$task_data$session_id <- data$task_data$task_condition


  data_cleaned <- modeling_cleandata(data$task_data)

  task_data_baseline <- data_cleaned$baseline
  task_data_devalued <- data_cleaned$devalued

  model_dat_baseline <- model_preprocessing(
    raw_data = task_data_baseline,
    retest = FALSE,
    subjs = unique(task_data_baseline$subj_id),
    n_subj = length(unique(task_data_baseline$subj_id)),
    t_subjs = aggregate(trial ~ subj_id, FUN = max, data = task_data_baseline)[, 2],
    t_max = max(aggregate(trial ~ subj_id, FUN = max, data = task_data_baseline)[, 2])
  )

  model_dat_devalued <- model_preprocessing(
    raw_data = task_data_devalued,
    retest = FALSE,
    subjs = unique(task_data_devalued$subj_id),
    n_subj = length(unique(task_data_devalued$subj_id)),
    t_subjs = aggregate(trial ~ subj_id, FUN = max, data = task_data_devalued)[, 2],
    t_max = max(aggregate(trial ~ subj_id, FUN = max, data = task_data_devalued)[, 2])
  )

  #### Create function dependencies -----------------------------------------------------------------------------------------

  # compile single model
  compile_model <- function(model_name, model_type) {
    model_path <- here::here("code", "stan", model_type, model_name)
    model <- cmdstanr::cmdstan_model(model_path)
    return(model)
  }

  #### Select models to fit -------------------------------------------------------------------------------------------------

  models <- expand.grid(
    model_name = c(
      "ed_m1_parabolic.stan", "ed_m2_parabolic.stan", "ed_m3_parabolic.stan",
      "ed_m1_linear.stan", "ed_m2_linear.stan", "ed_m3_linear.stan",
      "ed_m1_exponential.stan", "ed_m2_exponential.stan", "ed_m3_exponential.stan"
    ),
    baseline = c(TRUE, FALSE)
  ) %>%
    mutate(results_name = paste0(
      sub("\\.stan$", "", sub("ed_", "", model_name)),
      ifelse(baseline, "_baseline", "_devalued"),
      suffix
    )) %>%
    mutate(model_type = case_when(
      grepl("parabolic", model_name) ~ "models_parabolic",
      grepl("linear", model_name) ~ "models_linear",
      grepl("exponential", model_name) ~ "models_exponential"
    ))

  # filter models if specific models are provided
  if (!is.null(models_to_run)) {
    models <- models %>%
      filter(model_name %in% paste0("ed_", models_to_run, ".stan"))
  }

  #### Compile models -------------------------------------------------------------------------------------------------------

  compiled_models <- map2(models$model_name, models$model_type, compile_model)

  ### Model Fit Function ----------------------------------------------------------------------------------------------------

  stan_sample <- function(model, model_name, results_name, baseline) {
    model_data <- if (baseline) model_dat_baseline else model_dat_devalued
    task_data <- if (baseline) task_data_baseline else task_data_devalued

    paste0("Running model for:   ", task_data$session_id[1])

    # assign parameter names based on model type
    if (grepl("m1", model_name)) {
      parameter_names <- c("kE", "a")
    } else if (grepl("m2", model_name)) {
      parameter_names <- c("kE", "kR")
    } else if (grepl("m3", model_name)) {
      parameter_names <- c("kE", "kR", "a")
    } else {
      stop("Unknown model type in model_name: ", model_name)
    }

    # fit model
    model_fit <- model$sample(
      data = model_data,
      refresh = 0, chains = 4, parallel_chains = 4,
      iter_warmup = 2000, iter_sampling = 10000,
      adapt_delta = 0.8, step_size = 1, max_treedepth = 10, save_warmup = TRUE
    )

    # extract parameter estimates
    model_params <- get_params(
      subj_id = unique(task_data$subj_id),
      model_fit = model_fit,
      n_subj = length(unique(task_data$subj_id)),
      n_params = length(parameter_names),
      param_names = parameter_names
    )

    # handle missing columns before pivoting
    expected_params <- c("estimate", "hdi_lower", "hdi_upper")
    missing_params <- setdiff(expected_params, names(model_params$individual_params))
    for (param in missing_params) {
      model_params$individual_params[[param]] <- NA
    }

    # pivot parameters
    model_params$individual_params <- model_params$individual_params %>%
      tidyr::pivot_wider(
        id_cols = subj_id,
        names_from = parameter,
        values_from = c(estimate, hdi_lower, hdi_upper),
        names_glue = "{.value}_{parameter}",
        values_fill = NA
      )

    # convergence check
    model_converge <- convergence_check(model_fit,
      params = parameter_names,
      Rhat = TRUE, ess = TRUE, trace_plot = TRUE, rank_hist = FALSE
    )

    # compute LOO
    model_loo <- model_fit$loo()

    # Store results in a named list
    results <- list(
      parameters = model_params,
      convergence = list(Rhat = model_converge$Rhat, ess = model_converge$ess),
      trace_plot = model_converge$trace_plot,
      loo = model_loo
    )

    # save results as a single RDS file
    saveRDS(results, here::here("data", "experiment2", "modelling", paste0(results_name, "_results.RDS")))

    # Assign results to a dynamically named variable in the global environment
    assign(results_name, results, envir = .GlobalEnv)

    # create a name for the model_fit object
    fit_name <- paste0(sub("\\.stan$", "", sub("ed_", "", model_name)), "_fit_", ifelse(baseline, "baseline", "devalued"))

    # assign the model_fit object to the global environment
    assign(fit_name, model_fit, envir = .GlobalEnv)

    return(results) # Return results to environment
  }

  # use purrr::pmap() to process models in parallel
  results_list <- pmap(
    list(compiled_models, models$model_name, models$results_name, models$baseline),
    stan_sample
  )

  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
}

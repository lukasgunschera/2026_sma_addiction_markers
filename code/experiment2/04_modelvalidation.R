## ======================================================================================================================= ##
## Script:    Modelling Task Data
## ======================================================================================================================= ##
## Authors:   Lukas J. Gunschera
## Date:      Mon Nov 17 15:06:38 2025
## ======================================================================================================================= ##
##
## ======================================================================================================================= ##

# set parameters and seed
source(here::here("code", "00_setup.R"))

library(renv)
renv::restore() # restore the environment

### Set Parameters-----------------------------------------------------------------------------------------------------------

fit_models <- TRUE # TRUE = model(s) are fitted, FALSE = model(s) are loaded from file

models_to_run <- c(
  "m1_parabolic", "m2_parabolic", "m3_parabolic",
  "m1_linear", "m2_linear", "m3_linear",
  "m1_exponential", "m2_exponential", "m3_exponential"
)

set.seed(777)

# load dependencies
pacman::p_load(zoo, plm, ragg, here, purrr, dplyr, rstatix, janitor, ggplot2, cmdstanr, magrittr, hBayesDM, ggpattern, bayesplot, lubridate, BayesFactor)

here::i_am("renv.lock")

source(here::here("code", "experiment2", "functions", "fun_plots.R"))
source(here::here("code", "experiment2", "functions", "fun_helper.R"))
source(here::here("code", "experiment2", "functions", "fun_fit_models.R"))
source(here::here("code", "experiment2", "functions", "fun_model_preprocess.R"))
source(here::here("code", "experiment2", "functions", "fun_model_comparison.R"))
source(here::here("code", "experiment2", "functions", "fun_modeling_cleandata.R"))
source(here::here("code", "experiment2", "functions", "fun_parameter_estimates.R"))
source(here::here("code", "experiment2", "functions", "fun_posterior_predictions.R"))
source(here::here("code", "experiment2", "functions", "fun_model_convergence_check.R"))

# load data from file (preprocessed data)
data <- readRDS(here::here("data", "experiment2", "processed", "data_clean.RDS"))
task_data <- data$game

## FIT OR LOAD MODELS =======================================================================================================

### Fit Models --------------------------------------------------------------------------------------------------------------

if (fit_models) {
  fit_stan_models(models_to_run)

  ### Save Results ----------------------------------------------------------------------------------------------------------

  #### Parabolic discounting models -----------------------------------------------------------------------------------------

  ##### PD Model 1 ----------------------------------------------------------------------------------------------------------

  # convergence check
  m1_para_check_baseline <- convergence_check(m1_parabolic_fit_baseline,
    params = c("kE", "a"),
    Rhat = TRUE, ess = TRUE,
    trace_plot = TRUE,
    rank_hist = FALSE
  )

  m1_para_check_devalued <- convergence_check(m1_parabolic_fit_devalued,
    params = c("kE", "a"),
    Rhat = TRUE, ess = TRUE,
    trace_plot = TRUE,
    rank_hist = FALSE
  )

  # show trace plots for conditions
  m1_para_check_baseline$trace_plot
  m1_para_check_devalued$trace_plot

  # save results
  saveRDS(
    list(m1_para_check_baseline$Rhat, m1_para_check_baseline$ess),
    here::here("data", "experiment2", "modelling", "modelfit", "m1_parabolic_check_baseline.RDS")
  )

  saveRDS(
    list(m1_para_check_devalued$Rhat, m1_para_check_devalued$ess),
    here::here("data", "experiment2", "modelling", "modelfit", "m1_parabolic_check_devalued.RDS")
  )

  # LOO for model comparisons
  m1_para_loo_baseline <- m1_parabolic_fit_baseline$loo()
  saveRDS(m1_para_loo_baseline, here::here("data", "experiment2", "modelling", "modelfit", "m1_parabolic_loo_baseline.RDS"))

  m1_para_loo_devalued <- m1_parabolic_fit_devalued$loo()
  saveRDS(m1_para_loo_devalued, here::here("data", "experiment2", "modelling", "modelfit", "m1_parabolic_loo_devalued.RDS"))

  # parameter estimates
  m1_para_params_baseline <- get_params(
    subj_id = unique(task_data$subj_id),
    model_fit = m1_parabolic_fit_baseline,
    n_subj = length(unique(task_data$subj_id)),
    n_params = 2,
    param_names = c("kE", "a")
  )

  m1_para_params_devalued <- get_params(
    subj_id = unique(task_data$subj_id),
    model_fit = m1_parabolic_fit_devalued,
    n_subj = length(unique(task_data$subj_id)),
    n_params = 2,
    param_names = c("kE", "a")
  )

  # save results
  saveRDS(m1_para_params_baseline, here::here("data", "experiment2", "modelling", "modelfit", "m1_parabolic_parameters_baseline.RDS"))
  saveRDS(m1_para_params_devalued, here::here("data", "experiment2", "modelling", "modelfit", "m1_parabolic_parameters_devalued.RDS"))

  ##### PD Model 2 ----------------------------------------------------------------------------------------------------------

  # convergence check
  m2_para_check_baseline <- convergence_check(m2_parabolic_fit_baseline,
    params = c("kE", "kR"),
    Rhat = TRUE, ess = TRUE,
    trace_plot = TRUE,
    rank_hist = FALSE
  )

  m2_para_check_devalued <- convergence_check(m2_parabolic_fit_devalued,
    params = c("kE", "kR"),
    Rhat = TRUE, ess = TRUE,
    trace_plot = TRUE,
    rank_hist = FALSE
  )

  # show trace plots for conditions
  m2_para_check_baseline$trace_plot
  m2_para_check_devalued$trace_plot

  # save results
  saveRDS(
    list(m2_para_check_baseline$Rhat, m2_para_check_baseline$ess),
    here::here("data", "experiment2", "modelling", "modelfit", "m2_parabolic_check_baseline.RDS")
  )

  saveRDS(
    list(m2_para_check_devalued$Rhat, m2_para_check_devalued$ess),
    here::here("data", "experiment2", "modelling", "modelfit", "m2_parabolic_check_devalued.RDS")
  )

  # LOO for model comparisons
  m2_para_loo_baseline <- m2_parabolic_fit_baseline$loo()
  saveRDS(m2_para_loo_baseline, here::here("data", "experiment2", "modelling", "modelfit", "m2_parabolic_loo_baseline.RDS"))

  m2_para_loo_devalued <- m2_parabolic_fit_devalued$loo()
  saveRDS(m2_para_loo_devalued, here::here("data", "experiment2", "modelling", "modelfit", "m2_parabolic_loo_devalued.RDS"))

  # parameter estimates
  m2_para_params_baseline <- get_params(
    subj_id = unique(task_data$subj_id),
    model_fit = m2_parabolic_fit_baseline,
    n_subj = length(unique(task_data$subj_id)),
    n_params = 2,
    param_names = c("kE", "kR")
  )

  m2_para_params_devalued <- get_params(
    subj_id = unique(task_data$subj_id),
    model_fit = m2_parabolic_fit_devalued,
    n_subj = length(unique(task_data$subj_id)),
    n_params = 2,
    param_names = c("kE", "kR")
  )

  # save results
  saveRDS(m2_para_params_baseline, here::here("data", "experiment2", "modelling", "modelfit", "m2_parabolic_parameters_baseline.RDS"))
  saveRDS(m2_para_params_devalued, here::here("data", "experiment2", "modelling", "modelfit", "m2_parabolic_parameters_devalued.RDS"))

  ##### PD Model 3 ----------------------------------------------------------------------------------------------------------

  # convergence check
  m3_para_check_baseline <- convergence_check(m3_parabolic_fit_baseline,
    params = c("kE", "kR", "a"),
    Rhat = TRUE, ess = TRUE,
    trace_plot = TRUE,
    rank_hist = FALSE
  )

  m3_para_check_devalued <- convergence_check(m3_parabolic_fit_devalued,
    params = c("kE", "kR", "a"),
    Rhat = TRUE, ess = TRUE,
    trace_plot = TRUE,
    rank_hist = FALSE
  )

  # show trace plots for conditions
  m3_para_check_baseline$trace_plot
  m3_para_check_devalued$trace_plot

  # save results
  saveRDS(
    list(m3_para_check_baseline$Rhat, m3_para_check_baseline$ess),
    here::here("data", "experiment2", "modelling", "modelfit", "m3_parabolic_check_baseline.RDS")
  )

  saveRDS(
    list(m3_para_check_devalued$Rhat, m3_para_check_devalued$ess),
    here::here("data", "experiment2", "modelling", "modelfit", "m3_parabolic_check_devalued.RDS")
  )

  # LOO for model comparisons
  m3_para_loo_baseline <- m3_parabolic_fit_baseline$loo()
  saveRDS(m3_para_loo_baseline, here::here("data", "experiment2", "modelling", "modelfit", "m3_parabolic_loo_baseline.RDS"))

  m3_para_loo_devalued <- m3_parabolic_fit_devalued$loo()
  saveRDS(m3_para_loo_devalued, here::here("data", "experiment2", "modelling", "modelfit", "m3_parabolic_loo_devalued.RDS"))

  # parameter estimates
  m3_para_params_baseline <- get_params(
    subj_id = unique(task_data$subj_id),
    model_fit = m3_parabolic_fit_baseline,
    n_subj = length(unique(task_data$subj_id)),
    n_params = 3,
    param_names = c("kE", "kR", "a")
  )

  m3_para_params_devalued <- get_params(
    subj_id = unique(task_data$subj_id),
    model_fit = m3_parabolic_fit_devalued,
    n_subj = length(unique(task_data$subj_id)),
    n_params = 3,
    param_names = c("kE", "kR", "a")
  )

  # save results
  saveRDS(m3_para_params_baseline, here::here("data", "experiment2", "modelling", "modelfit", "m3_parabolic_parameters_baseline.RDS"))
  saveRDS(m3_para_params_devalued, here::here("data", "experiment2", "modelling", "modelfit", "m3_parabolic_parameters_devalued.RDS"))

  #### Linear discounting models --------------------------------------------------------------------------------------------

  ##### LD Model 1 ----------------------------------------------------------------------------------------------------------

  # convergence check
  m1_lin_check_baseline <- convergence_check(m1_linear_fit_baseline,
    params = c("kE", "a"),
    Rhat = TRUE, ess = TRUE,
    trace_plot = TRUE,
    rank_hist = FALSE
  )

  m1_lin_check_devalued <- convergence_check(m1_linear_fit_devalued,
    params = c("kE", "a"),
    Rhat = TRUE, ess = TRUE,
    trace_plot = TRUE,
    rank_hist = FALSE
  )

  # show trace plots for conditions
  m1_lin_check_baseline$trace_plot
  m1_lin_check_devalued$trace_plot

  # save results
  saveRDS(
    list(m1_lin_check_baseline$Rhat, m1_lin_check_baseline$ess),
    here::here("data", "experiment2", "modelling", "modelfit", "m1_linear_check_baseline.RDS")
  )

  saveRDS(
    list(m1_lin_check_devalued$Rhat, m1_lin_check_devalued$ess),
    here::here("data", "experiment2", "modelling", "modelfit", "m1_linear_check_devalued.RDS")
  )

  # LOO for model comparisons
  m1_lin_loo_baseline <- m1_linear_fit_baseline$loo()
  saveRDS(m1_lin_loo_baseline, here::here("data", "experiment2", "modelling", "modelfit", "m1_linear_loo_baseline.RDS"))

  m1_lin_loo_devalued <- m1_linear_fit_devalued$loo()
  saveRDS(m1_lin_loo_devalued, here::here("data", "experiment2", "modelling", "modelfit", "m1_linear_loo_devalued.RDS"))

  # parameter estimates
  m1_lin_params_baseline <- get_params(
    subj_id = unique(task_data$subj_id),
    model_fit = m1_linear_fit_baseline,
    n_subj = length(unique(task_data$subj_id)),
    n_params = 2,
    param_names = c("kE", "a")
  )

  m1_lin_params_devalued <- get_params(
    subj_id = unique(task_data$subj_id),
    model_fit = m1_linear_fit_devalued,
    n_subj = length(unique(task_data$subj_id)),
    n_params = 2,
    param_names = c("kE", "a")
  )

  # save results
  saveRDS(m1_lin_params_baseline, here::here("data", "experiment2", "modelling", "modelfit", "m1_linear_parameters_baseline.RDS"))
  saveRDS(m1_lin_params_devalued, here::here("data", "experiment2", "modelling", "modelfit", "m1_linear_parameters_devalued.RDS"))

  ##### LD Model 2 ----------------------------------------------------------------------------------------------------------

  # convergence check
  m2_lin_check_baseline <- convergence_check(m2_linear_fit_baseline,
    params = c("kE", "kR"),
    Rhat = TRUE, ess = TRUE,
    trace_plot = TRUE,
    rank_hist = FALSE
  )

  m2_lin_check_devalued <- convergence_check(m2_linear_fit_devalued,
    params = c("kE", "kR"),
    Rhat = TRUE, ess = TRUE,
    trace_plot = TRUE,
    rank_hist = FALSE
  )

  # show trace plots for conditions
  m2_lin_check_baseline$trace_plot
  m2_lin_check_devalued$trace_plot

  # save results
  saveRDS(
    list(m2_lin_check_baseline$Rhat, m2_lin_check_baseline$ess),
    here::here("data", "experiment2", "modelling", "modelfit", "m2_linear_check_baseline.RDS")
  )

  saveRDS(
    list(m2_lin_check_devalued$Rhat, m2_lin_check_devalued$ess),
    here::here("data", "experiment2", "modelling", "modelfit", "m2_linear_check_devalued.RDS")
  )

  # LOO for model comparisons
  m2_lin_loo_baseline <- m2_linear_fit_baseline$loo()
  saveRDS(m2_lin_loo_baseline, here::here("data", "experiment2", "modelling", "modelfit", "m2_linear_loo_baseline.RDS"))

  m2_lin_loo_devalued <- m2_linear_fit_devalued$loo()
  saveRDS(m2_lin_loo_devalued, here::here("data", "experiment2", "modelling", "modelfit", "m2_linear_loo_devalued.RDS"))

  # parameter estimates
  m2_lin_params_baseline <- get_params(
    subj_id = unique(task_data$subj_id),
    model_fit = m2_linear_fit_baseline,
    n_subj = length(unique(task_data$subj_id)),
    n_params = 2,
    param_names = c("kE", "kR")
  )

  m2_lin_params_devalued <- get_params(
    subj_id = unique(task_data$subj_id),
    model_fit = m2_linear_fit_devalued,
    n_subj = length(unique(task_data$subj_id)),
    n_params = 2,
    param_names = c("kE", "kR")
  )

  # save results
  saveRDS(m2_lin_params_baseline, here::here("data", "experiment2", "modelling", "modelfit", "m2_linear_parameters_baseline.RDS"))
  saveRDS(m2_lin_params_devalued, here::here("data", "experiment2", "modelling", "modelfit", "m2_linear_parameters_devalued.RDS"))

  ##### LD Model 3 ----------------------------------------------------------------------------------------------------------

  # convergence check
  m3_lin_check_baseline <- convergence_check(m3_linear_fit_baseline,
    params = c("kE", "kR", "a"),
    Rhat = TRUE, ess = TRUE,
    trace_plot = TRUE,
    rank_hist = FALSE
  )

  m3_lin_check_devalued <- convergence_check(m3_linear_fit_devalued,
    params = c("kE", "kR", "a"),
    Rhat = TRUE, ess = TRUE,
    trace_plot = TRUE,
    rank_hist = FALSE
  )

  # show trace plots for conditions
  m3_lin_check_baseline$trace_plot
  m3_lin_check_devalued$trace_plot

  # save results
  saveRDS(
    list(m3_lin_check_baseline$Rhat, m3_lin_check_baseline$ess),
    here::here("data", "experiment2", "modelling", "modelfit", "m3_linear_check_baseline.RDS")
  )

  saveRDS(
    list(m3_lin_check_devalued$Rhat, m3_lin_check_devalued$ess),
    here::here("data", "experiment2", "modelling", "modelfit", "m3_linear_check_devalued.RDS")
  )

  # LOO for model comparisons
  m3_lin_loo_baseline <- m3_linear_fit_baseline$loo()
  saveRDS(m3_lin_loo_baseline, here::here("data", "experiment2", "modelling", "modelfit", "m3_linear_loo_baseline.RDS"))

  m3_lin_loo_devalued <- m3_linear_fit_devalued$loo()
  saveRDS(m3_lin_loo_devalued, here::here("data", "experiment2", "modelling", "modelfit", "m3_linear_loo_devalued.RDS"))

  # parameter estimates
  m3_lin_params_baseline <- get_params(
    subj_id = unique(task_data$subj_id),
    model_fit = m3_linear_fit_baseline,
    n_subj = length(unique(task_data$subj_id)),
    n_params = 3,
    param_names = c("kE", "kR", "a")
  )

  m3_lin_params_devalued <- get_params(
    subj_id = unique(task_data$subj_id),
    model_fit = m3_linear_fit_devalued,
    n_subj = length(unique(task_data$subj_id)),
    n_params = 3,
    param_names = c("kE", "kR", "a")
  )

  # save results
  saveRDS(m3_lin_params_baseline, here::here("data", "experiment2", "modelling", "modelfit", "m3_linear_parameters_baseline.RDS"))
  saveRDS(m3_lin_params_devalued, here::here("data", "experiment2", "modelling", "modelfit", "m3_linear_parameters_devalued.RDS"))

  #### Exponential discounting models ---------------------------------------------------------------------------------------

  ##### ED Model 1 ----------------------------------------------------------------------------------------------------------

  # convergence check
  m1_exp_check_baseline <- convergence_check(m1_exponential_fit_baseline,
    params = c("kE", "a"),
    Rhat = TRUE, ess = TRUE,
    trace_plot = TRUE,
    rank_hist = FALSE
  )

  m1_exp_check_devalued <- convergence_check(m1_exponential_fit_devalued,
    params = c("kE", "a"),
    Rhat = TRUE, ess = TRUE,
    trace_plot = TRUE,
    rank_hist = FALSE
  )

  # show trace plots for conditions
  m1_exp_check_baseline$trace_plot
  m1_exp_check_devalued$trace_plot

  # save results
  saveRDS(
    list(m1_exp_check_baseline$Rhat, m1_exp_check_baseline$ess),
    here::here("data", "experiment2", "modelling", "modelfit", "m1_exponential_check_baseline.RDS")
  )

  saveRDS(
    list(m1_exp_check_devalued$Rhat, m1_exp_check_devalued$ess),
    here::here("data", "experiment2", "modelling", "modelfit", "m1_exponential_check_devalued.RDS")
  )

  # LOO for model comparisons
  m1_exp_loo_baseline <- m1_exponential_fit_baseline$loo()
  saveRDS(m1_exp_loo_baseline, here::here("data", "experiment2", "modelling", "modelfit", "m1_exponential_loo_baseline.RDS"))

  m1_exp_loo_devalued <- m1_exponential_fit_devalued$loo()
  saveRDS(m1_exp_loo_devalued, here::here("data", "experiment2", "modelling", "modelfit", "m1_exponential_loo_devalued.RDS"))

  # parameter estimates
  m1_exp_params_baseline <- get_params(
    subj_id = unique(task_data$subj_id),
    model_fit = m1_exponential_fit_baseline,
    n_subj = length(unique(task_data$subj_id)),
    n_params = 2,
    param_names = c("kE", "a")
  )

  m1_exp_params_devalued <- get_params(
    subj_id = unique(task_data$subj_id),
    model_fit = m1_exponential_fit_devalued,
    n_subj = length(unique(task_data$subj_id)),
    n_params = 2,
    param_names = c("kE", "a")
  )

  # save results
  saveRDS(m1_exp_params_baseline, here::here("data", "experiment2", "modelling", "modelfit", "m1_exponential_parameters_baseline.RDS"))
  saveRDS(m1_exp_params_devalued, here::here("data", "experiment2", "modelling", "modelfit", "m1_exponential_parameters_devalued.RDS"))

  ##### ED Model 2 ----------------------------------------------------------------------------------------------------------

  # convergence check
  m2_exp_check_baseline <- convergence_check(m2_exponential_fit_baseline,
    params = c("kE", "kR"),
    Rhat = TRUE, ess = TRUE,
    trace_plot = TRUE,
    rank_hist = FALSE
  )

  m2_exp_check_devalued <- convergence_check(m2_exponential_fit_devalued,
    params = c("kE", "kR"),
    Rhat = TRUE, ess = TRUE,
    trace_plot = TRUE,
    rank_hist = FALSE
  )

  # show trace plots for conditions
  m2_exp_check_baseline$trace_plot
  m2_exp_check_devalued$trace_plot

  # save results
  saveRDS(
    list(m2_exp_check_baseline$Rhat, m2_exp_check_baseline$ess),
    here::here("data", "experiment2", "modelling", "modelfit", "m2_exponential_check_baseline.RDS")
  )

  saveRDS(
    list(m2_exp_check_devalued$Rhat, m2_exp_check_devalued$ess),
    here::here("data", "experiment2", "modelling", "modelfit", "m2_exponential_check_devalued.RDS")
  )

  # LOO for model comparisons
  m2_exp_loo_baseline <- m2_exponential_fit_baseline$loo()
  saveRDS(m2_exp_loo_baseline, here::here("data", "experiment2", "modelling", "modelfit", "m2_exponential_loo_baseline.RDS"))

  m2_exp_loo_devalued <- m2_exponential_fit_devalued$loo()
  saveRDS(m2_exp_loo_devalued, here::here("data", "experiment2", "modelling", "modelfit", "m2_exponential_loo_devalued.RDS"))

  # parameter estimates
  m2_exp_params_baseline <- get_params(
    subj_id = unique(task_data$subj_id),
    model_fit = m2_exponential_fit_baseline,
    n_subj = length(unique(task_data$subj_id)),
    n_params = 2,
    param_names = c("kE", "kR")
  )

  m2_exp_params_devalued <- get_params(
    subj_id = unique(task_data$subj_id),
    model_fit = m2_exponential_fit_devalued,
    n_subj = length(unique(task_data$subj_id)),
    n_params = 2,
    param_names = c("kE", "kR")
  )

  # save results
  saveRDS(m2_exp_params_baseline, here::here("data", "experiment2", "modelling", "modelfit", "m2_exponential_parameters_baseline.RDS"))
  saveRDS(m2_exp_params_devalued, here::here("data", "experiment2", "modelling", "modelfit", "m2_exponential_parameters_devalued.RDS"))

  ##### ED Model 3 ----------------------------------------------------------------------------------------------------------

  # convergence check
  m3_exp_check_baseline <- convergence_check(m3_exponential_fit_baseline,
    params = c("kE", "kR", "a"),
    Rhat = TRUE, ess = TRUE,
    trace_plot = TRUE,
    rank_hist = FALSE
  )

  m3_exp_check_devalued <- convergence_check(m3_exponential_fit_devalued,
    params = c("kE", "kR", "a"),
    Rhat = TRUE, ess = TRUE,
    trace_plot = TRUE,
    rank_hist = FALSE
  )

  # show trace plots for conditions
  m3_exp_check_baseline$trace_plot
  m3_exp_check_devalued$trace_plot

  # save results
  saveRDS(
    list(m3_exp_check_baseline$Rhat, m3_exp_check_baseline$ess),
    here::here("data", "experiment2", "modelling", "modelfit", "m3_exponential_check_baseline.RDS")
  )

  saveRDS(
    list(m3_exp_check_devalued$Rhat, m3_exp_check_devalued$ess),
    here::here("data", "experiment2", "modelling", "modelfit", "m3_exponential_check_devalued.RDS")
  )

  # LOO for model comparisons
  m3_exp_loo_baseline <- m3_exponential_fit_baseline$loo()
  saveRDS(m3_exp_loo_baseline, here::here("data", "experiment2", "modelling", "modelfit", "m3_exponential_loo_baseline.RDS"))

  m3_exp_loo_devalued <- m3_exponential_fit_devalued$loo()
  saveRDS(m3_exp_loo_devalued, here::here("data", "experiment2", "modelling", "modelfit", "m3_exponential_loo_devalued.RDS"))

  # parameter estimates
  m3_exp_params_baseline <- get_params(
    subj_id = unique(task_data$subj_id),
    model_fit = m3_exponential_fit_baseline,
    n_subj = length(unique(task_data$subj_id)),
    n_params = 3,
    param_names = c("kE", "kR", "a")
  )

  m3_exp_params_devalued <- get_params(
    subj_id = unique(task_data$subj_id),
    model_fit = m3_exponential_fit_devalued,
    n_subj = length(unique(task_data$subj_id)),
    n_params = 3,
    param_names = c("kE", "kR", "a")
  )

  # save results
  saveRDS(m3_exp_params_baseline, here::here("data", "experiment2", "modelling", "modelfit", "m3_exponential_parameters_baseline.RDS"))
  saveRDS(m3_exp_params_devalued, here::here("data", "experiment2", "modelling", "modelfit", "m3_exponential_parameters_devalued.RDS"))

  ### Load Model Results ----------------------------------------------------------------------------------------------------

  m1_para_loo_baseline <- readRDS(here::here("data", "experiment2", "modelling", "modelfit", "m1_parabolic_loo_baseline.RDS"))
  m1_para_check_baseline <- readRDS(here::here("data", "experiment2", "modelling", "modelfit", "m1_parabolic_check_baseline.RDS"))
  m1_para_params_baseline <- readRDS(here::here("data", "experiment2", "modelling", "modelfit", "m1_parabolic_parameters_baseline.RDS"))

  m1_para_loo_devalued <- readRDS(here::here("data", "experiment2", "modelling", "modelfit", "m1_parabolic_loo_devalued.RDS"))
  m1_para_check_devalued <- readRDS(here::here("data", "experiment2", "modelling", "modelfit", "m1_parabolic_check_devalued.RDS"))
  m1_para_params_devalued <- readRDS(here::here("data", "experiment2", "modelling", "modelfit", "m1_parabolic_parameters_devalued.RDS"))

  m2_para_loo_baseline <- readRDS(here::here("data", "experiment2", "modelling", "modelfit", "m2_parabolic_loo_baseline.RDS"))
  m2_para_check_baseline <- readRDS(here::here("data", "experiment2", "modelling", "modelfit", "m2_parabolic_check_baseline.RDS"))
  m2_para_params_baseline <- readRDS(here::here("data", "experiment2", "modelling", "modelfit", "m2_parabolic_parameters_baseline.RDS"))

  m2_para_loo_devalued <- readRDS(here::here("data", "experiment2", "modelling", "modelfit", "m2_parabolic_loo_devalued.RDS"))
  m2_para_check_devalued <- readRDS(here::here("data", "experiment2", "modelling", "modelfit", "m2_parabolic_check_devalued.RDS"))
  m2_para_params_devalued <- readRDS(here::here("data", "experiment2", "modelling", "modelfit", "m2_parabolic_parameters_devalued.RDS"))

  m3_para_loo_baseline <- readRDS(here::here("data", "experiment2", "modelling", "modelfit", "m3_parabolic_loo_baseline.RDS"))
  m3_para_check_baseline <- readRDS(here::here("data", "experiment2", "modelling", "modelfit", "m3_parabolic_check_baseline.RDS"))
  m3_para_params_baseline <- readRDS(here::here("data", "experiment2", "modelling", "modelfit", "m3_parabolic_parameters_baseline.RDS"))

  m3_para_loo_devalued <- readRDS(here::here("data", "experiment2", "modelling", "modelfit", "m3_parabolic_loo_devalued.RDS"))
  m3_para_check_devalued <- readRDS(here::here("data", "experiment2", "modelling", "modelfit", "m3_parabolic_check_devalued.RDS"))
  m3_para_params_devalued <- readRDS(here::here("data", "experiment2", "modelling", "modelfit", "m3_parabolic_parameters_devalued.RDS"))

  m1_lin_loo_baseline <- readRDS(here::here("data", "experiment2", "modelling", "modelfit", "m1_linear_loo_baseline.RDS"))
  m1_lin_check_baseline <- readRDS(here::here("data", "experiment2", "modelling", "modelfit", "m1_linear_check_baseline.RDS"))
  m1_lin_params_baseline <- readRDS(here::here("data", "experiment2", "modelling", "modelfit", "m1_linear_parameters_baseline.RDS"))

  m1_lin_loo_devalued <- readRDS(here::here("data", "experiment2", "modelling", "modelfit", "m1_linear_loo_devalued.RDS"))
  m1_lin_check_devalued <- readRDS(here::here("data", "experiment2", "modelling", "modelfit", "m1_linear_check_devalued.RDS"))
  m1_lin_params_devalued <- readRDS(here::here("data", "experiment2", "modelling", "modelfit", "m1_linear_parameters_devalued.RDS"))

  m2_lin_loo_baseline <- readRDS(here::here("data", "experiment2", "modelling", "modelfit", "m2_linear_loo_baseline.RDS"))
  m2_lin_check_baseline <- readRDS(here::here("data", "experiment2", "modelling", "modelfit", "m2_linear_check_baseline.RDS"))
  m2_lin_params_baseline <- readRDS(here::here("data", "experiment2", "modelling", "modelfit", "m2_linear_parameters_baseline.RDS"))

  m2_lin_loo_devalued <- readRDS(here::here("data", "experiment2", "modelling", "modelfit", "m2_linear_loo_devalued.RDS"))
  m2_lin_check_devalued <- readRDS(here::here("data", "experiment2", "modelling", "modelfit", "m2_linear_check_devalued.RDS"))
  m2_lin_params_devalued <- readRDS(here::here("data", "experiment2", "modelling", "modelfit", "m2_linear_parameters_devalued.RDS"))

  m3_lin_loo_baseline <- readRDS(here::here("data", "experiment2", "modelling", "modelfit", "m3_linear_loo_baseline.RDS"))
  m3_lin_check_baseline <- readRDS(here::here("data", "experiment2", "modelling", "modelfit", "m3_linear_check_baseline.RDS"))
  m3_lin_params_baseline <- readRDS(here::here("data", "experiment2", "modelling", "modelfit", "m3_linear_parameters_baseline.RDS"))

  m3_lin_loo_devalued <- readRDS(here::here("data", "experiment2", "modelling", "modelfit", "m3_linear_loo_devalued.RDS"))
  m3_lin_check_devalued <- readRDS(here::here("data", "experiment2", "modelling", "modelfit", "m3_linear_check_devalued.RDS"))
  m3_lin_params_devalued <- readRDS(here::here("data", "experiment2", "modelling", "modelfit", "m3_linear_parameters_devalued.RDS"))

  m1_exp_loo_baseline <- readRDS(here::here("data", "experiment2", "modelling", "modelfit", "m1_exponential_loo_baseline.RDS"))
  m1_exp_check_baseline <- readRDS(here::here("data", "experiment2", "modelling", "modelfit", "m1_exponential_check_baseline.RDS"))
  m1_exp_params_baseline <- readRDS(here::here("data", "experiment2", "modelling", "modelfit", "m1_exponential_parameters_baseline.RDS"))

  m1_exp_loo_devalued <- readRDS(here::here("data", "experiment2", "modelling", "modelfit", "m1_exponential_loo_devalued.RDS"))
  m1_exp_check_devalued <- readRDS(here::here("data", "experiment2", "modelling", "modelfit", "m1_exponential_check_devalued.RDS"))
  m1_exp_params_devalued <- readRDS(here::here("data", "experiment2", "modelling", "modelfit", "m1_exponential_parameters_devalued.RDS"))

  m2_exp_loo_baseline <- readRDS(here::here("data", "experiment2", "modelling", "modelfit", "m2_exponential_loo_baseline.RDS"))
  m2_exp_check_baseline <- readRDS(here::here("data", "experiment2", "modelling", "modelfit", "m2_exponential_check_baseline.RDS"))
  m2_exp_params_baseline <- readRDS(here::here("data", "experiment2", "modelling", "modelfit", "m2_exponential_parameters_baseline.RDS"))

  m2_exp_loo_devalued <- readRDS(here::here("data", "experiment2", "modelling", "modelfit", "m2_exponential_loo_devalued.RDS"))
  m2_exp_check_devalued <- readRDS(here::here("data", "experiment2", "modelling", "modelfit", "m2_exponential_check_devalued.RDS"))
  m2_exp_params_devalued <- readRDS(here::here("data", "experiment2", "modelling", "modelfit", "m2_exponential_parameters_devalued.RDS"))

  m3_exp_loo_baseline <- readRDS(here::here("data", "experiment2", "modelling", "modelfit", "m3_exponential_loo_baseline.RDS"))
  m3_exp_check_baseline <- readRDS(here::here("data", "experiment2", "modelling", "modelfit", "m3_exponential_check_baseline.RDS"))
  m3_exp_params_baseline <- readRDS(here::here("data", "experiment2", "modelling", "modelfit", "m3_exponential_parameters_baseline.RDS"))

  m3_exp_loo_devalued <- readRDS(here::here("data", "experiment2", "modelling", "modelfit", "m3_exponential_loo_devalued.RDS"))
  m3_exp_check_devalued <- readRDS(here::here("data", "experiment2", "modelling", "modelfit", "m3_exponential_check_devalued.RDS"))
  m3_exp_params_devalued <- readRDS(here::here("data", "experiment2", "modelling", "modelfit", "m3_exponential_parameters_devalued.RDS"))
}

## CONVERGENCE CHECKS =======================================================================================================

# Rhats most extreme value (neg. or pos.) across all models
rhat_values <- c(
  m1_parabolic_baseline$convergence$Rhat, m1_parabolic_devalued$convergence$Rhat,
  m2_parabolic_baseline$convergence$Rhat, m2_parabolic_devalued$convergence$Rhat,
  m3_parabolic_baseline$convergence$Rhat, m3_parabolic_devalued$convergence$Rhat,
  m1_linear_baseline$convergence$Rhat, m1_linear_devalued$convergence$Rhat,
  m2_linear_baseline$convergence$Rhat, m2_linear_devalued$convergence$Rhat,
  m3_linear_baseline$convergence$Rhat, m3_linear_devalued$convergence$Rhat,
  m1_exponential_baseline$convergence$Rhat, m1_exponential_devalued$convergence$Rhat,
  m2_exponential_baseline$convergence$Rhat, m2_exponential_devalued$convergence$Rhat,
  m3_exponential_baseline$convergence$Rhat, m3_exponential_devalued$convergence$Rhat
)

rhat_values[which.max(abs(rhat_values))] # max = 1.000697

# ESS minimum across all models (= 20817.494)
min(
  m1_parabolic_baseline$convergence$ess, m1_parabolic_devalued$convergence$ess,
  m2_parabolic_baseline$convergence$ess, m2_parabolic_devalued$convergence$ess,
  m3_parabolic_baseline$convergence$ess, m3_parabolic_devalued$convergence$ess,
  m1_linear_baseline$convergence$ess, m1_linear_devalued$convergence$ess,
  m2_linear_baseline$convergence$ess, m2_linear_devalued$convergence$ess,
  m3_linear_baseline$convergence$ess, m3_linear_devalued$convergence$ess,
  m1_exponential_baseline$convergence$ess, m1_exponential_devalued$convergence$ess,
  m2_exponential_baseline$convergence$ess, m2_exponential_devalued$convergence$ess,
  m3_exponential_baseline$convergence$ess, m3_exponential_devalued$convergence$ess
) |> sprintf("%.3f", .)

## MODEL COMPARISON =========================================================================================================

comparison_baseline <- model_comparison(
  loo_paths = list.files(
    path = here::here("data", "experiment2", "modelling", "modelfit"),
    pattern = "loo_baseline.RDS", full.names = TRUE
  ),
  model_names = c(
    "Exponential 1", "Linear 1", "Parabolic 1",
    "Exponential 2", "Linear 2", "Parabolic 2",
    "Exponential 3", "Linear 3", "Parabolic 3"
  ), LOO_plot = TRUE
)

comparison_baseline$LOO_plot <- comparison_baseline$LOO_plot +
  scale_x_discrete(
    labels = c(
      expression("Exponential" ~ beta[E] ~ alpha),
      expression("Exponential" ~ beta[E] ~ beta[R]),
      expression("Exponential" ~ beta[E] ~ beta[R] ~ alpha),
      expression("Linear" ~ beta[E] ~ alpha),
      expression("Linear" ~ beta[E] ~ beta[R]),
      expression("Linear" ~ beta[E] ~ beta[R] ~ alpha),
      expression("Parabolic" ~ beta[E] ~ alpha),
      expression("Parabolic" ~ beta[E] ~ beta[R]),
      expression("Parabolic" ~ beta[E] ~ beta[R] ~ alpha)
    )
  )

ggplot2::ggsave(comparison_baseline$LOO_plot,
  filename = "model_comparison_baseline.png",
  dpi = 800, device = "png", path = here::here("output", "figures", "experiment2", "modelfit"), bg = "white"
)

comparison_devalued <- model_comparison(
  loo_paths = list.files(
    path = here::here("data", "experiment2", "modelling", "modelfit"),
    pattern = "loo_devalued.RDS", full.names = TRUE
  ),
  model_names = c(
    "Exponential 1", "Linear 1", "Parabolic 1",
    "Exponential 2", "Linear 2", "Parabolic 2",
    "Exponential 3", "Linear 3", "Parabolic 3"
  ), LOO_plot = TRUE
)

comparison_devalued$LOO_plot <- comparison_devalued$LOO_plot +
  scale_x_discrete(
    labels = c(
      expression("Exponential" ~ beta[E] ~ alpha),
      expression("Exponential" ~ beta[E] ~ beta[R]),
      expression("Exponential" ~ beta[E] ~ beta[R] ~ alpha),
      expression("Linear" ~ beta[E] ~ alpha),
      expression("Linear" ~ beta[E] ~ beta[R]),
      expression("Linear" ~ beta[E] ~ beta[R] ~ alpha),
      expression("Parabolic" ~ beta[E] ~ alpha),
      expression("Parabolic" ~ beta[E] ~ beta[R]),
      expression("Parabolic" ~ beta[E] ~ beta[R] ~ alpha)
    )
  )

ggplot2::ggsave(comparison_devalued$LOO_plot,
  filename = "model_comparison_devalued.png",
  dpi = 800, device = "png", path = here::here("output", "figures", "experiment2", "modelfit", bg = "white")
)

ggpubr::ggarrange(
  comparison_baseline$LOO_plot,
  comparison_devalued$LOO_plot,
  ncol = 2, nrow = 1,
  common.legend = TRUE, legend = "bottom",
  labels = c("A", "B")
) |>
  ggplot2::ggsave(
    filename = "model_comparison_both.png",
    width = 12, height = 4,
    dpi = 800, device = "png", path = here::here("output", "figures", "experiment2", "modelfit"), bg = "white"
  )

## POSTERIOR PREDICTIVE CHECKS (winning model only) =========================================================================

names(data)[names(data) == "game"] <- "task_data"
data$task_data$session_id <- data$task_data$task_condition
data_cleaned <- modeling_cleandata(data$task_data)

task_data_baseline <- data_cleaned$baseline
task_data_devalued <- data_cleaned$devalued

# extract posterior predictives for baseline
ppc_baseline <- posterior_predictions(
  csv_paths = m3_parabolic_fit_baseline$output_files(),
  n_chains = 4,
  n_iter = 14 * 10000,
  n_subj = length(unique(data$task_data$subj_id)),
  n_trials = 32,
  real_dat = task_data_baseline
)

# extract posterior predictives for devalued
ppc_devalued <- posterior_predictions(
  csv_paths = m3_parabolic_fit_devalued$output_files(),
  n_chains = 4,
  n_iter = 14 * 10000,
  n_subj = length(unique(data$task_data$subj_id)),
  n_trials = 32,
  real_dat = task_data_devalued
)

saveRDS(ppc_baseline, here::here("data", "experiment2", "modelling", "m3_parabolic_ppc_baseline.RDS"))
saveRDS(ppc_devalued, here::here("data", "experiment2", "modelling", "m3_parabolic_ppc_devalued.RDS"))

m3_para_ppc_baseline <- readRDS(here::here("data", "experiment2", "modelling", "m3_parabolic_ppc_baseline.RDS"))
m3_para_ppc_devalued <- readRDS(here::here("data", "experiment2", "modelling", "m3_parabolic_ppc_devalued.RDS"))

#### Plot posterior predictives ---------------------------------------------------------------------------------------------

# change names due to changed jsPsych script
list(ppc_baseline, ppc_devalued) |>
  map(~ {
    .x$posterior_predictions_trial_type %<>% rename(offerEffort = effort_a, offerReward = amount_a)
    .x$posterior_predictions_effort %<>% rename(offerEffort = effort_a)
    .x$posterior_predictions_reward %<>% rename(offerReward = amount_a)
    .x
  }) |>
  list2env(envir = .GlobalEnv, x = setNames(., c("m3_para_ppc_baseline", "m3_para_ppc_devalued")))

ppc_plot_baseline <- ppc_plots(m3_para_ppc_baseline, indiv_plot_title = "", group_plot_title = "", show_legend = FALSE)
ppc_plot_devalued <- ppc_plots(m3_para_ppc_devalued, indiv_plot_title = "", group_plot_title = "", show_legend = FALSE)

# save arranged plot with AB panel for supplement
ggpubr::ggarrange(
  ppc_plot_baseline$LOO_plot,
  ppc_plot_devalued$LOO_plot,
  ncol = 2, nrow = 1,
  common.legend = TRUE, legend = "bottom",
  labels = c("A", "B")
) |>
  ggplot2::ggsave(
    filename = "model_comparison_both.png",
    width = 12, height = 4, bg = "white",
    dpi = 800, device = "png", path = here::here("output", "figures", "experiment2", "modelfit")
  )

# save posterior predictive plots (individual estimates)
ggplot2::ggsave(ppc_plot_baseline$indiv_plot,
  filename = "ppc_plot_baseline.png", bg = "white",
  dpi = 800, device = "png", path = here::here("output", "figures", "experiment2", "modelfit")
)
ggplot2::ggsave(ppc_plot_devalued$indiv_plot,
  filename = "ppc_plot_devalued.png", bg = "white",
  dpi = 800, device = "png", path = here::here("output", "figures", "experiment2", "modelfit")
)

# save posterior predictive plots (grouped estimates)
ggplot2::ggsave(ppc_plot_baseline$group_plot,
  filename = "ppc_plot_group_baseline.png", bg = "white",
  dpi = 800, device = "png", path = here::here("output", "figures", "experiment2", "modelfit")
)
ggplot2::ggsave(ppc_plot_devalued$group_plot,
  filename = "ppc_plot_group_devalued.png", bg = "white",
  dpi = 800, device = "png", path = here::here("output", "figures", "experiment2", "modelfit")
)

# save grouped plots
ggpubr::ggarrange(
  ppc_plot_baseline$group_plot + theme(
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "none"
  ),
  ppc_plot_devalued$group_plot + theme(
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "none"
  ),
  ncol = 2, nrow = 1,
  common.legend = FALSE,
  labels = c("A", "B"),
  vjust = 3
) |>
  ggplot2::ggsave(
    filename = "ppc_group_both.png",
    width = 12, height = 4,
    dpi = 800, device = "png", path = here::here("output", "figures", "experiment2", "modelfit")
  )

# get legend
dummy <- ggplot2::ggplot(
  data.frame(x = 1:4, y = 1:4, level = factor(1:4)),
  ggplot2::aes(x = x, y = y, color = level)
) +
  ggplot2::geom_point() +
  ggplot2::scale_color_manual(
    values = c("#febb81", "#f8765c", "#982d80", "#5f187f"),
    labels = 1:4
  ) +
  ggplot2::guides(color = ggplot2::guide_legend(
    title = "Effort/Reward level",
    direction = "horizontal"
  )) +
  theme_bw()

ppc_leg <- cowplot::get_legend(dummy)

# Arrange plots with shared legend
ggpubr::ggarrange(
  ppc_plot_baseline$indiv_plot + theme(
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "none"
  ),
  ppc_plot_devalued$indiv_plot + theme(
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "none"
  ),
  ncol = 2, nrow = 1,
  legend.grob = ppc_leg,
  legend = "bottom",
  labels = c("A", "B"),
  vjust = 3
) |>
  ggplot2::ggsave(
    filename = "ppc_indiv_both.png",
    width = 12, height = 4, bg = "white",
    dpi = 800, device = "png", path = here::here("output", "figures", "experiment2", "modelfit")
  )

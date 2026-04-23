## ======================================================================================================================= ##
## Script:    Model fitting (winning model only)
## ======================================================================================================================= ##
## Authors:   Lukas J. Gunschera
## Date:      Mon Nov 17 15:06:04 2025
## ======================================================================================================================= ##
##
## ======================================================================================================================= ##

## SETUP ====================================================================================================================

# set parameters and seed
source(here::here("code", "00_setup.R"))

### Set Parameters-----------------------------------------------------------------------------------------------------------

fit_models <- FALSE # TRUE = model(s) are fitted, FALSE = model(s) are loaded from file
models_to_run <- c("m3_parabolic") # character vector specifying the models to run

# model space: "ed_m1_parabolic.stan", "ed_m2_parabolic.stan", "ed_m3_parabolic.stan"
# model space: "ed_m1_linear.stan", "ed_m2_linear.stan", "ed_m3_linear.stan"
# model space: "ed_m1_exponential.stan", "ed_m2_exponential.stan", "ed_m3_exponential.stan"

# load dpendencies
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

## FIT OR LOAD MODELS =======================================================================================================

### Fit Models --------------------------------------------------------------------------------------------------------------

if (fit_models) {
  fit_stan_models(models_to_run)

  ### Load Model Results ------------------------------------------------------------------------------------------------------
} else {
  if ("m1_parabolic" %in% models_to_run) {
    m1_parabolic_devalued <- readRDS(here::here(
      "data", "experiment2", "modelling", "m1_parabolic_devalued_results.RDS"
    ))
    m1_parabolic_baseline <- readRDS(here::here(
      "data", "experiment2", "modelling", "m1_parabolic_baseline_results.RDS"
    ))
  }

  if ("m2_parabolic" %in% models_to_run) {
    m2_parabolic_devalued <- readRDS(here::here(
      "data", "experiment2", "modelling", "m2_parabolic_devalued_results.RDS"
    ))
    m2_parabolic_baseline <- readRDS(here::here(
      "data", "experiment2", "modelling", "m2_parabolic_baseline_results.RDS"
    ))
  }

  if ("m3_parabolic" %in% models_to_run) {
    m3_parabolic_devalued <- readRDS(here::here(
      "data", "experiment2", "modelling", "m3_parabolic_devalued_results.RDS"
    ))
    m3_parabolic_baseline <- readRDS(here::here(
      "data", "experiment2", "modelling", "m3_parabolic_baseline_results.RDS"
    ))
  }

  if ("m1_linear" %in% models_to_run) {
    m1_linear_devalued <- readRDS(here::here(
      "data", "experiment2", "modelling", "m1_linear_devalued_results.RDS"
    ))
    m1_linear_baseline <- readRDS(here::here(
      "data", "experiment2", "modelling", "m1_linear_baseline_results.RDS"
    ))
  }

  if ("m2_linear" %in% models_to_run) {
    m2_linear_devalued <- readRDS(here::here(
      "data", "experiment2", "modelling", "m2_linear_devalued_results.RDS"
    ))
    m2_linear_baseline <- readRDS(here::here(
      "data", "experiment2", "modelling", "m2_linear_baseline_results.RDS"
    ))
  }

  if ("m3_linear" %in% models_to_run) {
    m3_linear_devalued <- readRDS(here::here(
      "data", "experiment2", "modelling", "m3_linear_devalued_results.RDS"
    ))
    m3_linear_baseline <- readRDS(here::here(
      "data", "experiment2", "modelling", "m3_linear_baseline_results.RDS"
    ))
  }

  if ("m1_exponential" %in% models_to_run) {
    m1_exponential_devalued <- readRDS(here::here(
      "data", "experiment2", "modelling", "m1_exponential_devalued_results.RDS"
    ))
    m1_exponential_baseline <- readRDS(here::here(
      "data", "experiment2", "modelling", "m1_exponential_baseline_results.RDS"
    ))
  }

  if ("m2_exponential" %in% models_to_run) {
    m2_exponential_devalued <- readRDS(here::here(
      "data", "experiment2", "modelling", "m2_exponential_devalued_results.RDS"
    ))
    m2_exponential_baseline <- readRDS(here::here(
      "data", "experiment2", "modelling", "m2_exponential_baseline_results.RDS"
    ))
  }

  if ("m3_exponential" %in% models_to_run) {
    m3_exponential_devalued <- readRDS(here::here(
      "data", "experiment2", "modelling", "m3_exponential_devalued_results.RDS"
    ))
    m3_exponential_baseline <- readRDS(here::here(
      "data", "experiment2", "modelling", "m3_exponential_baseline_results.RDS"
    ))
  }
}

## PROCESS DATA =============================================================================================================

# format model parameters for comparison
m3_parabolic_parameters <- dplyr::left_join(
  m3_parabolic_devalued$parameters$individual_params,
  m3_parabolic_baseline$parameters$individual_params,
  by = "subj_id",
  suffix = c("_devalued", "_baseline")
)

# remove estimate from colnames for conciseness
colnames(m3_parabolic_parameters) <- gsub(
  "estimate_",
  "",
  colnames(m3_parabolic_parameters)
)

# reformat data to add parameters to the participants' data (TODO: FIX FOR NEW FORMAT)
data_params <- data$demographics |>
  dplyr::left_join(m3_parabolic_parameters, by = "subj_id") |>
  dplyr::mutate(
    devaluation_sens = a_baseline - a_devalued,
    wl_baseline = a_baseline - liking_b,
    wl_devalued = a_devalued - liking_d
  )

### Save Data ---------------------------------------------------------------------------------------------------------------
saveRDS(data_params, here::here("data", "experiment2", "modelling", "parameters", "m3_para_data.RDS"))

## Level 1 ===========================================================================================================

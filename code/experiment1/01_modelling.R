## ======================================================================================================================= ##
## Script:       MODELLING
## ======================================================================================================================= ##
## Authors:      Lukas Gunschera
## Contact:      l.gunschera@outlook.com
##
## Date created: 2025-02-10
## ======================================================================================================================= ##
##
## ======================================================================================================================= ##

## SETUP ====================================================================================================================

# setup document and seed
source(here::here("code", "00_setup.R"))

library(renv) # load renv package
renv::restore() # restore the environment

### Set Parameters-----------------------------------------------------------------------------------------------------------

fit_models <- TRUE # TRUE = model(s) are fitted, FALSE = model(s) are loaded from file
models_to_run <- c("m3_parabolic") # winning model selected, full model space examined in 02_modelvalidation

# model space: "ed_m1_parabolic.stan", "ed_m2_parabolic.stan", "ed_m3_parabolic.stan"
# model space: "ed_m1_linear.stan", "ed_m2_linear.stan", "ed_m3_linear.stan"
# model space: "ed_m1_exponential.stan", "ed_m2_exponential.stan", "ed_m3_exponential.stan"

# load dependencies
pacman::p_load(zoo, plm, ragg, here, purrr, dplyr, rstatix, janitor, ggplot2, cmdstanr, magrittr, hBayesDM, ggpattern, bayesplot, lubridate, BayesFactor)

here::i_am("renv.lock")

source(here::here("code", "experiment1", "functions", "fun_plots.R"))
source(here::here("code", "experiment1", "functions", "fun_helper.R"))
source(here::here("code", "experiment1", "functions", "fun_fit_models.R"))
source(here::here("code", "experiment1", "functions", "fun_model_preprocess.R"))
source(here::here("code", "experiment1", "functions", "fun_model_comparison.R"))
source(here::here("code", "experiment1", "functions", "fun_modeling_cleandata.R"))
source(here::here("code", "experiment1", "functions", "fun_parameter_estimates.R"))
source(here::here("code", "experiment1", "functions", "fun_posterior_predictions.R"))
source(here::here("code", "experiment1", "functions", "fun_model_convergence_check.R"))

# load data from file (preprocessed data)
data <- readRDS(here::here("data", "experiment1", "processed", "data.RDS"))

## FIT OR LOAD MODELS =======================================================================================================

### Fit Models --------------------------------------------------------------------------------------------------------------

if (fit_models) {
  fit_stan_models(models_to_run)

  ### Load Model Results ------------------------------------------------------------------------------------------------------
} else {
  if (parabolic_m1) {
    m1_parabolic_tiktok <- readRDS(here::here(
      "data",
      "experiment1",
      "modelling",
      "m1_parabolic_tiktok_results.RDS"
    ))
    m1_parabolic_netflix <- readRDS(here::here(
      "data",
      "experiment1",
      "modelling",
      "m1_parabolic_netflix_results.RDS"
    ))
  }

  if (parabolic_m2) {
    m2_parabolic_tiktok <- readRDS(here::here(
      "data",
      "experiment1",
      "modelling",
      "m2_parabolic_tiktok_results.RDS"
    ))
    m2_parabolic_netflix <- readRDS(here::here(
      "data",
      "experiment1",
      "modelling",
      "m2_parabolic_netflix_results.RDS"
    ))
  }

  if (parabolic_m3) {
    m3_parabolic_tiktok <- readRDS(here::here(
      "data",
      "experiment1",
      "modelling",
      "m3_parabolic_tiktok_results.RDS"
    ))
    m3_parabolic_netflix <- readRDS(here::here(
      "data",
      "experiment1",
      "modelling",
      "m3_parabolic_netflix_results.RDS"
    ))
  }

  if (linear_m1) {
    m1_linear_tiktok <- readRDS(here::here(
      "data",
      "experiment1",
      "modelling",
      "m1_linear_tiktok_results.RDS"
    ))
    m1_linear_netflix <- readRDS(here::here(
      "data",
      "experiment1",
      "modelling",
      "m1_linear_netflix_results.RDS"
    ))
  }

  if (linear_m2) {
    m2_linear_tiktok <- readRDS(here::here(
      "data",
      "experiment1",
      "modelling",
      "m2_linear_tiktok_results.RDS"
    ))
    m2_linear_netflix <- readRDS(here::here(
      "data",
      "experiment1",
      "modelling",
      "m2_linear_netflix_results.RDS"
    ))
  }

  if (linear_m3) {
    m3_linear_tiktok <- readRDS(here::here(
      "data",
      "experiment1",
      "modelling",
      "m3_linear_tiktok_results.RDS"
    ))
    m3_linear_netflix <- readRDS(here::here(
      "data",
      "experiment1",
      "modelling",
      "m3_linear_netflix_results.RDS"
    ))
  }

  if (exponential_m1) {
    m1_exponential_tiktok <- readRDS(here::here(
      "data",
      "experiment1",
      "modelling",
      "m1_exponential_tiktok_results.RDS"
    ))
    m1_exponential_netflix <- readRDS(here::here(
      "data",
      "experiment1",
      "modelling",
      "m1_exponential_netflix_results.RDS"
    ))
  }

  if (exponential_m2) {
    m2_exponential_tiktok <- readRDS(here::here(
      "data",
      "experiment1",
      "modelling",
      "m2_exponential_tiktok_results.RDS"
    ))
    m2_exponential_netflix <- readRDS(here::here(
      "data",
      "experiment1",
      "modelling",
      "m2_exponential_netflix_results.RDS"
    ))
  }

  if (exponential_m3) {
    m3_exponential_tiktok <- readRDS(here::here(
      "data",
      "experiment1",
      "modelling",
      "m3_exponential_tiktok_results.RDS"
    ))
    m3_exponential_netflix <- readRDS(here::here(
      "data",
      "experiment1",
      "modelling",
      "m3_exponential_netflix_results.RDS"
    ))
  }
}

## PROCESS DATA =============================================================================================================

# format model parameters for comparison
m3_parabolic_parameters <- dplyr::left_join(
  m3_parabolic_tiktok$parameters$individual_params,
  m3_parabolic_netflix$parameters$individual_params,
  by = "subj_id",
  suffix = c("_tiktok", "_netflix")
)

# remove estimate from colnames for conciseness
colnames(m3_parabolic_parameters) <- gsub(
  "estimate_",
  "",
  colnames(m3_parabolic_parameters)
)

# reformat data to add parameters to the participants' data
data_params <- data$demographics |>
  dplyr::left_join(data$questionnaires, by = "subj_id") |>
  dplyr::left_join(m3_parabolic_parameters, by = "subj_id") |>
  dplyr::mutate(
    wl_netflix = a_netflix - strap_netflix_avg,
    wl_tiktok = a_tiktok - strap_tiktok_avg
  )

### Save Data ---------------------------------------------------------------------------------------------------------------
saveRDS(data_params, here::here("data", "experiment1", "modelling", "parameters", "m3_para_data.RDS"))

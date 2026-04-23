## ======================================================================================================================= ##
## Script:       MODELCOMPARISON
## ======================================================================================================================= ##
## Authors:      Lukas Gunschera, Sara Mehrhof
## Contact:      l.gunschera@outlook.com
##
## Date created: 2025-02-10
## ======================================================================================================================= ##
##
## ======================================================================================================================= ##

## SETUP ====================================================================================================================

source(here::here("code", "00_setup.R"))

### Set Parameters-----------------------------------------------------------------------------------------------------------

fit_models <- TRUE # TRUE = model(s) are fitted, FALSE = model(s) are loaded from file
models_to_run <- c(
  "m1_parabolic", "m2_parabolic", "m3_parabolic",
  "m1_linear", "m2_linear", "m3_linear",
  "m1_exponential", "m2_exponential", "m3_exponential"
)

parabolic_m1 <- TRUE
parabolic_m2 <- TRUE
parabolic_m3 <- TRUE
linear_m1 <- TRUE
linear_m2 <- TRUE
linear_m3 <- TRUE
exponential_m1 <- TRUE
exponential_m2 <- TRUE
exponential_m3 <- TRUE

# Model 1 includes parameters kE and a
# Model 2 includes parameters kE and kR
# Model 3 includes parameters kE, kR, and a

set.seed(777) # reproduce random number generation
renv::restore() # recreate computing environment

# load dependencies
pacman::p_load(ragg, here, purrr, dplyr, janitor, ggplot2, cmdstanr, magrittr, hBayesDM, bayesplot, lubridate, BayesFactor)

here::i_am("renv.lock") # set here to the renv.lock file for correct pathing

source(here::here("code", "experiment1", "functions", "fun_plots.R"))
source(here::here("code", "experiment1", "functions", "fun_helper.R"))
source(here::here("code", "experiment1", "functions", "fun_fit_models.R"))
source(here::here("code", "experiment1", "functions", "fun_model_preprocess.R"))
source(here::here("code", "experiment1", "functions", "fun_model_comparison.R"))
source(here::here("code", "experiment1", "functions", "fun_modeling_cleandata.R"))
source(here::here("code", "experiment1", "functions", "fun_parameter_estimates.R"))
source(here::here("code", "experiment1", "functions", "fun_posterior_predictions.R"))
source(here::here("code", "experiment1", "functions", "fun_model_convergence_check.R"))

# load data from file (data is used in the modeling.R script)
data <- readRDS(here::here("data", "experiment1", "processed", "data.RDS"))

## FIT OR LOAD MODELS =======================================================================================================

### A: Fit Models -----------------------------------------------------------------------------------------------------------

if (fit_models) {
  fit_stan_models(models_to_run)

  ### B: Load Models ----------------------------------------------------------------------------------------------------------
} else {
  if (parabolic_m1) {
    m1_parabolic_tiktok <- readRDS(here::here("data", "experiment1", "modelling", "m1_parabolic_tiktok_results.RDS"))
    m1_parabolic_netflix <- readRDS(here::here("data", "experiment1", "modelling", "m1_parabolic_netflix_results.RDS"))
  }

  if (parabolic_m2) {
    m2_parabolic_tiktok <- readRDS(here::here("data", "experiment1", "modelling", "m2_parabolic_tiktok_results.RDS"))
    m2_parabolic_netflix <- readRDS(here::here("data", "experiment1", "modelling", "m2_parabolic_netflix_results.RDS"))
  }

  if (parabolic_m3) {
    m3_parabolic_tiktok <- readRDS(here::here("data", "experiment1", "modelling", "m3_parabolic_tiktok_results.RDS"))
    m3_parabolic_netflix <- readRDS(here::here("data", "experiment1", "modelling", "m3_parabolic_netflix_results.RDS"))
  }

  if (linear_m1) {
    m1_linear_tiktok <- readRDS(here::here("data", "experiment1", "modelling", "m1_linear_tiktok_results.RDS"))
    m1_linear_netflix <- readRDS(here::here("data", "experiment1", "modelling", "m1_linear_netflix_results.RDS"))
  }

  if (linear_m2) {
    m2_linear_tiktok <- readRDS(here::here("data", "experiment1", "modelling", "m2_linear_tiktok_results.RDS"))
    m2_linear_netflix <- readRDS(here::here("data", "experiment1", "modelling", "m2_linear_netflix_results.RDS"))
  }

  if (linear_m3) {
    m3_linear_tiktok <- readRDS(here::here("data", "experiment1", "modelling", "m3_linear_tiktok_results.RDS"))
    m3_linear_netflix <- readRDS(here::here("data", "experiment1", "modelling", "m3_linear_netflix_results.RDS"))
  }

  if (exponential_m1) {
    m1_exponential_tiktok <- readRDS(here::here("data", "experiment1", "modelling", "m1_exponential_tiktok_results.RDS"))
    m1_exponential_netflix <- readRDS(here::here("data", "experiment1", "modelling", "m1_exponential_netflix_results.RDS"))
  }

  if (exponential_m2) {
    m2_exponential_tiktok <- readRDS(here::here("data", "experiment1", "modelling", "m2_exponential_tiktok_results.RDS"))
    m2_exponential_netflix <- readRDS(here::here("data", "experiment1", "modelling", "m2_exponential_netflix_results.RDS"))
  }

  if (exponential_m3) {
    m3_exponential_tiktok <- readRDS(here::here("data", "experiment1", "modelling", "m3_exponential_tiktok_results.RDS"))
    m3_exponential_netflix <- readRDS(here::here("data", "experiment1", "modelling", "m3_exponential_netflix_results.RDS"))
  }
}

## CONVERGENCE CHECKS =======================================================================================================

# Rhats most extreme value (neg. or pos.) across all models
rhat_values <- c(
  m1_parabolic_tiktok$convergence$Rhat, m1_parabolic_netflix$convergence$Rhat,
  m2_parabolic_tiktok$convergence$Rhat, m2_parabolic_netflix$convergence$Rhat,
  m3_parabolic_tiktok$convergence$Rhat, m3_parabolic_netflix$convergence$Rhat,
  m1_linear_tiktok$convergence$Rhat, m1_linear_netflix$convergence$Rhat,
  m2_linear_tiktok$convergence$Rhat, m2_linear_netflix$convergence$Rhat,
  m3_linear_tiktok$convergence$Rhat, m3_linear_netflix$convergence$Rhat,
  m1_exponential_tiktok$convergence$Rhat, m1_exponential_netflix$convergence$Rhat,
  m2_exponential_tiktok$convergence$Rhat, m2_exponential_netflix$convergence$Rhat,
  m3_exponential_tiktok$convergence$Rhat, m3_exponential_netflix$convergence$Rhat
)

rhat_values[which.max(abs(rhat_values))]

# ESS minimum across all models
min(
  m1_parabolic_tiktok$convergence$ess, m1_parabolic_netflix$convergence$ess,
  m2_parabolic_tiktok$convergence$ess, m2_parabolic_netflix$convergence$ess,
  m3_parabolic_tiktok$convergence$ess, m3_parabolic_netflix$convergence$ess,
  m1_linear_tiktok$convergence$ess, m1_linear_netflix$convergence$ess,
  m2_linear_tiktok$convergence$ess, m2_linear_netflix$convergence$ess,
  m3_linear_tiktok$convergence$ess, m3_linear_netflix$convergence$ess,
  m1_exponential_tiktok$convergence$ess, m1_exponential_netflix$convergence$ess,
  m2_exponential_tiktok$convergence$ess, m2_exponential_netflix$convergence$ess,
  m3_exponential_tiktok$convergence$ess, m3_exponential_netflix$convergence$ess
)

## MODEL COMPARISON =========================================================================================================

comparison_tiktok <- model_comparison(
  loo_paths = list.files(
    path = here::here("data", "experiment1", "modelling", "modelfit"),
    pattern = "tiktok_loo.RDS", full.names = TRUE
  ),
  model_names = c(
    "Exponential 1", "Linear 1", "Parabolic 1",
    "Exponential 2", "Linear 2", "Parabolic 2",
    "Exponential 3", "Linear 3", "Parabolic 3"
  ), LOO_plot = TRUE
)

comparison_tiktok$LOO_plot <- comparison_tiktok$LOO_plot +
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

ggplot2::ggsave(comparison_tiktok$LOO_plot,
  filename = "model_comparison_tiktok.png",
  dpi = 1000, device = "png", path = here::here("output", "figures")
)


comparison_netflix <- model_comparison(
  loo_paths = list.files(
    path = here::here("data", "experiment1", "modelling", "modelfit"),
    pattern = "netflix_loo.RDS", full.names = TRUE
  ),
  model_names = c(
    "Exponential 1", "Linear 1", "Parabolic 1",
    "Exponential 2", "Linear 2", "Parabolic 2",
    "Exponential 3", "Linear 3", "Parabolic 3"
  ), LOO_plot = TRUE
)

comparison_netflix$LOO_plot <- comparison_netflix$LOO_plot +
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

ggplot2::ggsave(comparison_netflix$LOO_plot,
  filename = "model_comparison_netflix.png",
  dpi = 1000, device = "png", path = here::here("output", "figures", "experiment1")
)

ggpubr::ggarrange(
  comparison_tiktok$LOO_plot,
  comparison_netflix$LOO_plot,
  ncol = 2, nrow = 1,
  common.legend = TRUE, legend = "bottom",
  labels = c("A", "B")
) |>
  ggplot2::ggsave(
    filename = "model_comparison_both.png",
    width = 12, height = 4,
    dpi = 1000, device = "png", path = here::here("output", "figures", "experiment1")
  )

## POSTERIOR PREDICTIVE CHECKS (winning model only) =========================================================================

data_cleaned <- modeling_cleandata(data$task_data)
task_data_tiktok <- data_cleaned$tiktok
task_data_netflix <- data_cleaned$netflix

# extract posterior predictives for tiktok
ppc_tiktok <- posterior_predictions(
  csv_paths = m3_parabolic_fit_tiktok$output_files(),
  n_chains = 4,
  n_iter = 14 * 10000,
  n_subj = length(unique(data$task_data$subj_id)),
  n_trials = 64,
  real_dat = task_data_tiktok
)

# extract posterior predictives for netflix
ppc_netflix <- posterior_predictions(
  csv_paths = m3_parabolic_fit_netflix$output_files(),
  n_chains = 4,
  n_iter = 14 * 10000,
  n_subj = length(unique(data$task_data$subj_id)),
  n_trials = 64,
  real_dat = task_data_netflix
)

# save results
saveRDS(ppc_tiktok, here::here("data", "experiment1", "modelling", "m3_para_ppc_tiktok.RDS"))
saveRDS(ppc_netflix, here::here("data", "experiment1", "modelling", "m3_para_ppc_netflix.RDS"))

m3_para_ppc_tiktok <- readRDS(here::here("data", "experiment1", "modelling", "m3_para_ppc_tiktok.RDS"))
m3_para_ppc_netflix <- readRDS(here::here("data", "experiment1", "modelling", "m3_para_ppc_netflix.RDS"))

#### Rename for function ----------------------------------------------------------------------------------------------------

# Rename columns in the trial_type data frame
names(m3_para_ppc_tiktok$posterior_predictions_trial_type)[
  names(m3_para_ppc_tiktok$posterior_predictions_trial_type) == "amount_a"
] <- "offerReward"

names(m3_para_ppc_tiktok$posterior_predictions_trial_type)[
  names(m3_para_ppc_tiktok$posterior_predictions_trial_type) == "effort_a"
] <- "offerEffort"

# rename in the effort and reward data frames for the individual plots
names(m3_para_ppc_tiktok$posterior_predictions_effort)[
  names(m3_para_ppc_tiktok$posterior_predictions_effort) == "effort_a"
] <- "offerEffort"

names(m3_para_ppc_tiktok$posterior_predictions_reward)[
  names(m3_para_ppc_tiktok$posterior_predictions_reward) == "amount_a"
] <- "offerReward"

# For Netflix data
m3_para_ppc_netflix <- readRDS(here::here("data", "experiment1", "modelling", "m3_para_ppc_netflix.RDS"))

# Rename columns in the trial_type data frame
names(m3_para_ppc_netflix$posterior_predictions_trial_type)[
  names(m3_para_ppc_netflix$posterior_predictions_trial_type) == "amount_a"
] <- "offerReward"

names(m3_para_ppc_netflix$posterior_predictions_trial_type)[
  names(m3_para_ppc_netflix$posterior_predictions_trial_type) == "effort_a"
] <- "offerEffort"

# Rename in the effort and reward data frames
names(m3_para_ppc_netflix$posterior_predictions_effort)[
  names(m3_para_ppc_netflix$posterior_predictions_effort) == "effort_a"
] <- "offerEffort"

names(m3_para_ppc_netflix$posterior_predictions_reward)[
  names(m3_para_ppc_netflix$posterior_predictions_reward) == "amount_a"
] <- "offerReward"

#### Visualise PPCs ---------------------------------------------------------------------------------------------------------

# create plots of posterior model predictives
ppc_plot_tiktok <- ppc_plots(m3_para_ppc_tiktok, indiv_plot_title = "", group_plot_title = "", show_legend = FALSE)
ppc_plot_netflix <- ppc_plots(m3_para_ppc_netflix, indiv_plot_title = "", group_plot_title = "", show_legend = FALSE)

# save posterior predictive plots (individual estimates)
ggplot2::ggsave(ppc_plot_tiktok$indiv_plot,
  filename = "ppc_plot_tiktok.png",
  dpi = 1000, device = "png", path = here::here("output", "figures", "experiment1")
)
ggplot2::ggsave(ppc_plot_netflix$indiv_plot,
  filename = "ppc_plot_netflix.png",
  dpi = 1000, device = "png", path = here::here("output", "figures", "experiment1")
)

# save posterior predictive plots (grouped estimates)
ggplot2::ggsave(ppc_plot_tiktok$group_plot,
  filename = "ppc_plot_group_tiktok.png",
  dpi = 1000, device = "png", path = here::here("output", "figures", "experiment1")
)
ggplot2::ggsave(ppc_plot_netflix$group_plot,
  filename = "ppc_plot_group_netflix.png",
  dpi = 1000, device = "png", path = here::here("output", "figures", "experiment1")
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
  theme_bw() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    theme.background = element_rect(fill = "white", color = NA)
  )

ppc_leg <- cowplot::get_legend(dummy)

# save individual plots
ggpubr::ggarrange(
  ppc_plot_tiktok$indiv_plot + theme(
    plot.background = element_rect(fill = "white", color = NA)
  ),
  ppc_plot_netflix$indiv_plot + theme(
    plot.background = element_rect(fill = "white", color = NA)
  ),
  ncol = 2, nrow = 1,
  legend.grob = ppc_leg,
  common.legend = TRUE, legend = "bottom",
  labels = c("A", "B"),
  vjust = 3
) |>
  ggplot2::ggsave(
    filename = "ppc_indiv_both.png",
    width = 12, height = 4, bg = "white",
    dpi = 1000, device = "png", path = here::here("output", "figures")
  )

# save grouped plots
ggpubr::ggarrange(
  ppc_plot_tiktok$group_plot + theme(
    plot.background = element_rect(fill = "white", color = NA)
  ),
  ppc_plot_netflix$group_plot + theme(
    plot.background = element_rect(fill = "white", color = NA)
  ),
  ncol = 2, nrow = 1,
  common.legend = TRUE, legend = "bottom",
  labels = c("A", "B"),
  vjust = 3
) |>
  ggplot2::ggsave(
    filename = "ppc_group_both.png",
    width = 12, height = 4,
    dpi = 1000, device = "png", path = here::here("output", "figures")
  )

## PARAMETER ESTIMATES ======================================================================================================

m3_para_all_params

kE_group_mean <- m3_para_all_params$group_params |>
  dplyr::filter(parameter == "mu_kE") |>
  .$estimate

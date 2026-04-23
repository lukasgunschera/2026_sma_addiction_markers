## ======================================================================================================================= ##
## Script:    Descriptive Statistics
## ======================================================================================================================= ##
## Authors:   Clelia Veith, Lukas Gunschera
## Date:      Tue Nov 18 07:59:02 2025
## ======================================================================================================================= ##
## Notes:
##  Data sets description:
##   data: data control group + addiction group (after exclusion)
##   excl_data: excluded data
##   all_data: included data + excluded data
##
## Contents:
##   1) Data exclusion
##   2) Demographics
##   3) Questionnaire summaries
##   4) Task performance metrics
##   5) Table for report
## ======================================================================================================================= ##

# set parameters and seed
source(here::here("code", "00_setup.R"))

library(renv) # load renv package
renv::restore() # restore the environment

#### Load dependencies ------------------------------------------------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  here, ggplot2, ggpubr, tidyverse, dplyr, stringr, purrr,
  janitor, MatchIt, writexl, lubridate, tidyr, kableExtra
)

# Source helper functions
source(here::here("code", "experiment2", "functions", "helper_functions.R"))

# Load data from file (preprocessed data)
data <- readRDS(here::here("data", "experiment2", "processed", "data_clean.RDS"))
meta_data <- readRDS(here::here("data", "experiment2", "processed", "meta_data.RDS"))

## (1) DEMOGRAPHICS =========================================================================================================

## Extraction of final comments - for qualitative review
cat("Final comments - for qualitative review:\n")
data$questionnaires |>
  select(subj_id, final_comment) |>
  filter(!is.na(final_comment))

# ### Excluded sample -------------------------------------------------------------------------------------------------------
#
# if (nrow(excl_data) > 0) { # Only if there are excluded participants .
#
#   excl_data |>
#     summarise(
#       N = n(),
#       # N_perc = (n() / n_invited) * 100,
#       mean_age = mean(age), sd_age = sd(age),
#       min_age = min(age), max_age = max(age), # median_ses = median(ses),
#       # iqr_upper_ses = quantile(ses, 0.75),
#       # iqr_lower_ses = quantile(ses, 0.25))
#     )
#
#   excl_data |>
#     mutate(
#       gender = ifelse(gender == "other", gender_other, gender)
#     ) |>
#     tabyl(gender)
#
#   excl_data |>
#     mutate(
#       ethnicity = ifelse(ethnicity == "other", ethnicity_other, ethnicity)
#     ) |>
#     tabyl(ethnicity)
#
#   excl_data$demographics |>
#     tabyl(neuro_screening)
#
#   excl_data$demographics |>
#     tabyl(mh_screening)
# }


### Included sample (both groups) -------------------------------------------------------------------------------------------

# Demographics of all participants (both groups)
data$demographics |>
  dplyr::summarise(
    N = n(),
    mean_age = mean(age), sd_age = sd(age),
    min_age = min(age), max_age = max(age),
    mean_income = mean(income_value), sd_income = sd(income_value),
    # median_ses = median(ses),
    # iqr_upper_ses = quantile(ses, 0.75),
    # iqr_lower_ses = quantile(ses, 0.25)
  )

# Gender
data$demographics |>
  dplyr::mutate(
    gender = ifelse(gender == "other", gender_other, gender)
  ) |>
  tabyl(gender)

# Ethnicity
data$demographics |>
  tabyl(ethnicity)

# Ethnicity: other
data$demographics |>
  dplyr::mutate(
    ethnicity = ifelse(ethnicity == "other", ethnicity_other, ethnicity)
  ) |>
  tabyl(ethnicity)


# Neurological disorders
data$demographics |>
  tabyl(neuro_screening)

# Neurological disorders: other
data$demographics |>
  dplyr::summarise(
    ms = sum(neuro_ms), epilepsy = sum(neuro_epilepsy),
    tbi = sum(neuro_tbi), headache = sum(neuro_headache),
    parkinsons = sum(neuro_parkinsons), stroke = sum(neuro_stroke)
  )

data$demographics |>
  tabyl(neuro_other)

# Mental health conditions
data$demographics |>
  tabyl(mh_screening)

# Mental health conditions: other
data$demographics |>
  dplyr::summarise(
    mdd = sum(mh_mdd), bpd = sum(mh_bpd), gad = sum(mh_gad),
    ocd = sum(mh_ocd), autism = sum(mh_autism)
  )

data$demographics |>
  tabyl(mh_other)

# Condition order
data$demographics |>
  tabyl(order)

### Included sample: by group (control, addiction) --------------------------------------------------------------------------

# Demographics by group
data$demographics |>
  dplyr::group_by(group) |>
  dplyr::summarise(
    N = n(),
    mean_age = mean(age), sd_age = sd(age),
    min_age = min(age), max_age = max(age),
    mean_income = mean(income_value), sd_income = sd(income_value),
    # median_ses = median(ses),
    # iqr_upper_ses = quantile(ses, 0.75),
    # iqr_lower_ses = quantile(ses, 0.25)
    .groups = "drop"
  )

# Gender by group
data$demographics |>
  tabyl(group, gender) |>
  adorn_percentages("row") |>
  adorn_pct_formatting(digits = 2) |>
  adorn_ns()

# Ethnicity by group
data$demographics |>
  tabyl(group, ethnicity) |>
  adorn_percentages("row") |>
  adorn_pct_formatting(digits = 2) |>
  adorn_ns()

# Neurological disorders by group
data$demographics |>
  tabyl(group, neuro_screening) |>
  adorn_percentages("row") |>
  adorn_pct_formatting(digits = 2) |>
  adorn_ns()

# Mental health conditions by group
data$demographics |>
  tabyl(group, mh_screening) |>
  adorn_percentages("row") |>
  adorn_pct_formatting(digits = 2) |>
  adorn_ns()

cat("Condition order by group:\n")
data$demographics |>
  tabyl(group, order)


### Age & gender bins for recruitment of control group ----------------------------------------------------------------------

addiction_demographics <- data$demographics |>
  dplyr::filter(group == "addiction")

# Plot age distribution
hist(addiction_demographics$age,
  breaks = seq(min(addiction_demographics$age, na.rm = TRUE),
    max(addiction_demographics$age, na.rm = TRUE) + 1,
    by = 1
  ),
  xlab = "Age (years)",
  main = "Age distribution"
)

# Plot gender distribution
barplot(table(addiction_demographics$gender),
  xlab = "Gender",
  main = "Gender distribution"
)

# Age quartiles
age_quartiles_addiction <- quantile(addiction_demographics$age,
  probs = c(0, 0.25, 0.5, 0.75, 1),
  na.rm = TRUE
)

data$demographics <- data$demographics |>
  dplyr::mutate(age_group = cut(age,
    breaks = age_quartiles_addiction,
    labels = c("Q1", "Q2", "Q3", "Q4"),
    include.lowest = TRUE
  ))

# Create age & gender bins
data$demographics |>
  dplyr::filter(group == "addiction") |>
  dplyr::count(age_group, gender) |>
  tidyr::pivot_wider(names_from = gender, values_from = n, values_fill = 0)


# Check age & gender distribution of control group
control_demographics <- data$demographics |>
  dplyr::filter(group == "control")

data$demographics |>
  dplyr::filter(!is.na(age_group)) |>
  dplyr::group_by(group, age_group, gender) |>
  dplyr::summarise(n = n(), .groups = "drop") |>
  tidyr::pivot_wider(names_from = gender, values_from = n, values_fill = 0)

## (3) QUESTIONNAIRES ======================================================================================================

### Included sample ---------------------------------------------------------------------------------------------------------

cat("Summary questionnaire responses:\n")
data$questionnaires |>
  dplyr::summarise(
    bsmas_mean = mean(bsmas_sum), bsmas_sd = sd(bsmas_sum),
    phq8_mean = mean(phq8_sum), phq8_sd = sd(phq8_sum),
    gad7_mean = mean(gad7_sum), gad7_sd = sd(gad7_sum),
    bisr21_mean = mean(bisr21_sum), bisr21_sd = sd(bisr21_sum),
    wsas5_mean = mean(wsas5_sum), wsas5_sd = sd(wsas5_sum)
  )


### Included sample: by group -----------------------------------------------------------------------------------------------

cat("Summary questionnaire responses by group:\n")
data$questionnaires |>
  dplyr::group_by(group) |>
  dplyr::summarise(
    N = n(),
    bsmas_mean = mean(bsmas_sum), bsmas_sd = sd(bsmas_sum),
    phq8_mean = mean(phq8_sum), phq8_sd = sd(phq8_sum),
    gad7_mean = mean(gad7_sum), gad7_sd = sd(gad7_sum),
    bisr21_mean = mean(bisr21_sum), bisr21_sd = sd(bisr21_sum),
    wsas5_mean = mean(wsas5_sum), wsas5_sd = sd(wsas5_sum),
    .groups = "drop"
  )

## (4) TASK METRICS =========================================================================================================

# Duration (included sample)
cat("Task duration:\n")
data$game_meta |>
  dplyr::mutate(time_diff = abs(difftime(start_time, end_time))) |>
  dplyr::group_by(task_condition) |>
  dplyr::summarise(
    mean_time_diff = mean(time_diff), sd_time_diff = sd(time_diff),
    min_time_diff = min(time_diff), max_time_diff = max(time_diff)
  )

# Mean clicking calibration (included sample)
cat("Mean clicking calibration:\n")
data$game_meta |>
  dplyr::summarise(
    mean_cali = mean(clicking_calibration),
    sd_cali = sd(clicking_calibration),
    min_cali = min(clicking_calibration),
    max_cali = max(clicking_calibration)
  )

## (5) TABLE FOR REPORT =========================================================================================================

# Helper functions
fmt_n_pct <- function(n, total) {
  paste0(n, " (", round(100 * n / total, 1), "%)")
}

fmt_mean_sd <- function(x) {
  paste0(
    round(mean(x, na.rm = TRUE), 2),
    " (", round(sd(x, na.rm = TRUE), 2), ")"
  )
}

# N
N_total <- nrow(data$demographics)

N_by_group <- data$demographics |>
  dplyr::count(group) |>
  deframe()

# Age
age_tbl <- tibble(
  Demographic = "**Age (years)**",
  Total = fmt_mean_sd(data$demographics$age),
  control = fmt_mean_sd(data$demographics$age[data$demographics$group == "control"]),
  addiction = fmt_mean_sd(data$demographics$age[data$demographics$group == "addiction"])
)

# Gender
gender_tbl <- data$demographics |>
  dplyr::mutate(gender = ifelse(gender == "other", gender_other, gender)) |>
  dplyr::count(group, gender) |>
  dplyr::group_by(group) |>
  dplyr::mutate(value = fmt_n_pct(n, sum(n))) |>
  dplyr::ungroup() |>
  dplyr::select(group, gender, value) |>
  tidyr::pivot_wider(names_from = group, values_from = value)

gender_total <- data$demographics |>
  dplyr::mutate(gender = ifelse(gender == "other", gender_other, gender)) |>
  dplyr::count(gender) |>
  dplyr::mutate(Total = fmt_n_pct(n, sum(n))) |>
  dplyr::select(gender, Total)

gender_tbl <- gender_total |>
  dplyr::left_join(gender_tbl, by = "gender") |>
  dplyr::mutate(Demographic = paste0("  ", str_to_sentence(gender))) |>
  dplyr::select(Demographic, Total, control, addiction)

gender_tbl <- bind_rows(
  tibble(Demographic = "**Gender**", Total = "", control = "", addiction = ""),
  gender_tbl
)

# Ethnicity
ethnicity_tbl <- data$demographics |>
  dplyr::mutate(ethnicity = ifelse(ethnicity == "other", ethnicity_other, ethnicity)) |>
  dplyr::count(group, ethnicity) |>
  dplyr::group_by(group) |>
  dplyr::mutate(value = fmt_n_pct(n, sum(n))) |>
  dplyr::ungroup() |>
  dplyr::select(group, ethnicity, value) |>
  tidyr::pivot_wider(names_from = group, values_from = value)

ethnicity_total <- data$demographics |>
  dplyr::mutate(ethnicity = ifelse(ethnicity == "other", ethnicity_other, ethnicity)) |>
  dplyr::count(ethnicity) |>
  dplyr::mutate(Total = fmt_n_pct(n, sum(n))) |>
  dplyr::select(ethnicity, Total)

ethnicity_tbl <- ethnicity_total |>
  dplyr::left_join(ethnicity_tbl, by = "ethnicity") |>
  dplyr::mutate(Demographic = paste0("  ", ethnicity)) |>
  dplyr::select(Demographic, Total, control, addiction)

ethnicity_tbl <- bind_rows(
  tibble(Demographic = "**Ethnicity**", Total = "", control = "", addiction = ""),
  ethnicity_tbl
)

# Neuorlogical disorders
neuro_tbl <- data$demographics |>
  dplyr::count(group, neuro_screening) |>
  dplyr::group_by(group) |>
  dplyr::mutate(value = fmt_n_pct(n, sum(n))) |>
  dplyr::ungroup() |>
  dplyr::select(group, neuro_screening, value) |>
  tidyr::pivot_wider(names_from = group, values_from = value)

neuro_total <- data$demographics |>
  dplyr::count(neuro_screening) |>
  dplyr::mutate(Total = fmt_n_pct(n, sum(n))) |>
  dplyr::select(neuro_screening, Total)

neuro_tbl <- neuro_total |>
  dplyr::left_join(neuro_tbl, by = "neuro_screening") |>
  dplyr::mutate(Demographic = paste0("  ", neuro_screening)) |>
  dplyr::select(Demographic, Total, control, addiction)

neuro_tbl <- bind_rows(
  tibble(Demographic = "**Neurological disorder**", Total = "", control = "", addiction = ""),
  neuro_tbl
)

# Mental health conditions
mh_tbl <- data$demographics |>
  dplyr::count(group, mh_screening) |>
  dplyr::group_by(group) |>
  dplyr::mutate(value = fmt_n_pct(n, sum(n))) |>
  dplyr::ungroup() |>
  dplyr::select(group, mh_screening, value) |>
  tidyr::pivot_wider(names_from = group, values_from = value)

mh_total <- data$demographics |>
  dplyr::count(mh_screening) |>
  dplyr::mutate(Total = fmt_n_pct(n, sum(n))) |>
  dplyr::select(mh_screening, Total)

mh_tbl <- mh_total |>
  dplyr::left_join(mh_tbl, by = "mh_screening") |>
  dplyr::mutate(Demographic = paste0("  ", mh_screening)) |>
  dplyr::select(Demographic, Total, control, addiction)

mh_tbl <- bind_rows(
  tibble(Demographic = "**Mental health condition**", Total = "", control = "", addiction = ""),
  mh_tbl
)

# Condition order
order_tbl <- data$demographics |>
  dplyr::count(group, order) |>
  dplyr::group_by(group) |>
  dplyr::mutate(value = fmt_n_pct(n, sum(n))) |>
  dplyr::ungroup() |>
  dplyr::select(group, order, value) |>
  tidyr::pivot_wider(names_from = group, values_from = value)

order_total <- data$demographics |>
  dplyr::count(order) |>
  dplyr::mutate(Total = fmt_n_pct(n, sum(n))) |>
  dplyr::select(order, Total)

order_tbl <- order_total |>
  dplyr::left_join(order_tbl, by = "order") |>
  dplyr::mutate(Demographic = paste0("  ", order)) |>
  dplyr::select(Demographic, Total, control, addiction)

order_tbl <- bind_rows(
  tibble(Demographic = "**Condition order**", Total = "", control = "", addiction = ""),
  order_tbl
)

# Final table
table_demographics <- dplyr::bind_rows(
  age_tbl,
  gender_tbl,
  ethnicity_tbl,
  neuro_tbl,
  mh_tbl,
  order_tbl
)

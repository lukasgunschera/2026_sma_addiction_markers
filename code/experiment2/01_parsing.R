## ======================================================================================================================= ##
## Script:    Parse Raw Participant Data
## ======================================================================================================================= ##
## Authors:   Lukas Gunschera, Clelia Veith
## Date:      Tue Nov 18 07:57:48 2025
## ======================================================================================================================= ##
## JATOS raw data files & Prolific meta data files are parsed and saved as RDS files
## for further analyses
## ======================================================================================================================= ##

## SETUP ====================================================================================================================

# set parameters and seed
source(here::here("code", "00_setup.R"))

library(renv) # load renv package
renv::restore() # restore the environment

#### Load dependencies ------------------------------------------------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(here, dplyr, readr, magrittr)

source(here::here("code", "experiment2", "functions", "fun_helper.R"))
source(here::here("code", "experiment2", "functions", "fun_parsing.R"))

# load data from raw data folder
raw_data_location <- list.files(
  path = here::here("data", "experiment2", "raw"), pattern = "\\.txt$",
  full.names = TRUE, recursive = TRUE
)

if (length(raw_data_location) == 0) {
  message("Raw data are not available - loading processed & anonymised data from file")
  data <- readRDS(here::here("data", "experiment2", "processed", "data_cleaned.rds"))
} else {
  message("Raw data found - parsing raw data files")


  files <- raw_data_location[grepl(".*\\.txt$", raw_data_location)]

  ## PARSE RAW DATA =========================================================================================================

  processed_files <- lapply(files, parsing_fun)

  # combine processed files into a single object
  data <- list(
    demographics = do.call(rbind, lapply(processed_files, `[[`, "demographics")),
    game = do.call(rbind, lapply(processed_files, `[[`, "game")),
    game_meta = do.call(rbind, lapply(processed_files, `[[`, "game_meta")),
    modelling_data = do.call(rbind, lapply(processed_files, `[[`, "modelling_data")),
    tiktok_use = do.call(rbind, lapply(processed_files, `[[`, "tiktok_use")),
    questionnaires = do.call(rbind, lapply(processed_files, `[[`, "questionnaires"))
  )

  ## ANONYMISE RAW DATA =====================================================================================================

  # mapping from ALL unique IDs across the entire dataset
  all_ids <- unique(c(
    data$demographics$subj_id,
    data$game$subj_id,
    data$game_meta$subj_id,
    data$modelling_data$subjID,
    data$tiktok_use$subj_id,
    data$questionnaires$subj_id
  ))

  id_map <- tibble(
    original_id = all_ids,
    anon_id     = paste0("P", str_pad(seq_along(all_ids), width = 3, pad = "0"))
    # gives P001, P002, ... P240
  )

  # helper function to recode a column using the map
  recode_id <- function(ids, map) {
    map$anon_id[match(ids, map$original_id)]
  }

  # apply to each component
  data$demographics <- data$demographics %>%
    mutate(subj_id = recode_id(subj_id, id_map))

  data$game <- data$game %>%
    mutate(subj_id = recode_id(subj_id, id_map))

  data$game_meta <- data$game_meta %>%
    mutate(subj_id = recode_id(subj_id, id_map))

  data$modelling_data <- data$modelling_data %>%
    mutate(subjID = recode_id(subjID, id_map))

  data$tiktok_use <- data$tiktok_use %>%
    mutate(subj_id = recode_id(subj_id, id_map))

  data$questionnaires <- data$questionnaires %>%
    mutate(subj_id = recode_id(subj_id, id_map))

  # verify no original IDs remain
  stopifnot(!any(all_ids %in% data$demographics$subj_id))
  stopifnot(!any(all_ids %in% data$game$subj_id))
  stopifnot(!any(all_ids %in% data$game_meta$subj_id))
  stopifnot(!any(all_ids %in% data$modelling_data$subjID))
  stopifnot(!any(all_ids %in% data$tiktok_use$subj_id))
  stopifnot(!any(all_ids %in% data$questionnaires$subj_id))

  # save the mapping separately (not to share)
  write.csv(id_map, file = here::here("data", "experiment2", "id_mapping.csv"), row.names = FALSE)

  ## ERROR CHECKS ===========================================================================================================

  age_check <- data$demographics |>
    group_by(group) |>
    dplyr::mutate(
      age_group = case_when(
        age >= 18 & age <= 22 ~ "18-22",
        age >= 23 & age <= 27 ~ "23-27",
        age >= 28 & age <= 32 ~ "28-32",
        age >= 33 & age <= 35 ~ "33-35",
        TRUE ~ "Outside range"
      )
    ) |>
    dplyr::count(age_group) |>
    dplyr::mutate(
      percent = round(n / nrow(data$demographics) * 100, 1),
      target_n = case_when(
        age_group == "18-22" ~ 33,
        age_group == "23-27" ~ 33,
        age_group == "28-32" ~ 32,
        age_group == "33-35" ~ 32,
        TRUE ~ 0
      ),
      target_percent = ifelse(age_group %in% c("18-22", "23-27", "28-32", "33-35"), 25, 0),
      diff = n - target_n
    )

  current_counts <- data$demographics |>
    dplyr::mutate(
      age_group = case_when(
        age >= 18 & age <= 22 ~ "18-22",
        age >= 23 & age <= 27 ~ "23-27",
        age >= 28 & age <= 32 ~ "28-32",
        age >= 33 & age <= 35 ~ "33-35",
        TRUE ~ "Outside range"
      )
    ) |>
    dplyr::count(age_group)

  gender_check <- data$demographics |>
    dplyr::mutate(
      gender_group = case_when(
        tolower(gender) %in% c("man", "male", "trans male", "trans man") ~ "Man",
        tolower(gender) %in% c("woman", "female", "trans female", "trans woman") ~ "Woman",
        TRUE ~ "Non-binary/Other"
      )
    ) |>
    dplyr::count(gender_group) |>
    dplyr::mutate(
      percent = round(n / nrow(data$demographics) * 100, 1),
      target_n = case_when(
        gender_group == "Man" ~ 38,
        gender_group == "Woman" ~ 90,
        gender_group == "Non-binary/Other" ~ 2
      ),
      target_percent = case_when(
        gender_group == "Man" ~ 29.0,
        gender_group == "Woman" ~ 69,
        gender_group == "Non-binary/Other" ~ 2
      ),
      diff = n - target_n
    )

  # Extract unique subject IDs from each element
  subj_demographics <- unique(data$demographics$subj_id)
  subj_game <- unique(data$game$subj_id)
  subj_game_meta <- unique(data$game_meta$subj_id)
  subj_modelling <- unique(data$modelling_data$subjID) # Note: different column name
  subj_tiktok <- unique(data$tiktok_use$subj_id)
  subj_questionnaires <- unique(data$questionnaires$subj_id)

  # count subjects in each element
  cat("Number of unique subjects per data element:\n")
  cat("Demographics:", length(subj_demographics), "\n")
  cat("Game:", length(subj_game), "\n")
  cat("Game Meta:", length(subj_game_meta), "\n")
  cat("Modelling Data:", length(subj_modelling), "\n")
  cat("TikTok Use:", length(subj_tiktok), "\n")
  cat("Questionnaires:", length(subj_questionnaires), "\n\n")

  ## CLEANING AND EXCLUSION =================================================================================================
  # Preregistered exclusion criteria at https://osf.io/mnk3b/overview
  # (1) fail one ore more simple catch question
  # (2) fail two hard catch questions
  # (3) incomplete data based on which confirmatory hypotheses cannot be tested
  # (4) self-reported noncompliance, evaluated to be severe enough to potentially impact data quality

  ### (1 & 2) Failed catch question exclusions ------------------------------------------------------------------------------

  # Merge demographics, TikTok use & questionnaire data
  data$demographics <- data$demographics %>%
    left_join(data$questionnaires |> dplyr::select(-group), by = "subj_id") |>
    left_join(data$tiktok_use |> dplyr::select(-group), by = "subj_id") |>
    dplyr::mutate(
      exclude_catch = ifelse(catch_pass_easy == FALSE | catch_pass_hard == FALSE, TRUE, FALSE)
    )

  # Get number of excluded participants
  exclusion_summary <- data$demographics |>
    dplyr::summarise(
      n_excluded_total = sum(exclude_catch, na.rm = TRUE),
      n_excluded_easy = sum(catch_pass_easy == FALSE, na.rm = TRUE),
      n_excluded_hard = sum(catch_pass_hard == FALSE, na.rm = TRUE),
      n_excluded_both = sum(catch_pass_easy == FALSE & catch_pass_hard == FALSE, na.rm = TRUE)
    )

  # IDs of excluded people
  catch_exclusions <- data$demographics |>
    dplyr::filter(exclude_catch == TRUE) |>
    pull(subj_id)

  # Results printed
  print(exclusion_summary)
  cat("\nExcluded IDs:", paste(catch_exclusions, collapse = ", "), "\n")

  ### (3) Incomplete data exclusions ----------------------------------------------------------------------------------------

  print("Incomplete data are removed inside the parsing function, please see function for details")

  ### (4) Non-compliance manual inspection and inclusion --------------------------------------------------------------------

  # Task compliance - baseline
  task_noncompliant_b <- data$demographics |>
    dplyr::filter(task_compliance_b == "yes") |>
    dplyr::select(subj_id, group, task_compliance_b, task_compliance_text_b)

  # Task compliance - devalued
  task_noncompliant_d <- data$demographics |>
    dplyr::filter(task_compliance_d == "yes") |>
    dplyr::select(subj_id, group, task_compliance_d, task_compliance_text_d)

  # TikTok compliance - usage during waiting period
  tiktok_noncompliant <- data$demographics |>
    dplyr::filter(tiktok_compliance_confirmation == 0 | tiktok_compliance_confirmation == "other") |>
    dplyr::select(
      subj_id, group, tiktok_compliance_confirmation,
      tiktok_non_compliance_session, tiktok_non_compliance_explanation
    )

  # Devaluation compliance - settings (greyscale, no sound)
  dev_noncompliant <- data$demographics |>
    dplyr::filter(dev_compliance_confirmation == 0 | dev_compliance_confirmation == "other") |>
    dplyr::select(
      subj_id, group, dev_compliance_confirmation,
      dev_non_compliance_session, dev_non_compliance_explanation
    )

  # Non-compliance summary
  compliance_summary <- data.frame(
    type = c("Task baseline", "Task devaluation", "TikTok use", "Devaluation settings"),
    n_noncompliant = c(
      nrow(task_noncompliant_b),
      nrow(task_noncompliant_d),
      nrow(tiktok_noncompliant),
      nrow(dev_noncompliant)
    )
  )

  # Print summary
  print(compliance_summary)

  # View each type for manual review
  cat("\n=== TASK COMPLIANCE - BASELINE ===\n")
  print(task_noncompliant_b)
  cat("\n=== TASK COMPLIANCE - DEVALUATION ===\n")
  print(task_noncompliant_d)
  cat("\n=== TIKTOK COMPLIANCE ===\n")
  print(tiktok_noncompliant)
  cat("\n=== DEVALUATION COMPLIANCE ===\n")
  print(dev_noncompliant)

  # Manual review of non-compliance reasons to gauge severity and impact on data quality
  manual_compliance_exclusions <- c(
    "P005",
    "P017",
    "P060"
  ) # manually add identifiers following review

  ### (4) Extra manual exclusions ----------------------------------------------------------------------

  # Manual exclusion of participants with missing task data
  manual_extra_exclusions <- c("P133")

  # Note: there are also participants who completed the experimental session ("Approved" in meta data)
  # but not all their components were saved (n = 7). They automatically get excluded here with the
  # parsing, however they are not explicitely included in the excluded summary

  # Extract participants who repeated quiz more than 5 times
  extra_quiz <- data$game |>
    dplyr::filter(phase == "quiz") |>
    group_by(subj_id) |>
    dplyr::summarise(n_quiz_trials = n(), .groups = "drop") |>
    dplyr::filter(n_quiz_trials > 30) |> # > 5 trials
    dplyr::mutate(n_repeats = n_quiz_trials / 6) |>
    dplyr::select(subj_id, n_repeats)

  print(extra_quiz)

  ### (5) Combine all exclusions and create clean data set ------------------------------------------------------------------

  # Strict exclusions for sensitivity analyses
  strict_exclusion_sensitivity <- unique(c(task_noncompliant_b$subj_id, task_noncompliant_d$subj_id, tiktok_noncompliant$subj_id, dev_noncompliant$subj_id, manual_extra_exclusions, catch_exclusions))

  # Combine all exclusion vectors
  all_exclusions <- unique(c(catch_exclusions, manual_compliance_exclusions, manual_extra_exclusions))

  # Add exclusion flags to demographics
  data$demographics <- data$demographics |>
    dplyr::mutate(
      exclude_manual_compliance = subj_id %in% manual_compliance_exclusions,
      exclude_manual_extra = subj_id %in% manual_extra_exclusions,
      exclude_final = subj_id %in% all_exclusions
    )

  # Vector of included identifiers
  included_ids <- data$demographics$subj_id[!data$demographics$exclude_final]

  # Create cleaned data object (excluding all flagged participants)
  data_clean <- list(
    demographics = data$demographics |> dplyr::filter(subj_id %in% included_ids),
    questionnaires = data$questionnaires |> dplyr::filter(subj_id %in% included_ids),
    tiktok_use = data$tiktok_use |> dplyr::filter(subj_id %in% included_ids),
    game = data$game |> dplyr::filter(subj_id %in% included_ids),
    game_meta = data$game_meta |> dplyr::filter(subj_id %in% included_ids),
    modelling_data = data$modelling_data |> dplyr::filter(subjID %in% included_ids)
  )

  # Vector of included identifiers for strict sensitivity analyses
  included_ids_strict_sensitivity <- data$demographics$subj_id[!data$demographics$subj_id %in% strict_exclusion_sensitivity]

  # Create strict dataset for sensitivity analyses
  data_strict_sensitivity <- list(
    demographics = data$demographics |> dplyr::filter(subj_id %in% included_ids_strict_sensitivity),
    questionnaires = data$questionnaires |> dplyr::filter(subj_id %in% included_ids_strict_sensitivity),
    tiktok_use = data$tiktok_use |> dplyr::filter(subj_id %in% included_ids_strict_sensitivity),
    game = data$game |> dplyr::filter(subj_id %in% included_ids_strict_sensitivity),
    game_meta = data$game_meta |> dplyr::filter(subj_id %in% included_ids_strict_sensitivity),
    modelling_data = data$modelling_data |> dplyr::filter(subjID %in% included_ids_strict_sensitivity)
  )

  # save dataset without exclusions for sensitivity analyses
  saveRDS(data, here::here("data", "experiment2", "processed", "data_sensitivity_full.rds"))
  # save dataset with strict exclusions
  saveRDS(data_strict_sensitivity, here::here("data", "experiment2", "processed", "data_sensitivity_strict.rds"))

  # Final exclusion summary
  final_exclusion_summary <- data.frame(
    criterion = c(
      "Original sample", "Catch questions", "Manual compliance",
      "Manual extra", "Total excluded", "Final sample"
    ),
    n = c(
      nrow(data$demographics),
      length(catch_exclusions),
      length(manual_compliance_exclusions),
      length(manual_extra_exclusions),
      length(all_exclusions),
      sum(!data$demographics$exclude_final)
    )
  )

  cat("\n=== FINAL EXCLUSION SUMMARY ===\n")
  cat("\nAll excluded IDs:", paste(all_exclusions, collapse = ", "), "\n")


  ## PARSING META DATA ======================================================================================================

  ### Load meta data from meta folder
  meta_data_location <- list.files(
    path = here::here("data", "experiment2", "meta"), pattern = "\\.csv$",
    full.names = TRUE, recursive = TRUE
  )

  ### Read and combine all csv files
  # Get column names
  all_colnames <- lapply(
    meta_data_location,
    function(f) names(read.csv(f, stringsAsFactors = FALSE))
  )

  # Find columns that exist in all files
  common_cols <- Reduce(intersect, all_colnames)
  common_cols

  # Read files only with selected common columns
  meta_data_list <- lapply(meta_data_location, function(f) {
    df <- read.csv(f, stringsAsFactors = FALSE)
    df[, common_cols, drop = FALSE]
  })

  # Combine all files
  meta_data <- do.call(rbind, meta_data_list)

  ## SAVE PROCESSED DATA ====================================================================================================

  saveRDS(data_clean, here::here("data", "experiment2", "processed", "data_clean.rds"))
  saveRDS(all_exclusions, here::here("data", "experiment2", "processed", "excluded_ids.rds"))
  saveRDS(final_exclusion_summary, here::here("data", "experiment2", "processed", "exclusion_summary.rds"))
  saveRDS(meta_data, here::here("data", "experiment2", "processed", "meta_data.rds"))
}

## CREATE CODEBOOK ==========================================================================================================

# extract labels and convert to tibble
codebook_names <- purrr::map_dfr(data_clean, ~ enframe(get_label(.x)))

# compute descriptives and select relevant columns
codebook_descriptives <- purrr::map_dfr(
  data_clean,
  ~ psych::describe(.x) |>
    dplyr::as_tibble() |>
    dplyr::select("n", "min", "max", "mean", "sd", "skew", "kurtosis")
)

# combine results
codebook <- cbind(codebook_names, codebook_descriptives)

# save codebook
saveRDS(codebook, here::here("output", "documentation", "codebook_exp1.RDS"))

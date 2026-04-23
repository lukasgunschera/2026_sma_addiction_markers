## ======================================================================================================================= ##
## Script:    Parsing Function
## ======================================================================================================================= ##
## Authors:   Clelia Veith, Lukas Gunschera
## Date:      Tue Nov 18 07:59:02 2025
## ======================================================================================================================= ##
## Description:
##  Parsing function for processing JATOS .txt data files based on Mehrhof & Nord (2025) parsing function
##
##  Function arguments:
##
##  file: relative path to .txt data file or existing R object
##  screening_included: does data include screening component (TRUE) or not (FALSE)?
##  demographic_dat: include demographic data in output?
##  task_dat: include task data in output?
##  modelling_dat: include task data formatted for modelling?
##  tiktok_dat: include TikTok use data in output?
##  questionnaire_dat: include questionnaire data in output?
##
##  Notes:
##  _d denotes devalued; _b denotes baseline
## ======================================================================================================================= ##

# Load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(lubridate, jsonlite, ggplot2, ggpubr)

parsing_fun <- function(file,
                        screening_included = TRUE,
                        demographic_dat = TRUE,
                        task_dat = TRUE,
                        modelling_dat = TRUE,
                        tiktok_dat = TRUE,
                        questionnaire_dat = TRUE) {
  ### Dependencies ----------------------------------------------------------------------------------------------------------

  source(here::here("code", "experiment2", "functions", "helper_functions.R"))
  require(dplyr)
  require(labelled)
  require(tidyverse)
  require(lubridate)

  ### Import raw data -------------------------------------------------------------------------------------------------------

  if (inherits(file, "list")) {
    raw_dat <- file
  } else if (is.character(file)) {
    raw_dat <- list()
    for (f in file) {
      raw_dat <- append(
        raw_dat,
        fromJSON(sprintf("[%s]", paste(readLines(f, encoding = "UTF-8", warn = FALSE),
          collapse = ","
        )))
      )
    }
  }

  #### Add identifiers throughout -------------------------------------------------------------------------------------------

  # Propagate prolific_id down the rows
  last_id <- NA_character_

  raw_dat <- lapply(raw_dat, function(x) {
    # Update last_id if this component has its own prolific_id
    if (!is.null(x$prolific_id) && !is.na(x$prolific_id)) {
      last_id <<- x$prolific_id
    }

    # Only add prolific_id to task components that don't have one
    is_task_component <- any(grepl("This is the start of the game", unlist(x), fixed = TRUE))

    if (is_task_component && (is.null(x$prolific_id) || is.na(x$prolific_id))) {
      x$prolific_id <- last_id
    }

    x
  })

  ### Component Indexing ----------------------------------------------------------------------------------------------------

  make_index <- function(pattern, include_ids = NULL) {
    which(sapply(raw_dat, function(x) {
      # unify component content into character vector
      vals <- unlist(x, use.names = FALSE)

      # fail-safe: if unlist returns nothing, skip
      if (length(vals) == 0) {
        return(FALSE)
      }

      # extract propagated prolific ID if present
      pid <- if (!is.null(x$prolific_id)) x$prolific_id else NA

      # INCLUSION logic - only include if in include_ids
      keep <- TRUE
      if (!is.null(include_ids) && length(include_ids) > 0 && !is.na(pid)) {
        keep <- (pid %in% include_ids)
      }

      keep && any(grepl(pattern, vals, fixed = TRUE))
    }))
  }

  # First pass - find all components to determine completeness
  all_questionnaire_indexes <- make_index("questionnaire")

  ### Identify Complete Participants --------------------------------------------------------------------

  extract_id <- function(x) {
    id <- x$prolific_id[1]
    if (is.null(id) || is.na(id)) {
      return(NA_character_)
    }
    as.character(id)
  }

  all_ids <- unique(na.omit(sapply(raw_dat, extract_id)))
  complete_ids <- unique(na.omit(sapply(raw_dat[all_questionnaire_indexes], extract_id)))


  # Second pass - create indexes with INCLUSION but DEDUPLICATE
  make_index_dedup <- function(pattern, include_ids = NULL) {
    all_matches <- which(sapply(raw_dat, function(x) {
      vals <- unlist(x, use.names = FALSE)
      if (length(vals) == 0) {
        return(FALSE)
      }

      pid <- if (!is.null(x$prolific_id)) x$prolific_id else NA

      keep <- TRUE
      if (!is.null(include_ids) && length(include_ids) > 0 && !is.na(pid)) {
        keep <- (pid %in% include_ids)
      }

      keep && any(grepl(pattern, vals, fixed = TRUE))
    }))

    # Dedupliaction in case of duplicate entries
    seen_ids <- character()
    dedup_matches <- integer()

    for (idx in all_matches) {
      pid <- extract_id(raw_dat[[idx]])
      if (!is.na(pid) && !(pid %in% seen_ids)) {
        seen_ids <- c(seen_ids, pid)
        dedup_matches <- c(dedup_matches, idx)
      }
    }

    return(dedup_matches)
  }

  index_screening <- make_index_dedup("screening", complete_ids)
  index_intake <- make_index_dedup("intake", complete_ids)
  index_calibration <- make_index_dedup("calibration", complete_ids)
  index_trial1 <- make_index_dedup("trial1", complete_ids)
  index_trial2 <- make_index_dedup("trial2", complete_ids)
  index_calibration <- make_index_dedup("This is the start of the game", complete_ids)
  index_baseline <- make_index_dedup("This is the start of the game: baseline", complete_ids)
  index_baseline_use <- make_index_dedup("main_b", complete_ids)
  index_devalued <- make_index_dedup("This is the start of the game: devalued", complete_ids)
  index_devalued_use <- make_index_dedup("main_d", complete_ids)
  index_questionnaires <- make_index_dedup("questionnaire", complete_ids)

  ## SCREENING ==============================================================================================================

  has_screening_data <- length(index_screening) > 0

  # Initialise demographics data frame
  demographics_df <- tibble()

  if (has_screening_data) {
    cat("Found screening component - extracting demographics from screening\n")

    for (i in index_screening) {
      # Base columns that should exist in screening
      base_cols <- c("prolific_id", "consent_confirmation", "screening")
      demo_cols <- c("age", "gender")

      # Identify available columns
      available_cols <- base_cols[base_cols %in% colnames(raw_dat[[i]])]
      available_demos <- demo_cols[demo_cols %in% colnames(raw_dat[[i]])]

      # Extract all available columns
      demographics_df_subj <- raw_dat[[i]] |>
        select(all_of(c(available_cols, available_demos)))

      demographics_df <- bind_rows(demographics_df, demographics_df_subj)
    }
  } else {
    cat("No screening component found - extracting demographics from intake\n")

    if (length(index_intake) > 0) {
      for (i in index_intake) {
        # Base columns for intake
        base_cols <- c("prolific_id", "consent_confirmation")
        demo_cols <- c("age", "gender")

        # Identify available columns
        available_cols <- base_cols[base_cols %in% colnames(raw_dat[[i]])]
        available_demos <- demo_cols[demo_cols %in% colnames(raw_dat[[i]])]

        # Extract all available columns
        demographics_df_subj <- raw_dat[[i]] |> select(all_of(c(available_cols, available_demos)))

        demographics_df <- bind_rows(demographics_df, demographics_df_subj)
      }
    } else {
      warning("No valid intake data found for demographics extraction")
    }
  }

  ## INTAKE  ================================================================================================================

  cat("Extracting intake data from", length(index_intake), "participants\n")

  # Initialise intake data frame with proper structure
  intake_df <- tibble(
    subj_id = character(),
    consent = character(),
    group = character(),
    order = character(),
    age = numeric(),
    gender = character(),
    gender_other = character(),
    ethnicity = character(),
    ethnicity_other = character(),
    income_value = numeric(),
    income_currency = character(),
    neuro_screening = logical(),
    neuro_ms = logical(),
    neuro_epilepsy = logical(),
    neuro_tbi = logical(),
    neuro_headache = logical(),
    neuro_parkinsons = logical(),
    neuro_stroke = logical(),
    neuro_other = character(),
    mh_screening = logical(),
    mh_mdd = logical(),
    mh_bpd = logical(),
    mh_gad = logical(),
    mh_ocd = logical(),
    mh_autism = logical(),
    mh_other = character(),
    device = character()
  )

  # Process each participant's intake data
  for (i in seq_along(index_intake)) {
    idx <- index_intake[i]
    intake <- raw_dat[[idx]]

    # Extract participant ID
    subj_id <- extract_id(intake)
    if (is.na(subj_id)) {
      warning("Skipping intake entry ", i, " - no valid participant ID found")
      next
    }

    # --- Basic Demographics ---

    # Consent: prioritise intake, fallback to screening if available
    consent_val <- if ("consent_confirmation" %in% names(intake)) {
      intake$consent_confirmation[1]
    } else if (has_screening_data && subj_id %in% demographics_df$prolific_id) {
      demographics_df$consent_confirmation[demographics_df$prolific_id == subj_id][1]
    } else {
      NA_character_
    }

    # Group: extract from intake or derive from screening
    group_val <- if ("group" %in% names(intake)) {
      intake$group[1]
    } else if (has_screening_data && subj_id %in% demographics_df$prolific_id) {
      screening_response <- demographics_df$screening[
        demographics_df$prolific_id == subj_id
      ][1]
      if (!is.na(screening_response)) {
        if (screening_response %in% c(0, 1)) {
          "control"
        } else if (screening_response %in% c(3, 4)) {
          "addiction"
        } else {
          NA_character_
        }
      } else {
        NA_character_
      }
    } else {
      NA_character_
    }

    # Order (if exists)
    order_val <- if ("condition" %in% names(intake)) {
      intake$condition[1]
    } else {
      NA_character_
    }

    # Age: prioritise intake, fallback to screening
    age_val <- if ("age" %in% names(intake)) {
      as.numeric(intake$age[1])
    } else if (has_screening_data && subj_id %in% demographics_df$prolific_id) {
      as.numeric(demographics_df$age[demographics_df$prolific_id == subj_id][1])
    } else {
      NA_real_
    }

    # Gender: prioritise intake, fallback to screening
    gender_val <- if ("gender" %in% names(intake)) {
      intake$gender[1]
    } else if (has_screening_data && subj_id %in% demographics_df$prolific_id) {
      demographics_df$gender[demographics_df$prolific_id == subj_id][1]
    } else {
      NA_character_
    }

    gender_other_val <- if ("gender_other" %in% names(intake) && !is.null(intake$gender_other)) {
      intake$gender_other[1]
    } else {
      NA_character_
    }

    # Ethnicity
    ethnicity_val <- if ("ethnicity" %in% names(intake)) {
      intake$ethnicity[1]
    } else {
      NA_character_
    }
    ethnicity_other_val <- if ("ethnicity_other" %in% names(intake) &&
      !is.null(intake$ethnicity_other)) {
      intake$ethnicity_other[1]
    } else {
      NA_character_
    }

    # Income
    income_value_val <- if ("income_value" %in% names(intake)) {
      as.numeric(intake$income_value[1])
    } else {
      NA_real_
    }
    income_currency_val <- if ("income_currency" %in% names(intake)) {
      intake$income_currency[1]
    } else {
      NA_character_
    }

    # --- Neurological Conditions ---

    if ("neuro_screen" %in% names(intake) &&
      !is.na(intake$neuro_screen[1]) && intake$neuro_screen[1] == 1) {
      neuro_conditions <- if ("neuro_conditions" %in% names(intake)) {
        unlist(intake$neuro_conditions[1])
      } else {
        character(0)
      }

      neuro_screening_val <- TRUE
      neuro_ms_val <- "ms" %in% neuro_conditions
      neuro_epilepsy_val <- "epilepsy" %in% neuro_conditions
      neuro_tbi_val <- "tbi" %in% neuro_conditions
      neuro_headache_val <- "headache" %in% neuro_conditions
      neuro_parkinsons_val <- "parkinsons" %in% neuro_conditions
      neuro_stroke_val <- "stroke" %in% neuro_conditions
      neuro_other_val <- if ("other" %in% neuro_conditions && "neuro_other" %in% names(intake)) {
        intake$neuro_other[1]
      } else {
        NA_character_
      }
    } else {
      neuro_screening_val <- FALSE
      neuro_ms_val <- FALSE
      neuro_epilepsy_val <- FALSE
      neuro_tbi_val <- FALSE
      neuro_headache_val <- FALSE
      neuro_parkinsons_val <- FALSE
      neuro_stroke_val <- FALSE
      neuro_other_val <- NA_character_
    }

    # --- Mental Health Conditions ---

    if ("mh_screen" %in% names(intake) &&
      !is.na(intake$mh_screen[1]) && intake$mh_screen[1] == 1) {
      mh_conditions <- if ("mh_conditions" %in% names(intake)) {
        unlist(intake$mh_conditions[1])
      } else {
        character(0)
      }

      mh_screening_val <- TRUE
      mh_mdd_val <- "mdd" %in% mh_conditions
      mh_bpd_val <- "bpd" %in% mh_conditions
      mh_gad_val <- "gad" %in% mh_conditions
      mh_ocd_val <- "ocd" %in% mh_conditions
      mh_autism_val <- "autism" %in% mh_conditions
      mh_other_val <- if ("other" %in% mh_conditions &&
        "mh_other" %in% names(intake)) {
        intake$mh_other[1]
      } else {
        NA_character_
      }
    } else {
      mh_screening_val <- FALSE
      mh_mdd_val <- FALSE
      mh_bpd_val <- FALSE
      mh_gad_val <- FALSE
      mh_ocd_val <- FALSE
      mh_autism_val <- FALSE
      mh_other_val <- NA_character_
    }

    # Device type
    device_val <- if ("device_type" %in% names(intake)) {
      intake$device_type[1]
    } else {
      NA_character_
    }

    #### Add extracted participant data as new row --------------------------------------------------------------------------

    intake_df <- intake_df |>
      add_row(
        subj_id = subj_id,
        consent = consent_val,
        group = group_val,
        order = order_val,
        age = age_val,
        gender = gender_val,
        gender_other = gender_other_val,
        ethnicity = ethnicity_val,
        ethnicity_other = ethnicity_other_val,
        income_value = income_value_val,
        income_currency = income_currency_val,
        neuro_screening = neuro_screening_val,
        neuro_ms = neuro_ms_val,
        neuro_epilepsy = neuro_epilepsy_val,
        neuro_tbi = neuro_tbi_val,
        neuro_headache = neuro_headache_val,
        neuro_parkinsons = neuro_parkinsons_val,
        neuro_stroke = neuro_stroke_val,
        neuro_other = neuro_other_val,
        mh_screening = mh_screening_val,
        mh_mdd = mh_mdd_val,
        mh_bpd = mh_bpd_val,
        mh_gad = mh_gad_val,
        mh_ocd = mh_ocd_val,
        mh_autism = mh_autism_val,
        mh_other = mh_other_val,
        device = device_val
      )
  }

  # Report extraction results
  cat("\nIntake extraction summary:\n")
  cat("- Total participants:", nrow(intake_df), "\n")
  cat(
    "- Participants with complete demographics:",
    sum(!is.na(intake_df$age) & !is.na(intake_df$gender)), "\n"
  )


  ## TASK DATA ==============================================================================================================

  if (task_dat) {
    res_comp_2 <- c(index_calibration, index_baseline, index_devalued)

    comp_2_df <- data.frame(matrix(ncol = 14, nrow = 0))
    comp_2_meta_df <- data.frame(matrix(ncol = 5, nrow = 0))

    for (comp_i in seq_along(res_comp_2)) {
      task <- raw_dat[[res_comp_2[comp_i]]]

      # Detect & correct double clicking error
      if (length(task$rt) != length(task$phase)) {
        warning(paste("Double clicking error in subject", extract_id(task), sep = " "))

        # Changed to < 5 (instead of <= 5)
        del <- which(abs(c(diff(task$rt), 6)) < 5 & task$rt != 999)
        task$rt <- task$rt[-del]
        task$choice <- task$choice[-del]
      }

      # Adjust variables if subject had to re-do calibration
      if (task$clicks |> length() > task$rt |> length()) {
        for (i in 1:(abs(task$clicks |> length() - task$rt |> length()) / 2)) {
          if (mean(task$clicks[(i + 1):(i + 2)]) < 7) {
            task[4:13]$phase %<>% append(c("calibration", "calibration"), after = i + 2)
            task[4:13]$trial %<>% append(2:3, after = i + 2)
            task[4:13]$points %<>% append(c(0, 0), after = i + 2)


            task[4:13][c(
              "trialType", "offerEffort", "offerReward",
              "choice", "rt", "goalClicks"
            )] %<>%
              map(\(x) append(x, c(999, 999), after = i + 2))
          } else {
            stop(paste("Check task data for subject with game ID", task$subjID, sep = " "))
          }
        }
      }

      # Adjust variables if subject has extra choice trials
      is_game_task <- grepl("baseline", task$start[1], fixed = TRUE) ||
        grepl("devalued", task$start[1], fixed = TRUE)

      if (length(task$choice) > length(task$phase)) {
        n_extra <- length(task$choice) - length(task$phase)

        if (!is_game_task) {
          # If it is calibration component - append quiz trials
          cat("Subject with extra choice trials (quiz):", task$prolific_id, "\n")

          for (i in 1:n_extra) {
            # Append extra trial at the end
            task[4:13]$phase %<>% append("quiz")
            task[4:13]$trial %<>% append(NA)
            task[4:13]$trialType %<>% append(NA)
            task[4:13]$points %<>% append(0)

            task[4:13][c("offerEffort", "offerReward", "clicks", "rt", "goalClicks")] %<>%
              map(~ append(.x, 999))
          }
        } else {
          task$choice <- head(task$choice, -n_extra)
          task$rt <- head(task$rt, -n_extra)
        }
      }


      #   } else {
      #     # If it is game component - remove mismatched or extra choice entries
      #     cat("Subject with extra choice trials (game):", task$prolific_id, "\n")
      #
      #     min_len <- min(length(task$choice), length(task$goalClicks))
      #     choice_vec <- task$choice[1:min_len]
      #     goalclicks_vec <- task$goalClicks[1:min_len]
      #
      #     # Find mismatched indices:
      #     # - choice is 0 but goalClicks is not 999 (should be 999 when rejected)
      #     # - choice is 1 but goalClicks is 999 (should be a value when accepted)
      #     mismatched_idx <- which(
      #       (choice_vec %in% c(0, 1)) &
      #         ((choice_vec == 0 & goalclicks_vec != 999) | (choice_vec == 1 & goalclicks_vec == 999))
      #     )
      #
      #     if(length(mismatched_idx) > 0) {
      #       cat("Removing", length(mismatched_idx), "mismatched choice entries at positions:",
      #           paste(mismatched_idx, collapse = ", "), "\n")
      #       cat("Choice values at those positions:", paste(task$choice[mismatched_idx], collapse = ", "), "\n")
      #       cat("Goal clicks values at those positions:", paste(task$goalClicks[mismatched_idx], collapse = ", "), "\n")
      #
      #       # Remove mismatched entries from choice and rt
      #       task$choice <- task$choice[-mismatched_idx]
      #       task$rt <- task$rt[-mismatched_idx]
      #     } else {
      #       # If no mismatches found, remove last entry
      #       cat("No mismatches found - removing last", n_extra, "choice(s)\n")
      #       task$choice <- head(task$choice, -n_extra)
      #       task$rt <- head(task$rt, -n_extra)
      #     }
      #   }
      # }

      # Print number of calibration, practice & quiz trials
      cat("Calibration trials:", sum(task$phase == "calibration"), "\n")
      cat("Practice trials:", sum(task$phase == "practice"), "\n")
      cat("Quiz trials:", sum(task$phase == "quiz") / 6, "\n")

      task_meta_data <- data.frame(
        # subj_id = id_shuffle(unique(raw_dat[[res_comp_2[comp_i]-1]]$prolific_id)),
        subj_id = raw_dat[[res_comp_2[comp_i] - 1]]$prolific_id,
        game_id = task$subjID,
        start_time = as_datetime(task$date[1]),
        end_time = as_datetime(task$date[2]),
        task_condition = NA,
        clicking_calibration =
          mean(task$clicks[task$phase == "calibration"][2:3])
      )

      task_data <- data.frame(
        # subj_id = rep(id_shuffle(unique(raw_dat[[res_comp_2[comp_i]-1]]$prolific_id)), length(task$phase)),
        subj_id = rep(raw_dat[[res_comp_2[comp_i] - 1]]$prolific_id, length(task$phase)),
        game_id = rep(task$subjID, length(task$phase)),
        start_time = rep(task$date[1], length(task$phase)),
        end_time = rep(task$date[2], length(task$phase)),
        task_condition = rep(NA, length(task$phase)),
        phase = task$phase,
        trial = task$trial,
        trialType = task$trialType,
        offerEffort = task$offerEffort,
        offerReward = task$offerReward,
        choice = task$choice,
        rt = task$rt,
        clicks = task$clicks,
        goalClicks = task$goalClicks,
        points = ifelse(task$phase == "calibration", task$points, task$points[-1]),
        clicking_calibration = rep(
          mean(task$clicks[task$phase == "calibration"][2:3]),
          length(task$phase)
        )
      )

      # # Define task condition based on experiment order
      # if (grepl("baseline", task$start[1], fixed = TRUE)) {
      #   task_data$task_condition[task$phase == "game"] <- "baseline"
      # } else if (grepl("devalued", task$start[1], fixed = TRUE)) {
      #   task_data$task_condition[task$phase == "game"] <- "devalued"
      # }
      #
      # task_data$task_condition[is.na(task_data$task_condition)] <- "NA"
      #
      # # Task condition for meta data
      # if (any(task_data$task_condition == "baseline")) {
      #   task_meta_data$task_condition <- "baseline"
      # } else if (any(task_data$task_condition == "devalued")) {
      #   task_meta_data$task_condition <- "devalued"
      # } else {
      #   task_meta_data$task_condition <- "calibration"
      # }

      # Define task condition based on experiment order: game data
      # NOTE: for participants in DB order, condition tag was reversed in saving data

      # Extract order from intake
      condition_order <- intake_df$order[intake_df$subj_id == task_data$subj_id[1]]

      if (condition_order == "DB") {
        # Swap baseline/devalued for DB participants
        if (grepl("baseline", task$start[1], fixed = TRUE)) {
          task_data$task_condition[task$phase == "game"] <- "devalued"
        } else if (grepl("devalued", task$start[1], fixed = TRUE)) {
          task_data$task_condition[task$phase == "game"] <- "baseline"
        }
      } else {
        # Standard assignment for BD participants
        if (grepl("baseline", task$start[1], fixed = TRUE)) {
          task_data$task_condition[task$phase == "game"] <- "baseline"
        } else if (grepl("devalued", task$start[1], fixed = TRUE)) {
          task_data$task_condition[task$phase == "game"] <- "devalued"
        }
      }

      task_data$task_condition[is.na(task_data$task_condition)] <- "NA"

      # Define task condition for game meta data
      if (any(task_data$task_condition == "baseline")) {
        task_meta_data$task_condition <- "baseline"
      } else if (any(task_data$task_condition == "devalued")) {
        task_meta_data$task_condition <- "devalued"
      } else {
        task_meta_data$task_condition <- "calibration"
      }

      comp_2_df <- rbind(comp_2_df, task_data)
      comp_2_meta_df <- rbind(comp_2_meta_df, task_meta_data)

      # Add clicking calibration to every row
      comp_2_df <- comp_2_df |>
        group_by(subj_id) |>
        mutate(clicking_calibration = mean(clicks[phase == "calibration"][2:3], na.rm = TRUE)) |>
        ungroup()

      comp_2_meta_df <- comp_2_meta_df |>
        group_by(subj_id) |>
        mutate(clicking_calibration = clicking_calibration[task_condition == "calibration"][1]) |>
        ungroup()

      if (modelling_dat) {
        modelling_df <- list()

        # added task_condition (5)
        data_modelling <- comp_2_df[
          comp_2_df$phase == "game",
          c(1, 2, 5, 7, 9, 10, 11)
        ]

        data_modelling$offerEffort <- standardization(
          data_modelling$offerEffort, 0, unique(data_modelling$offerEffort)
        )

        data_modelling$offerReward <- standardization(
          data_modelling$offerReward, 0, unique(data_modelling$offerReward)
        )

        colnames(data_modelling) <- c(
          "subjID", "gameID", "task_condition",
          "trial", "effort_a", "amount_a", "choice"
        )

        data_modelling <- cbind(
          data_modelling,
          "effort_b" = rep(0, dim(data_modelling)[1]),
          "amount_b" = rep(0, dim(data_modelling)[1])
        )

        modelling_df <- data_modelling
      }
    }
  }


  ## TRIAL SESSION DATA =====================================================================================================

  if (task_dat) {
    cat("Extracting trial1 data from", length(index_trial1), "participants\n")

    # Initialize trial dataframe
    comp_3_df <- tibble(
      subj_id = character(),
      screentime_before = character(),
      screentime_before_min = numeric()
    )

    # Process each participant's trial1 data
    for (i in seq_along(index_trial1)) {
      idx <- index_trial1[i]
      trial <- raw_dat[[idx]]

      # Extract participant ID
      subj_id <- extract_id(trial)
      if (is.na(subj_id)) {
        warning("Skipping trial1 entry ", i, " - no valid participant ID found")
        next
      }

      # Extract screentime data
      screentime_before_val <- if ("screentime_before" %in% names(trial) &&
        !is.null(trial$screentime_before[1])) {
        trial$screentime_before[1]
      } else {
        NA_character_
      }

      # Convert screentime to minutes
      screentime_before_min_val <- if (!is.na(screentime_before_val)) {
        tryCatch(
          {
            hours_minutes <- strsplit(screentime_before_val, ":")[[1]]
            if (length(hours_minutes) >= 2) {
              hours <- as.numeric(hours_minutes[1])
              minutes <- as.numeric(hours_minutes[2])
              hours * 60 + minutes
            } else {
              NA_real_
            }
          },
          error = function(e) {
            warning(
              "Failed to parse screentime for subject ",
              subj_id, ": ", screentime_before_val
            )
            NA_real_
          }
        )
      } else {
        NA_real_
      }

      # Add row to dataframe
      comp_3_df <- comp_3_df |>
        add_row(
          subj_id = subj_id,
          screentime_before = screentime_before_val,
          screentime_before_min = screentime_before_min_val
        )
    }

    cat("\nTrial1 extraction summary:\n")
    cat("- Total participants:", nrow(comp_3_df), "\n")
    cat(
      "- Participants with screentime data:",
      sum(!is.na(comp_3_df$screentime_before_min)), "\n"
    )
  }

  ## MAIN SESSION BASELINE ==================================================================================================

  cat(
    "Extracting baseline main session data from",
    length(index_baseline_use), "participants\n"
  )

  # Initialize baseline main session dataframe
  comp_4_df <- tibble(
    subj_id = character(),
    task_compliance_b = character(),
    task_compliance_text_b = character(),
    task_strategy_b = character(),
    task_strategy_text_b = character(),
    liking_1_b = numeric(),
    liking_2_b = numeric(),
    liking_b = numeric(),
    screentime_after_b = character(),
    screentime_after_b_min = numeric()
  )

  # Process each participant's baseline main session data
  for (i in seq_along(index_baseline_use)) {
    idx <- index_baseline_use[i]
    main_b <- raw_dat[[idx]]

    # Extract participant ID
    subj_id <- extract_id(main_b)
    if (is.na(subj_id)) {
      warning(
        "Skipping baseline main session entry ", i,
        " - no valid participant ID found"
      )
      next
    }

    # Task questions
    task_compliance_b_val <- if ("task_question1" %in% names(main_b)) {
      main_b$task_question1[1]
    } else {
      NA_character_
    }

    task_compliance_text_b_val <- if ("task_explanation1" %in% names(main_b) &&
      !is.null(main_b$task_explanation1)) {
      main_b$task_explanation1[1]
    } else {
      NA_character_
    }

    task_strategy_b_val <- if ("task_question2" %in% names(main_b)) {
      main_b$task_question2[1]
    } else {
      NA_character_
    }

    task_strategy_text_b_val <- if ("task_explanation2" %in% names(main_b) &&
      !is.null(main_b$task_explanation2)) {
      main_b$task_explanation2[1]
    } else {
      NA_character_
    }

    # Liking measures
    liking_1_b_val <- if ("liking_during" %in% names(main_b)) {
      as.numeric(main_b$liking_during[1])
    } else {
      NA_real_
    }

    liking_2_b_val <- if ("liking_after" %in% names(main_b)) {
      as.numeric(main_b$liking_after[1])
    } else {
      NA_real_
    }

    liking_b_val <- mean(c(liking_1_b_val, liking_2_b_val), na.rm = TRUE)

    # Screen time
    screentime_after_b_val <- if ("screentime_after_b" %in% names(main_b) &&
      !is.null(main_b$screentime_after_b[1])) {
      main_b$screentime_after_b[1]
    } else {
      NA_character_
    }

    # Convert screentime to minutes
    screentime_after_b_min_val <- if (!is.na(screentime_after_b_val)) {
      tryCatch(
        {
          hours_minutes <- strsplit(screentime_after_b_val, ":")[[1]]
          if (length(hours_minutes) >= 2) {
            hours <- as.numeric(hours_minutes[1])
            minutes <- as.numeric(hours_minutes[2])
            hours * 60 + minutes
          } else {
            NA_real_
          }
        },
        error = function(e) {
          warning(
            "Failed to parse screentime for subject ",
            subj_id, ": ", screentime_after_b_val
          )
          NA_real_
        }
      )
    } else {
      NA_real_
    }

    # Add row to dataframe
    comp_4_df <- comp_4_df |>
      add_row(
        subj_id = subj_id,
        task_compliance_b = task_compliance_b_val,
        task_compliance_text_b = task_compliance_text_b_val,
        task_strategy_b = task_strategy_b_val,
        task_strategy_text_b = task_strategy_text_b_val,
        liking_1_b = liking_1_b_val,
        liking_2_b = liking_2_b_val,
        liking_b = liking_b_val,
        screentime_after_b = screentime_after_b_val,
        screentime_after_b_min = screentime_after_b_min_val
      )
  }

  cat("\nBaseline main session extraction summary:\n")
  cat("- Total participants:", nrow(comp_4_df), "\n")
  cat("- Participants with liking data:", sum(!is.na(comp_4_df$liking_b)), "\n")


  ## MAIN SESSION DEVALUED ==================================================================================================

  cat(
    "Extracting devalued main session data from",
    length(index_devalued_use), "participants\n"
  )

  # Initialize devalued main session dataframe
  comp_5_df <- tibble(
    subj_id = character(),
    task_compliance_d = character(),
    task_compliance_text_d = character(),
    task_strategy_d = character(),
    task_strategy_text_d = character(),
    liking_1_d = numeric(),
    liking_2_d = numeric(),
    liking_d = numeric(),
    screentime_after_d = character(),
    screentime_after_d_min = numeric()
  )

  # Process each participant's devalued main session data
  for (i in seq_along(index_devalued_use)) {
    idx <- index_devalued_use[i]
    main_d <- raw_dat[[idx]]

    # Extract participant ID
    subj_id <- extract_id(main_d)
    if (is.na(subj_id)) {
      warning(
        "Skipping devalued main session entry ",
        i, " - no valid participant ID found"
      )
      next
    }

    # Task questions
    task_compliance_d_val <- if ("task_question1" %in% names(main_d)) {
      main_d$task_question1[1]
    } else {
      NA_character_
    }

    task_compliance_text_d_val <- if ("task_explanation1" %in% names(main_d) &&
      !is.null(main_d$task_explanation1)) {
      main_d$task_explanation1[1]
    } else {
      NA_character_
    }

    task_strategy_d_val <- if ("task_question2" %in% names(main_d)) {
      main_d$task_question2[1]
    } else {
      NA_character_
    }

    task_strategy_text_d_val <- if ("task_explanation2" %in% names(main_d) &&
      !is.null(main_d$task_explanation2)) {
      main_d$task_explanation2[1]
    } else {
      NA_character_
    }

    # Liking measures
    liking_1_d_val <- if ("liking_during" %in% names(main_d)) {
      as.numeric(main_d$liking_during[1])
    } else {
      NA_real_
    }

    liking_2_d_val <- if ("liking_after" %in% names(main_d)) {
      as.numeric(main_d$liking_after[1])
    } else {
      NA_real_
    }

    liking_d_val <- mean(c(liking_1_d_val, liking_2_d_val), na.rm = TRUE)

    # Screen time
    screentime_after_d_val <- if ("screentime_after_d" %in% names(main_d) &&
      !is.null(main_d$screentime_after_d[1])) {
      main_d$screentime_after_d[1]
    } else {
      NA_character_
    }

    # Convert screentime to minutes
    screentime_after_d_min_val <- if (!is.na(screentime_after_d_val)) {
      tryCatch(
        {
          hours_minutes <- strsplit(screentime_after_d_val, ":")[[1]]
          if (length(hours_minutes) >= 2) {
            hours <- as.numeric(hours_minutes[1])
            minutes <- as.numeric(hours_minutes[2])
            hours * 60 + minutes
          } else {
            NA_real_
          }
        },
        error = function(e) {
          warning(
            "Failed to parse screentime for subject ",
            subj_id, ": ", screentime_after_d_val
          )
          NA_real_
        }
      )
    } else {
      NA_real_
    }

    # Add row to dataframe
    comp_5_df <- comp_5_df |>
      add_row(
        subj_id = subj_id,
        task_compliance_d = task_compliance_d_val,
        task_compliance_text_d = task_compliance_text_d_val,
        task_strategy_d = task_strategy_d_val,
        task_strategy_text_d = task_strategy_text_d_val,
        liking_1_d = liking_1_d_val,
        liking_2_d = liking_2_d_val,
        liking_d = liking_d_val,
        screentime_after_d = screentime_after_d_val,
        screentime_after_d_min = screentime_after_d_min_val
      )
  }

  cat("\nDevalued main session extraction summary:\n")
  cat("- Total participants:", nrow(comp_5_df), "\n")
  cat("- Participants with liking data:", sum(!is.na(comp_5_df$liking_d)), "\n")

  ## QUESTIONNAIRES =========================================================================================================

  if (questionnaire_dat) {
    cat(
      "Extracting questionnaire data from",
      length(index_questionnaires), "participants\n"
    )

    # Initialize questionnaire dataframe
    comp_6_df <- data.frame(matrix(ncol = 0, nrow = 0))

    # Process each participant's questionnaire data
    for (comp_i in seq_along(index_questionnaires)) {
      idx <- index_questionnaires[comp_i]
      questionnaires <- raw_dat[[idx]]

      # Extract participant ID
      subj_id <- extract_id(questionnaires)
      if (is.na(subj_id)) {
        warning(
          "Skipping questionnaire entry ",
          comp_i, " - no valid participant ID found"
        )
        next
      }

      comp_6_df[comp_i, "subj_id"] <- subj_id

      # --- BSMAS (Bergen Social Media Addiction Scale: 1 to 5) ---

      bsmas_cols <- grep("^bsmas_\\d{2}$", names(questionnaires), value = TRUE)

      if (length(bsmas_cols) > 0) {
        bsmas <- as.data.frame(lapply(questionnaires[bsmas_cols], function(x) as.numeric(x) + 1))

        for (col in bsmas_cols) {
          comp_6_df[comp_i, col] <- bsmas[[col]]
        }

        comp_6_df[comp_i, "bsmas_sum"] <- sum(unlist(bsmas), na.rm = TRUE)
      }

      bsmas_rt_cols <- grep("^bsmas_\\d{2}_rt$", names(questionnaires), value = TRUE)

      if (length(bsmas_rt_cols) > 0) {
        for (col in bsmas_rt_cols) {
          comp_6_df[comp_i, col] <- questionnaires[[col]]
        }
      }

      # --- PHQ-8 (Patient Health Questionnaire: 0 to 3) ---

      phq8_cols <- grep("^phq8_\\d{2}$", names(questionnaires), value = TRUE)
      if (length(phq8_cols) > 0) {
        phq8 <- as.data.frame(lapply(questionnaires[phq8_cols], function(x) as.numeric(x)))

        for (col in phq8_cols) {
          comp_6_df[comp_i, col] <- phq8[[col]]
        }

        comp_6_df[comp_i, "phq8_sum"] <- sum(unlist(phq8), na.rm = TRUE)
      }

      phq8_rt_cols <- grep("^phq8_\\d{2}_rt$", names(questionnaires),
        value = TRUE
      )
      if (length(phq8_rt_cols) > 0) {
        for (col in phq8_rt_cols) {
          comp_6_df[comp_i, col] <- questionnaires[[col]]
        }
      }

      # --- GAD-7 (Generalized Anxiety Disorder: 0 to 3) ---

      gad7_cols <- grep("^gad7_\\d{2}$", names(questionnaires), value = TRUE)
      if (length(gad7_cols) > 0) {
        gad7 <- as.data.frame(lapply(questionnaires[gad7_cols], function(x) as.numeric(x)))

        for (col in gad7_cols) {
          comp_6_df[comp_i, col] <- gad7[[col]]
        }

        comp_6_df[comp_i, "gad7_sum"] <- sum(unlist(gad7), na.rm = TRUE)
      }

      gad7_rt_cols <- grep("^gad7_\\d{2}_rt$", names(questionnaires), value = TRUE)

      if (length(gad7_rt_cols) > 0) {
        for (col in gad7_rt_cols) {
          comp_6_df[comp_i, col] <- questionnaires[[col]]
        }
      }

      # --- BISR-21 (Brief Internet-related Self-Regulation: 1 to 4) ---

      bisr21_cols <- grep("^bisr21_\\d{2}_adj$", names(questionnaires),
        value = TRUE
      )
      if (length(bisr21_cols) > 0) {
        bisr21 <- as.data.frame(lapply(questionnaires[bisr21_cols], function(x) as.numeric(x)))

        for (col in bisr21_cols) {
          comp_6_df[comp_i, col] <- bisr21[[col]]
        }

        comp_6_df[comp_i, "bisr21_sum"] <- sum(unlist(bisr21), na.rm = TRUE)
      }

      bisr21_rt_cols <- grep("^bisr21_\\d{2}_rt$", names(questionnaires), value = TRUE)

      if (length(bisr21_rt_cols) > 0) {
        for (col in bisr21_rt_cols) {
          comp_6_df[comp_i, col] <- questionnaires[[col]]
        }
      }

      # --- WSAS-5 (Work and Social Adjustment Scale: 0 to 8) ---

      wsas5_cols <- grep("^wsas5_\\d{2}$", names(questionnaires),
        value = TRUE
      )
      if (length(wsas5_cols) > 0) {
        wsas5 <- as.data.frame(lapply(questionnaires[wsas5_cols], function(x) as.numeric(x)))

        for (col in wsas5_cols) {
          comp_6_df[comp_i, col] <- wsas5[[col]]
        }

        comp_6_df[comp_i, "wsas5_sum"] <- sum(unlist(wsas5), na.rm = TRUE)
      }

      wsas5_rt_cols <- grep("^wsas5_\\d{2}_rt$", names(questionnaires), value = TRUE)
      if (length(wsas5_rt_cols) > 0) {
        for (col in wsas5_rt_cols) {
          comp_6_df[comp_i, col] <- questionnaires[[col]]
        }
      }

      # --- Compliance Questions ---

      comp_6_df[comp_i, "tiktok_compliance_confirmation"] <-
        if (!is.null(questionnaires$tiktok_compliance_confirmation)) {
          questionnaires$tiktok_compliance_confirmation
        } else {
          NA
        }

      comp_6_df[comp_i, "tiktok_non_compliance_session"] <-
        if (!is.null(questionnaires$tiktok_non_compliance_session)) {
          paste(unlist(questionnaires$tiktok_non_compliance_session),
            collapse = ", "
          )
        } else {
          NA
        }

      comp_6_df[comp_i, "tiktok_non_compliance_explanation"] <-
        if (!is.null(questionnaires$tiktok_non_compliance_explanation)) {
          questionnaires$tiktok_non_compliance_explanation
        } else {
          NA
        }

      comp_6_df[comp_i, "dev_compliance_confirmation"] <-
        if (!is.null(questionnaires$dev_compliance_confirmation)) {
          questionnaires$dev_compliance_confirmation
        } else {
          NA
        }

      comp_6_df[comp_i, "dev_non_compliance_session"] <-
        if (!is.null(questionnaires$dev_non_compliance_session)) {
          paste(unlist(questionnaires$dev_non_compliance_session),
            collapse = ", "
          )
        } else {
          NA
        }

      comp_6_df[comp_i, "dev_non_compliance_explanation"] <-
        if (!is.null(questionnaires$dev_non_compliance_explanation)) {
          questionnaires$dev_non_compliance_explanation
        } else {
          NA
        }

      # --- Final Comment ---

      comp_6_df[comp_i, "final_comment"] <-
        if (!is.null(questionnaires$final_comment)) {
          questionnaires$final_comment
        } else {
          NA
        }

      # --- Catch/Attention Check Questions ---

      catch_easy_cols <- grep("^catch_easy\\d+$", names(questionnaires), value = TRUE)
      catch_hard_cols <- grep("^catch_hard\\d+$", names(questionnaires), value = TRUE)

      if (length(catch_easy_cols) > 0) {
        catch_easy <- as.data.frame(lapply(
          questionnaires[catch_easy_cols],
          function(x) as.numeric(x)
        ))

        for (col in catch_easy_cols) {
          comp_6_df[comp_i, col] <- catch_easy[[col]]
        }
      }

      if (length(catch_hard_cols) > 0) {
        catch_hard <- as.data.frame(lapply(
          questionnaires[catch_hard_cols],
          function(x) as.numeric(x)
        ))

        for (col in catch_hard_cols) {
          comp_6_df[comp_i, col] <- catch_hard[[col]]
        }
      }

      # Evaluate catch question performance
      catch_expected_easy <- c(catch_easy1 = 3, catch_easy2 = 0)
      catch_expected_hard <- c(catch_hard1 = 0, catch_hard2 = 0)

      # Pass if both easy questions correct
      if (length(catch_easy_cols) > 0 && exists("catch_easy")) {
        catch_pass_easy <- all(mapply(
          `==`, unlist(catch_easy),
          catch_expected_easy[names(catch_easy)]
        ), na.rm = TRUE)
      } else {
        catch_pass_easy <- NA
      }

      # Pass if at least one hard question correct
      if (length(catch_hard_cols) > 0 && exists("catch_hard")) {
        catch_pass_hard <- any(mapply(
          `==`, unlist(catch_hard),
          catch_expected_hard[names(catch_hard)]
        ), na.rm = TRUE)
      } else {
        catch_pass_hard <- NA
      }

      comp_6_df[comp_i, "catch_pass_easy"] <- catch_pass_easy
      comp_6_df[comp_i, "catch_pass_hard"] <- catch_pass_hard
    }

    comp_6_df <- as.data.frame(comp_6_df)

    cat("\nQuestionnaire extraction summary:\n")
    cat("- Total participants:", nrow(comp_6_df), "\n")
    cat(
      "- Participants passing easy catch questions:",
      sum(comp_6_df$catch_pass_easy, na.rm = TRUE), "\n"
    )
    cat(
      "- Participants passing hard catch questions:",
      sum(comp_6_df$catch_pass_hard, na.rm = TRUE), "\n"
    )
  }

  #### Combine TikTok Use Components (3,4,5) --------------------------------------------------------------------------------

  if (tiktok_dat && exists("comp_3_df") && exists("comp_4_df") && exists("comp_5_df")) {
    comp_345_df <- merge(comp_3_df, comp_4_df, by = "subj_id", all = TRUE)
    comp_345_df <- merge(comp_345_df, comp_5_df, by = "subj_id", all = TRUE)

    # Reorder columns for clarity
    comp_345_df <- comp_345_df[, c(
      "subj_id",
      "task_compliance_b", "task_compliance_text_b",
      "task_strategy_b", "task_strategy_text_b",
      "task_compliance_d", "task_compliance_text_d",
      "task_strategy_d", "task_strategy_text_d",
      "liking_1_b", "liking_2_b", "liking_b",
      "liking_1_d", "liking_2_d", "liking_d",
      "screentime_before", "screentime_before_min",
      "screentime_after_b", "screentime_after_b_min",
      "screentime_after_d", "screentime_after_d_min"
    )]

    cat("\nTikTok use data combined successfully\n")
  }

  ## OUTPUT =================================================================================================================

  results <- list()

  if (demographic_dat && exists("intake_df")) {
    results <- append(results, list("demographics" = intake_df))
  }

  if (task_dat && exists("comp_2_df")) {
    results <- append(results, list("game" = comp_2_df))
  }

  if (task_dat && exists("comp_2_meta_df")) {
    results <- append(results, list("game_meta" = comp_2_meta_df))
  }

  if (modelling_dat && exists("modelling_df")) {
    results <- append(results, list("modelling_data" = modelling_df))
  }

  if (tiktok_dat && exists("comp_345_df")) {
    results <- append(results, list("tiktok_use" = comp_345_df))
  }

  if (questionnaire_dat && exists("comp_6_df")) {
    results <- append(results, list("questionnaires" = comp_6_df))
  }

  # Add group variable to each data frame (from demographics/intake)
  if ("demographics" %in% names(results) && "group" %in% names(results$demographics)) {
    results <- lapply(results, \(df) {
      # Skip if already has group or if it's the demographics dataframe itself
      if ("group" %in% names(df) || !("subj_id" %in% names(df) || "subjID" %in% names(df))) {
        return(df)
      }

      # For modelling_data which uses subjID instead of subj_id
      if ("subjID" %in% names(df) && !"subj_id" %in% names(df)) {
        df$group <- results$demographics$group[match(df$subjID, results$demographics$subj_id)]
      }
      # For all other dataframes with subj_id
      else if ("subj_id" %in% names(df)) {
        df$group <- results$demographics$group[match(df$subj_id, results$demographics$subj_id)]
      }

      return(df)
    })

    cat("\nGroup variable added to all dataframes\n")
  }

  cat("\n=== Data extraction complete ===\n")
  cat("Returned list contains:", paste(names(results), collapse = ", "), "\n")

  return(results)
}

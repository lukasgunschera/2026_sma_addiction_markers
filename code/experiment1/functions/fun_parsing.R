## ======================================================================================================================= ##
## Script:       Parsing function
## ======================================================================================================================= ##
## Authors:      Sara Mehrhof, Lukas Gunschera
## Contact:      sara.mehrhof@gmail.com
##
## Date created: 2025-01-25
## ======================================================================================================================= ##
##
## ======================================================================================================================= ##

fun_parsing <- function(file) {
  require(sjlabelled)
  require(lubridate)
  require(jsonlite)
  require(labelled)
  require(ggplot2)
  require(ggpubr)
  library(tidyr)
  require(dplyr)

  ## Data Import ============================================================================================================

  # single file to import
  if (length(file) == 1) {
    raw_dat <- jsonlite::fromJSON(sprintf("[%s]", paste(readLines(file[1], encoding = "UTF-8", warn = F), collapse = ",")))

    # multiple files to import
  } else if (length(file) > 1) {
    #### Remove incomplete data  --------------------------------------------------------------------------------------------

    # create a vector to store all subj_ids and extract subj_id from each file
    all_subj_ids <- vector("character", length(file))

    for (i in 1:length(file)) {
      # read the file and extract subj_id
      data <- jsonlite::fromJSON(sprintf("[%s]", paste(readLines(file[i], encoding = "UTF-8", warn = F), collapse = ",")))
      # get the first non-NA subject ID in the file
      subj_id <- na.omit(data$subj_id)[1]
      all_subj_ids[i] <- as.character(subj_id)
    }

    # count occurrences of each subj_id and remove subjects without two files
    subj_counts <- table(all_subj_ids)
    complete_subjects <- names(subj_counts[subj_counts == 2])

    print(paste0("ID of incomplete subject:", names(subj_counts[subj_counts != 2])))

    # filter files to include only those from subjects with two files
    keep_files <- file[all_subj_ids %in% complete_subjects]

    # replace the original file vector with the filtered one
    file <- keep_files

    raw_dat <- list()
    for (i in 1:length(file)) {
      raw_dat[[i]] <- jsonlite::fromJSON(sprintf("[%s]", paste(readLines(file[i], encoding = "UTF-8", warn = F), collapse = ",")))
    }
  }

  ### PACKAGE 1: Demographic data -------------------------------------------------------------------------------------------

  demographics <- list()

  for (i in 1:length(raw_dat)) {
    # netflix condition in first session
    if (raw_dat[[i]]$session_id %>% na.omit() %>% unique() == "netflix_01") {
      netflix_01 <- raw_dat[[i]] %>%
        # move subject identifier to the first column
        select(subj_id, everything()) %>%
        select(subj_id:mh_conditions_other_text) %>%
        # combine subject identifier columns to add value in first row
        dplyr::mutate(subj_id = coalesce(subj_id[1], subj_id[2], subj_id[3])) %>%
        # drop redundant columns
        drop_na(consent) %>%
        # add column indicating randomization order
        add_column(randomization = "net_tik", .after = "subj_id")

      # assign imported data to demographics object
      demographics[[i]] <- netflix_01

      # tiktok condition in first session
    } else if (raw_dat[[i]]$session_id %>% na.omit() %>% unique() == "tiktok_01") {
      # select columns with demographic data
      tiktok_01 <- raw_dat[[i]] %>%
        # move subject identifier to the first column
        select(subj_id, everything()) %>%
        select(subj_id:mh_conditions_other_text) %>%
        # combine subject identifier columns to add value in first row
        dplyr::mutate(subj_id = coalesce(subj_id[1], subj_id[2], subj_id[3])) %>%
        # drop redundant columns
        drop_na(consent) %>%
        # add column indicating randomization order
        add_column(randomization = "tik_net", .after = "session_id")

      # assign imported data to demographics object
      demographics[[i]] <- tiktok_01
    }
  }

  # combine data with different conditions
  demographics <- do.call(rbind, demographics)

  ### PACKAGE 2: Task Data --------------------------------------------------------------------------------------------------

  task_meta_data_subj <- list()
  task_data_subj <- list()

  for (i in 1:length(raw_dat)) {
    # extract task meta data
    meta_data <- raw_dat[[i]] %>%
      select(c(subj_id, session_id, subjID:date)) %>%
      lapply(., unlist)


    # task id, timestamps at start and end of the task, task duration
    task_meta_data_subj[[i]] <- data.frame(
      subj_id = unique(na.omit(meta_data$subj_id)) %>% as.numeric(),
      session_id = unique(na.omit(meta_data$session_id)),
      task_id = meta_data$subjID,
      start_time = meta_data$date[seq(1, length(meta_data$date), by = 2)], # Odd-indexed times
      end_time = meta_data$date[seq(2, length(meta_data$date), by = 2)] # Even-indexed times
    ) %>%
      dplyr::mutate(
        start_time = as.POSIXct(.$start_time, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"),
        end_time = as.POSIXct(.$end_time, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
      ) %>%
      dplyr::mutate(task_duration = difftime(.$end_time, .$start_time, units = "secs"))

    # task data
    task_data_subj[[i]] <- raw_dat[[i]] %>%
      select(c(subj_id, session_id, subjID, phase:points)) %>%
      lapply(., unlist) %>%
      {
        # Find the maximum length of the vectors in the list
        max_length <- max(sapply(., length))

        # Pad shorter vectors with NA to match the maximum length
        . <- lapply(., function(x) {
          if (length(x) < max_length) {
            c(x, rep(NA, max_length - length(x)))
          } else {
            x
          }
        })

        # Replace NA values in subj_id with the first non-NA value
        first_non_na <- .$subj_id[!is.na(.$subj_id)][1]
        .$subj_id[is.na(.$subj_id)] <- first_non_na

        .
      } %>%
      as.data.frame() %>%
      dplyr::mutate(subj_id = ifelse(is.na(subj_id), unique(na.omit(subj_id)), subj_id)) %>%
      dplyr::mutate(session_id = ifelse(is.na(session_id), unique(na.omit(session_id)), session_id)) %>%
      dplyr::mutate(subj_id = as.numeric(subj_id))
  }

  task_meta_data <- bind_rows(task_meta_data_subj) %>%
    # label columns
    set_variable_labels(
      subj_id = "Participant identifier (Task Metadata)",
      session_id = "Session identifier indicating condition and administration order",
      task_id = "Task identifier",
      start_time = "Timestamp of task start (time experimenter opens Jatos link, not necessarily the true start time)",
      end_time = "Timestamp of task end",
      task_duration = "Duration of task in seconds"
    )

  task_data <- bind_rows(task_data_subj) %>%
    # label columns
    set_variable_labels(
      subj_id = "Participant identifier (Task Data)",
      session_id = "Session identifier indicating condition and administration order",
      subjID = "Game-generated identifier independent from the universal subj_id parameter",
      phase = "Task phase (calibration, practice, game)",
      trial = "Trial number",
      trialType = "Trial type identifier",
      offerEffort = "Effort level of given trial",
      offerReward = "Reward level of given trial",
      choice = "Behavioural choice (0 = reject, 1 = accept, 999 = NR)",
      rt = "Response time",
      clicks = "Clicks performed on given trial",
      goalClicks = "Number of clicks required to reach success",
      points = "Points accumulated so far"
    )

  ### PACKAGE 3: Questionnaire Data -----------------------------------------------------------------------------------------

  netflix_s_01 <- tiktok_s_01 <- netflix_s_02 <- tiktok_s_02 <- list()

  for (i in 1:length(raw_dat)) {
    # netflix session 1
    if (raw_dat[[i]]$session_id %>% na.omit() %>% unique() == "netflix_01") {
      netflix_s_01 <- append(netflix_s_01, list(raw_dat[[i]] %>%
        # select columns with questionnaire data
        select(subj_id, session_id, task_1_response:screentime_average) %>%
        .[3, ]))

      # tiktok session 1
    } else if (raw_dat[[i]]$session_id %>% na.omit() %>% unique() == "tiktok_01") {
      # select columns with questionnaire data
      tiktok_s_01 <- append(tiktok_s_01, list(raw_dat[[i]] %>%
        select(subj_id, session_id, task_1_response:screentime_average) %>%
        .[3, ]))

      # netflix session 2
    } else if (raw_dat[[i]]$session_id %>% na.omit() %>% unique() == "netflix_02") {
      # select columns with questionnaire data
      netflix_s_02 <- append(netflix_s_02, list(raw_dat[[i]] %>%
        select(subj_id, session_id, task_1_response:risq2_other) %>%
        .[3, ]))

      # tiktok session 2
    } else if (raw_dat[[i]]$session_id %>% na.omit() %>% unique() == "tiktok_02") {
      # select columns with questionnaire data
      tiktok_s_02 <- append(tiktok_s_02, list(raw_dat[[i]] %>%
        select(subj_id, session_id, task_1_response:risq2_other) %>%
        .[3, ]))
    }
  }

  # create object to store data across all questionnaires
  questionnaires <- c(netflix_s_01, tiktok_s_01, netflix_s_02, tiktok_s_02)

  # create a list to store the combined dataframes
  combined_questionnaires <- list()

  # iterate over unique subj_id values
  unique_subj_ids <- unique(unlist(lapply(questionnaires, function(df) df$subj_id)))

  for (subj in unique_subj_ids) {
    # filter data for the current subject from all dataframes
    subject_data <- Filter(function(df) nrow(df) > 0, lapply(questionnaires, function(df) df[df$subj_id == subj, ]))

    # separate data for session 1 and session 2
    session1 <- do.call(rbind, lapply(subject_data, function(df) df[df$session_id == "netflix_01" | df$session_id == "tiktok_01", ]))
    if (session1$session_id == "netflix_01") {
      session1 %<>%
        dplyr::rename(
          task_1_response_netflix    = task_1_response,
          task1_text_netflix         = task1_text,
          task_2_response_netflix    = task_2_response,
          task2_text_netflix         = task2_text,
          strap_1_netflix            = strap_1,
          strap_2_netflix            = strap_2,
          strap_3_netflix            = strap_3,
          screentime_average_netflix = screentime_average
        )
    } else if (session1$session_id == "tiktok_01") {
      session1 %<>%
        dplyr::rename(
          task_1_response_tiktok    = task_1_response,
          task1_text_tiktok         = task1_text,
          task_2_response_tiktok    = task_2_response,
          task2_text_tiktok         = task2_text,
          strap_1_tiktok            = strap_1,
          strap_2_tiktok            = strap_2,
          strap_3_tiktok            = strap_3,
          screentime_average_tiktok = screentime_average
        )
    }

    session2 <- do.call(rbind, lapply(subject_data, function(df) df[df$session_id == "tiktok_02" | df$session_id == "netflix_02", , ]))

    if (session2$session_id == "netflix_02") {
      session2 %<>%
        dplyr::rename(
          task_1_response_netflix    = task_1_response,
          task1_text_netflix         = task1_text,
          task_2_response_netflix    = task_2_response,
          task2_text_netflix         = task2_text,
          strap_1_netflix            = strap_1,
          strap_2_netflix            = strap_2,
          strap_3_netflix            = strap_3,
          screentime_average_netflix = screentime_average
        )
    } else if (session2$session_id == "tiktok_02") {
      session2 %<>%
        dplyr::rename(
          task_1_response_tiktok    = task_1_response,
          task1_text_tiktok         = task1_text,
          task_2_response_tiktok    = task_2_response,
          task2_text_tiktok         = task2_text,
          strap_1_tiktok            = strap_1,
          strap_2_tiktok            = strap_2,
          strap_3_tiktok            = strap_3,
          screentime_average_tiktok = screentime_average
        )
    }

    # add scoring for catch questions
    session2 %<>%
      dplyr::mutate(
        catch_questions_easy = case_when(catch_questions_3 != 3 | catch_questions_4 != 2 ~ "fail",
          .default = "pass"
        ),
        .after = catch_questions_4
      ) %>%
      dplyr::mutate(
        catch_questions_hard = case_when(catch_questions_1 != 1 & catch_questions_2 != 1 ~ "fail",
          .default = "pass"
        ),
        .after = catch_questions_4
      )

    # combine the columns for the same subject
    combined <- cbind(session1, session2[, !colnames(session2) %in% c("subj_id", "session")])

    # add the combined dataframe to the list
    combined_questionnaires[[length(combined_questionnaires) + 1]] <- combined
  }

  # combine all the combined dataframes into a single dataframe
  combined_questionnaires <- do.call(plyr::rbind.fill, combined_questionnaires)

  # remove session_id column names
  combined_questionnaires <- combined_questionnaires %>% select(!session_id)

  # order by subj id
  combined_questionnaires %<>% arrange(subj_id)

  ### Format Output ---------------------------------------------------------------------------------------------------------

  #### Format Demographics --------------------------------------------------------------------------------------------------
  demographics <- demographics %>%
    # convert to appropriate data types
    dplyr::mutate(
      subj_id = as.numeric(subj_id),
      randomization = as.factor(randomization),
      consent = ifelse(consent == "consent_yes", TRUE, FALSE),
      gender = as.factor(gender),
      race = as.factor(race),
      currency = as.factor(currency),
      neuro_screen = as.logical(neuro_screen),
      neuro_conditions_other_text = as.character(neuro_conditions_other_text),
      mh_screen = as.logical(mh_screen),
      mh_conditions_other_text = as.character(mh_conditions_other_text),
    ) %>%
    # rename for readability
    dplyr::rename(
      ethnicity        = race,
      income_currency  = currency,
      neuro_conditions = neuro_screen,
      mh_conditions    = mh_screen
    ) %>%
    #### Labeling -------------------------------------------------------------------------------------------------------------

    set_variable_labels(
      subj_id = "Participant identifier (Demographic Data)",
      session_id = "First session",
      randomization = "Counterbalancing order",
      consent = "Consent to participation",
      age = "Age of participant in years",
      gender = "Gender of participant",
      ethnicity = "Ethnicity of participant",
      income_currency = "Currency of income",
      income = "Income of participant",
      neuro_conditions = "Neurological conditions (yes,no)",
      neuro_conditions_ms = "Multiple sclerosis (yes,no)",
      neuro_conditions_epilepsy = "Epilepsy (yes,no)",
      neuro_conditions_tbi = "Traumatic brain injury (yes,no)",
      neuro_conditions_headache = "Headache disorder (yes,no)",
      neuro_conditions_parkinsons = "Parkinson's disease (yes,no)",
      neuro_conditions_stroke = "Stroke (yes,no)",
      neuro_conditions_none = "No neurological conditions (if screener response incorrect)",
      neuro_conditions_other = "Other conditions not listed",
      neuro_conditions_other_text = "Text input to indicate other conditions",
      mh_conditions = "Mental health or neurodevelopmental conditions (yes,no)",
      mh_conditions_mdd = "Major Depressive Disorder (yes,no)",
      mh_conditions_bipolar = "Bipolar Disorder (yes,no)",
      mh_conditions_gad = "Generalized Anxiety Disorder (yes,no)",
      mh_conditions_ocd = "Obsessive-Compulsive Disorder (yes,no)",
      mh_conditions_autism = "Autism Spectrum Disorder (yes,no)",
      mh_conditions_none = "No mental health conditions (if screener response incorrect)",
      mh_conditions_other = "Other conditions not listed",
      mh_conditions_other_text = "Text input to indicate other conditions",
    )

  #### Format Combined Questionnaires  --------------------------------------------------------------------------------------
  combined_questionnaires <- combined_questionnaires %>%
    # process data for intuitive readability
    dplyr::mutate(
      subj_id = as.integer(subj_id),
      task_1_response_tiktok = ifelse(task_1_response_tiktok == "task_one_no", TRUE, FALSE), # compliance = 'no' response
      task_1_response_netflix = ifelse(task_1_response_netflix == "task_one_no", TRUE, FALSE), # compliance = 'no' response
      task_2_response_tiktok = ifelse(task_2_response_tiktok == "task_two_yes", TRUE, FALSE), # compliance = 'no' response
      task_2_response_netflix = ifelse(task_2_response_netflix == "task_two_yes", TRUE, FALSE), # compliance = 'no' response
      task1_text_tiktok = as.character(task1_text_tiktok),
      task1_text_netflix = as.character(task1_text_netflix),
      task2_text_tiktok = as.character(task2_text_tiktok),
      task2_text_netflix = as.character(task2_text_netflix),
      watch_tiktok = ifelse(watch_tiktok == "tiktok_yes", TRUE, FALSE),
      watch_netflix = ifelse(watch_netflix == "netflix_yes", TRUE, FALSE),
      watch_tiktok_text = as.character(watch_tiktok_text),
      watch_netflix_text = as.character(watch_netflix_text),
      check_freq_1 = as.numeric(check_freq_1),
      assist_8_followup = as.character(assist_8_followup),
    ) %>%
    # rename for readability
    dplyr::rename(
      # TikTok data
      task_tiktok_compliance = task_1_response_tiktok,
      task_tiktok_strategy = task_2_response_tiktok,
      task_tiktok_compliance_text = task1_text_tiktok,
      task_tiktok_strategy_text = task2_text_tiktok,
      tiktok_checking_frequency = check_freq_1,
      tiktok_videos_watched = engagementvideos,
      tiktok_screentime_pre = screentime_before,
      tiktok_screentime_post = screentime_after,
      tiktok_screentime_avg = screentime_average_tiktok,
      watch_tiktok_compliance = watch_tiktok,
      watch_tiktok_compliance_text = watch_tiktok_text,

      # Netflix data
      task_netflix_compliance = task_1_response_netflix,
      task_netflix_strategy = task_2_response_netflix,
      task_netflix_compliance_text = task1_text_netflix,
      task_netflix_strategy_text = task2_text_netflix,
      netflix_screentime_avg = screentime_average_netflix,
      watch_netflix_compliance = watch_netflix,
      watch_netflix_compliance_switch = shows_watched,
      watch_netflix_compliance_text = watch_netflix_text,

      # STRAP-R Questionnaire Items
      strap_tiktok_1 = strap_1_tiktok,
      strap_tiktok_2 = strap_2_tiktok,
      strap_tiktok_3 = strap_3_tiktok,
      strap_netflix_1 = strap_1_netflix,
      strap_netflix_2 = strap_2_netflix,
      strap_netflix_3 = strap_3_netflix,

      # Catch questions (1,2 = hard, 3,4 = easy)
      catch_1 = catch_questions_1,
      catch_2 = catch_questions_2,
      catch_3 = catch_questions_3,
      catch_4 = catch_questions_4,

      # RISQ-Questionnaire
      risq_tiktok_avoid = risq1_default_1,
      risq_netflix_avoid = risq1_default_2,
      risq_gamble_avoid = risq1_default_3,
      risq_tiktok_approach = risq2_default_1,
      risq_netflix_approach = risq2_default_2,
      risq_gamble_approach = risq2_default_3,

      # ASSIST Question 8 items
      assist_8_inject = assist_8,
      assist_8_inject_text = assist_8_followup
    ) %>%
    rename_with(.fn = ~ case_when(
      # Format RISQ avoidance questions
      startsWith(.x, "risq1_") & !.x %in% c("risq1_default_1", "risq1_default_2", "risq1_default_3") ~
        paste0("risq_", sub("risq1_", "", .x), "_avoid"),

      # Format RISQ approach questions
      startsWith(.x, "risq2_") & !.x %in% c("risq2_default_1", "risq2_default_2", "risq2_default_3") ~
        paste0("risq_", sub("risq2_", "", .x), "_approach"),

      # Format ASSIST questions: 'assist_[drug]_[number]'
      startsWith(.x, "assist_") & !.x %in% c("assist_8") ~
        paste0("assist_", sub("assist_[0-9]+_", "", .x), "_", sub("assist_([0-9]+)_.*", "\\1", .x)),

      # Format ASSIST screening questions: 'assist_[drug]'
      startsWith(.x, "all_drugs_") ~
        paste0("assist_", sub("all_drugs_", "", .x)),

      # Keep everything else unchanged
      TRUE ~ .x
    )) %>%
    # set appropriate data type for formatted variables
    dplyr::mutate(
      across(.cols = dplyr::matches("^assist_.*_\\d+$") & !dplyr::matches("text"), .fns = as.integer),
      assist_other_text = as.character(assist_other_text),
      across(.cols = dplyr::matches("^risq_"), .fns = as.integer),
    ) %>%
    #### Questionnaire Total and Subscale Scores ------------------------------------------------------------------------------
    dplyr::mutate(
      # Barratt Impulsiveness Scale
      bis_11_total = rowSums(across(starts_with("bis")), na.rm = TRUE),
      bis_11_attention = rowSums(across(c(bis_5, bis_6, bis_9, bis_11, bis_20, bis_24, bis_26, bis_28)), na.rm = TRUE),
      bis_11_motor = rowSums(across(c(bis_2, bis_3, bis_4, bis_16, bis_17, bis_19, bis_21, bis_22, bis_23, bis_25, bis_30)), na.rm = TRUE),
      bis_11_nonplanning = rowSums(across(c(bis_1, bis_7, bis_8, bis_10, bis_12, bis_13, bis_14, bis_15, bis_18, bis_27, bis_29)), na.rm = TRUE),

      # Patient Health Questionnaire
      phq_8_total = rowSums(across(starts_with("phq")), na.rm = TRUE),

      # Generalized Anxiety Disorder
      gad_7_total = rowSums(across(starts_with("gad")), na.rm = TRUE),

      # Compulsive Internet Use Scale
      cius_14_total = rowSums(across(starts_with("cius")), na.rm = TRUE),

      # Alcohol Smoking and Substance Involvement Screening Test
      assist_alcohol_total = rowSums(across(starts_with("assist_alcohol_")), na.rm = TRUE),
      assist_cannabis_total = rowSums(across(starts_with("assist_cannabis_")), na.rm = TRUE),
      assist_cocaine_total = rowSums(across(starts_with("assist_cocaine_")), na.rm = TRUE),
      assist_amphetamine_total = rowSums(across(starts_with("assist_amphetamine_")), na.rm = TRUE),
      assist_inhalants_total = rowSums(across(starts_with("assist_inhalants_")), na.rm = TRUE),
      assist_sedatives_total = rowSums(across(starts_with("assist_sedatives_")), na.rm = TRUE),
      assist_hallucinogens_total = rowSums(across(starts_with("assist_hallucinogens_")), na.rm = TRUE),
      assist_opioids_total = rowSums(across(starts_with("assist_opioids_")), na.rm = TRUE),
      assist_other_total = rowSums(across(starts_with("assist_other_") & !contains("text")), na.rm = TRUE),

      # Bergen Social Media Addiction Scale
      bsmas_tiktok_total = rowSums(across(starts_with("bsmas_tiktok")), na.rm = TRUE),
      bsmas_netflix_total = rowSums(across(starts_with("bsmas_netflix")), na.rm = TRUE),

      # Sensitivity To Reinforcement of Addictive and other Primary Rewards Items
      strap_tiktok_avg = rowMeans(across(starts_with("strap_tiktok")), na.rm = TRUE),
      strap_netflix_avg = rowMeans(across(starts_with("strap_netflix")), na.rm = TRUE),
    ) %>%
    ### Add Descriptive Labels ------------------------------------------------------------------------------------------------

    set_variable_labels(
      subj_id = "Participant identifier (Questionnaire)",
      task_netflix_compliance = "Compliance to not switch hand or finger during Netflix task",
      task_tiktok_compliance = "Compliance to not switch hand or finger during TikTok task",
      task_netflix_compliance_text = "Reason for non-compliance (switch hand/ finger)",
      task_tiktok_compliance_text = "Reason for non-compliance (switch hand/ finger)",
      task_tiktok_strategy = "Strategy to complete TikTok effort-based decision-making task",
      task_netflix_strategy = "Strategy to complete Netflix effort-based decision-making task",
      task_tiktok_strategy_text = "Explained strategy to complete TikTok effort-based decision-making task",
      task_netflix_strategy_text = "Explained strategy to complete Netflix effort-based decision-making task",
      netflix_screentime_avg = "Estimated time spent watching Netflix per week",
      strap_netflix_1 = "Netflix liking measurement 1",
      strap_tiktok_1 = "Netflix liking measurement 1",
      strap_netflix_2 = "Netflix liking measurement 2",
      strap_tiktok_2 = "TikTok liking measurement 2",
      strap_netflix_3 = "TikTok liking measurement 3",
      strap_tiktok_3 = "TikTok liking measurement 3",
      watch_netflix_compliance_switch = "Compliance to not switch show/movie during waiting period",
      watch_netflix_compliance = "Compliance to not switch application during waiting period",
      watch_tiktok_compliance = "Compliance to not switch application during waiting period",
      watch_netflix_compliance_text = "Reason for non-compliance",
      watch_tiktok_compliance_text = "Reason for non-compliance",
      tiktok_videos_watched = "Number of videos watched during the waiting period",
      tiktok_checking_frequency = "Checking frequency of TikTok during day",
      tiktok_screentime_pre = "Screentime prior to waiting period start",
      tiktok_screentime_post = "Screentime after waiting period end",
      tiktok_screentime_avg = "Average weekly screentime on TikTok",
      bsmas_tiktok_1 = "Bergen Social Media Addiction Scale, TikTok question 1",
      bsmas_tiktok_2 = "Bergen Social Media Addiction Scale, TikTok question 2",
      bsmas_tiktok_3 = "Bergen Social Media Addiction Scale, TikTok question 3",
      bsmas_tiktok_4 = "Bergen Social Media Addiction Scale, TikTok question 4",
      bsmas_tiktok_5 = "Bergen Social Media Addiction Scale, TikTok question 5",
      bsmas_tiktok_6 = "Bergen Social Media Addiction Scale, TikTok question 6",
      bsmas_netflix_1 = "Bergen Social Media Addiction Scale, Netflix question 1",
      bsmas_netflix_2 = "Bergen Social Media Addiction Scale, Netflix question 2",
      bsmas_netflix_3 = "Bergen Social Media Addiction Scale, Netflix question 3",
      bsmas_netflix_4 = "Bergen Social Media Addiction Scale, Netflix question 4",
      bsmas_netflix_5 = "Bergen Social Media Addiction Scale, Netflix question 5",
      bsmas_netflix_6 = "Bergen Social Media Addiction Scale, Netflix question 6",
      bis_1 = "Barratt Impulsiveness Scale question 1",
      bis_2 = "Barratt Impulsiveness Scale question 2",
      bis_3 = "Barratt Impulsiveness Scale question 3",
      bis_4 = "Barratt Impulsiveness Scale question 4",
      bis_5 = "Barratt Impulsiveness Scale question 5",
      bis_6 = "Barratt Impulsiveness Scale question 6",
      bis_7 = "Barratt Impulsiveness Scale question 7",
      bis_8 = "Barratt Impulsiveness Scale question 8",
      bis_9 = "Barratt Impulsiveness Scale question 9",
      bis_10 = "Barratt Impulsiveness Scale question 10",
      bis_11 = "Barratt Impulsiveness Scale question 11",
      bis_12 = "Barratt Impulsiveness Scale question 12",
      bis_13 = "Barratt Impulsiveness Scale question 13",
      bis_14 = "Barratt Impulsiveness Scale question 14",
      bis_15 = "Barratt Impulsiveness Scale question 15",
      bis_16 = "Barratt Impulsiveness Scale question 16",
      bis_17 = "Barratt Impulsiveness Scale question 17",
      bis_18 = "Barratt Impulsiveness Scale question 18",
      bis_19 = "Barratt Impulsiveness Scale question 19",
      bis_20 = "Barratt Impulsiveness Scale question 20",
      bis_21 = "Barratt Impulsiveness Scale question 21",
      bis_22 = "Barratt Impulsiveness Scale question 22",
      bis_23 = "Barratt Impulsiveness Scale question 23",
      bis_24 = "Barratt Impulsiveness Scale question 24",
      bis_25 = "Barratt Impulsiveness Scale question 25",
      bis_26 = "Barratt Impulsiveness Scale question 26",
      bis_27 = "Barratt Impulsiveness Scale question 27",
      bis_28 = "Barratt Impulsiveness Scale question 28",
      bis_29 = "Barratt Impulsiveness Scale question 29",
      bis_30 = "Barratt Impulsiveness Scale question 30",
      phq_1 = "Patient Health Questionnaire question 1",
      phq_2 = "Patient Health Questionnaire question 2",
      phq_3 = "Patient Health Questionnaire question 3",
      phq_4 = "Patient Health Questionnaire question 4",
      phq_5 = "Patient Health Questionnaire question 5",
      phq_6 = "Patient Health Questionnaire question 6",
      phq_7 = "Patient Health Questionnaire question 7",
      phq_8 = "Patient Health Questionnaire question 8",
      gad_1 = "Generalized Anxiety Disorder question 1",
      gad_2 = "Generalized Anxiety Disorder question 2",
      gad_3 = "Generalized Anxiety Disorder question 3",
      gad_4 = "Generalized Anxiety Disorder question 4",
      gad_5 = "Generalized Anxiety Disorder question 5",
      gad_6 = "Generalized Anxiety Disorder question 6",
      gad_7 = "Generalized Anxiety Disorder question 7",
      cius_1 = "Compulsive Internet Use Scale question 1",
      cius_2 = "Compulsive Internet Use Scale question 2",
      cius_3 = "Compulsive Internet Use Scale question 3",
      cius_4 = "Compulsive Internet Use Scale question 4",
      cius_5 = "Compulsive Internet Use Scale question 5",
      cius_6 = "Compulsive Internet Use Scale question 6",
      cius_7 = "Compulsive Internet Use Scale question 7",
      cius_8 = "Compulsive Internet Use Scale question 8",
      cius_9 = "Compulsive Internet Use Scale question 9",
      cius_10 = "Compulsive Internet Use Scale question 10",
      cius_11 = "Compulsive Internet Use Scale question 11",
      cius_12 = "Compulsive Internet Use Scale question 12",
      cius_13 = "Compulsive Internet Use Scale question 13",
      cius_14 = "Compulsive Internet Use Scale question 14",
      catch_1 = "Hard catch question 1",
      catch_2 = "Hard catch question 2",
      catch_3 = "Simple catch question 1",
      catch_4 = "Simple catch question 2",
      catch_questions_hard = "Passed both hard catch questions?",
      catch_questions_easy = "Passed one or more easy catch questions?",
      assist_tobacco = "Alcohol Smoking and Substance Involvement Screening Test, Tobacco",
      assist_alcohol = "Alcohol Smoking and Substance Involvement Screening Test, Alcohol",
      assist_cannabis = "Alcohol Smoking and Substance Involvement Screening Test, Cannabis",
      assist_cocaine = "Alcohol Smoking and Substance Involvement Screening Test, Cocaine",
      assist_amphetamine = "Alcohol Smoking and Substance Involvement Screening Test, Amphetamine",
      assist_inhalants = "Alcohol Smoking and Substance Involvement Screening Test, Inhalants",
      assist_sedatives = "Alcohol Smoking and Substance Involvement Screening Test, Sedatives",
      assist_hallucinogens = "Alcohol Smoking and Substance Involvement Screening Test, Hallucinogens",
      assist_opioids = "Alcohol Smoking and Substance Involvement Screening Test, Opioids",
      assist_other = "Alcohol Smoking and Substance Involvement Screening Test, Other substance",
      assist_other_text = "Other substance reported",
      assist_tobacco_2 = "ASSIST Tobacco question 2",
      assist_tobacco_3 = "ASSIST Tobacco question 3",
      assist_tobacco_4 = "ASSIST Tobacco question 4",
      assist_tobacco_5 = "ASSIST Tobacco question 5",
      assist_tobacco_6 = "ASSIST Tobacco question 6",
      assist_tobacco_7 = "ASSIST Tobacco question 7",
      assist_alcohol_2 = "ASSIST Alcohol question 2",
      assist_alcohol_3 = "ASSIST Alcohol question 3",
      assist_alcohol_4 = "ASSIST Alcohol question 4",
      assist_alcohol_5 = "ASSIST Alcohol question 5",
      assist_alcohol_6 = "ASSIST Alcohol question 6",
      assist_alcohol_7 = "ASSIST Alcohol question 7",
      assist_cannabis_2 = "ASSIST Cannabis question 2",
      assist_cannabis_3 = "ASSIST Cannabis question 3",
      assist_cannabis_4 = "ASSIST Cannabis question 4",
      assist_cannabis_5 = "ASSIST Cannabis question 5",
      assist_cannabis_6 = "ASSIST Cannabis question 6",
      assist_cannabis_7 = "ASSIST Cannabis question 7",
      assist_cocaine_2 = "ASSIST Cocaine question 2",
      assist_cocaine_3 = "ASSIST Cocaine question 3",
      assist_cocaine_4 = "ASSIST Cocaine question 4",
      assist_cocaine_5 = "ASSIST Cocaine question 5",
      assist_cocaine_6 = "ASSIST Cocaine question 6",
      assist_cocaine_7 = "ASSIST Cocaine question 7",
      assist_amphetamine_2 = "ASSIST Amphetamine question 2",
      assist_amphetamine_3 = "ASSIST Amphetamine question 3",
      assist_amphetamine_4 = "ASSIST Amphetamine question 4",
      assist_amphetamine_5 = "ASSIST Amphetamine question 5",
      assist_amphetamine_6 = "ASSIST Amphetamine question 6",
      assist_amphetamine_7 = "ASSIST Amphetamine question 7",
      assist_inhalants_2 = "ASSIST Inhalants question 2",
      assist_inhalants_3 = "ASSIST Inhalants question 3",
      assist_inhalants_4 = "ASSIST Inhalants question 4",
      assist_inhalants_5 = "ASSIST Inhalants question 5",
      assist_inhalants_6 = "ASSIST Inhalants question 6",
      assist_inhalants_7 = "ASSIST Inhalants question 7",
      assist_sedatives_2 = "ASSIST Sedatives question 2",
      assist_sedatives_3 = "ASSIST Sedatives question 3",
      assist_sedatives_4 = "ASSIST Sedatives question 4",
      assist_sedatives_5 = "ASSIST Sedatives question 5",
      assist_sedatives_6 = "ASSIST Sedatives question 6",
      assist_sedatives_7 = "ASSIST Sedatives question 7",
      assist_hallucinogens_2 = "ASSIST Hallucinogens question 2",
      assist_hallucinogens_3 = "ASSIST Hallucinogens question 3",
      assist_hallucinogens_4 = "ASSIST Hallucinogens question 4",
      assist_hallucinogens_5 = "ASSIST Hallucinogens question 5",
      assist_hallucinogens_6 = "ASSIST Hallucinogens question 6",
      assist_hallucinogens_7 = "ASSIST Hallucinogens question 7",
      assist_opioids_2 = "ASSIST Opioids question 2 ",
      assist_opioids_3 = "ASSIST Opioids question 3",
      assist_opioids_4 = "ASSIST Opioids question 4",
      assist_opioids_5 = "ASSIST Opioids question 5",
      assist_opioids_6 = "ASSIST Opioids question 6",
      assist_opioids_7 = "ASSIST Opioids question 7",
      assist_other_2 = "ASSIST Other question 2",
      assist_other_3 = "ASSIST Other question 3",
      assist_other_4 = "ASSIST Other question 4",
      assist_other_5 = "ASSIST Other question 5",
      assist_other_6 = "ASSIST Other question 6",
      assist_other_7 = "ASSIST Other question 7",
      assist_inject_8 = "Any substance injected in the past 3 months?",
      assist_inject_text_8 = "Description of injection pattern",
      risq_tiktok_avoid = "RISQ Avoidance Questions",
      risq_netflix_avoid = "RISQ Avoidance Questions",
      risq_gamble_avoid = "RISQ Avoidance Questions",
      risq_tobacco_avoid = "RISQ Avoidance Questions",
      risq_alcohol_avoid = "RISQ Avoidance Questions",
      risq_cannabis_avoid = "RISQ Avoidance Questions",
      risq_cocaine_avoid = "RISQ Avoidance Questions",
      risq_amphetamine_avoid = "RISQ Avoidance Questions",
      risq_inhalants_avoid = "RISQ Avoidance Questions",
      risq_sedatives_avoid = "RISQ Avoidance Questions",
      risq_hallucinogens_avoid = "RISQ Avoidance Questions",
      risq_opioids_avoid = "RISQ Avoidance Questions",
      risq_other_avoid = "RISQ Avoidance Questions",
      risq_tiktok_approach = "RISQ Approach Questions",
      risq_netflix_approach = "RISQ Approach Questions",
      risq_gamble_approach = "RISQ Approach Questions",
      risq_tobacco_approach = "RISQ Approach Questions",
      risq_alcohol_approach = "RISQ Approach Questions",
      risq_cannabis_approach = "RISQ Approach Questions",
      risq_cocaine_approach = "RISQ Approach Questions",
      risq_amphetamine_approach = "RISQ Approach Questions",
      risq_inhalants_approach = "RISQ Approach Questions",
      risq_sedatives_approach = "RISQ Approach Questions",
      risq_hallucinogens_approach = "RISQ Approach Questions",
      risq_opioids_approach = "RISQ Approach Questions",
      risq_other_approach = "RISQ Approach Questions",
      bis_11_total = "Barratt Impulsiveness Scale total score",
      bis_11_attention = "Barratt Impulsiveness Scale attention subscale score",
      bis_11_motor = "Barratt Impulsiveness Scale motor subscale score",
      bis_11_nonplanning = "Barratt Impulsiveness Scale nonplanning subscale score",
      phq_8_total = "Patient Health Questionnaire total score",
      gad_7_total = "Generalised Anxiety Disorder total score",
      cius_14_total = "Compulsive TikTok Use total score",
      assist_alcohol_total = "ASSIST Alcohol total score",
      assist_cannabis_total = "ASSIST Cannabis total score",
      assist_cocaine_total = "ASSIST Cocaine total score",
      assist_amphetamine_total = "ASSIST Amphetamine total score",
      assist_inhalants_total = "ASSIST Inhalants total score",
      assist_sedatives_total = "ASSIST Sedatives total score",
      assist_hallucinogens_total = "ASSIST Hallucinogens total score",
      assist_opioids_total = "ASSIST Opioids total score",
      assist_other_total = "ASSIST Other total score",
      bsmas_tiktok_total = "Bergen Social Media Addiction Scale, TikTok total score",
      bsmas_netflix_total = "Bergen Social Media Addiction Scale, Netflix total score",
      strap_tiktok_avg = "Sensitivity To Reinforcement of Addictive and other Primary Rewards, TikTok average score",
      strap_netflix_avg = "Sensitivity To Reinforcement of Addictive and other Primary Rewards, Netflix average score"
    ) %>%
    ### Add Numeric Coding in Variable Labels ---------------------------------------------------------------------------------

    dplyr::mutate(
      # strap item coding
      across(
        .cols = c(starts_with("strap_tiktok_") & !contains("avg"), starts_with("strap_netflix_") & !contains("avg")),
        .fns = ~ set_labels(.x, labels = c(
          "1" = "extremely unpleasant",
          "2" = "quite unpleasant",
          "3" = "somewhat unpleasant",
          "4" = "neutral",
          "5" = "somewhat pleasant",
          "6" = "quite pleasant",
          "7" = "extremely pleasant"
        ))
      ),

      # risq item coding
      across(
        .cols = c(starts_with("risq")),
        .fns = ~ set_labels(.x, labels = c(
          "0" = "strongly disagree",
          "1" = "disagree",
          "2" = "equally disagree/ agree",
          "3" = "agree",
          "4" = "strongly agree"
        ))
      ),

      # bsmas item coding
      across(
        .cols = c(starts_with("bsmas") & !contains("total")),
        .fns = ~ set_labels(.x, labels = c(
          "0" = "very rarely",
          "1" = "rarely",
          "2" = "sometimes",
          "3" = "often",
          "4" = "very often"
        ))
      ),

      # phq item coding
      across(
        .cols = c(starts_with("phq") & !contains("total")),
        .fns = ~ set_labels(.x, labels = c(
          "0" = "not at all",
          "1" = "several days",
          "2" = "more than half the days",
          "3" = "nearly every day"
        ))
      ),

      # gad item coding
      across(
        .cols = c(starts_with("gad") & !contains("total")),
        .fns = ~ set_labels(.x, labels = c(
          "0" = "not at all",
          "1" = "several days",
          "2" = "more than half the days",
          "3" = "nearly every day"
        ))
      ),

      # cius item coding
      across(
        .cols = c(starts_with("cius") & !contains("total")),
        .fns = ~ set_labels(.x, labels = c(
          "0" = "never",
          "1" = "seldom",
          "2" = "sometimes",
          "3" = "often",
          "4" = "very often"
        ))
      ),

      # assist item coding (q2)
      across(
        .cols = c(starts_with("assist") & ends_with("2")),
        .fns = ~ set_labels(.x, labels = c(
          "0" = "never",
          "2" = "once or twice",
          "3" = "monthly",
          "4" = "weekly",
          "6" = "daily or almost daily"
        ))
      ),

      # assist item coding (q3)
      across(
        .cols = c(starts_with("assist") & ends_with("3")),
        .fns = ~ set_labels(.x, labels = c(
          "0" = "never",
          "3" = "once or twice",
          "4" = "monthly",
          "5" = "weekly",
          "6" = "daily or almost daily"
        ))
      ),

      # assist item coding (q4)
      across(
        .cols = c(starts_with("assist") & ends_with("4")),
        .fns = ~ set_labels(.x, labels = c(
          "0" = "never",
          "4" = "once or twice",
          "5" = "monthly",
          "6" = "weekly",
          "7" = "daily or almost daily"
        ))
      ),

      # assist item coding (q5)
      across(
        .cols = c(starts_with("assist") & ends_with("5")),
        .fns = ~ set_labels(.x, labels = c(
          "0" = "never",
          "5" = "once or twice",
          "6" = "monthly",
          "7" = "weekly",
          "8" = "daily or almost daily"
        ))
      ),

      # assist item coding (q6)
      across(
        .cols = c(starts_with("assist") & ends_with("6") | ends_with("7")),
        .fns = ~ set_labels(.x, labels = c(
          "0" = "no, never",
          "3" = "yes, but not in the past three months",
          "6" = "yes, in the past three months"
        ))
      ),

      # bis item coding
      across(
        .cols = c(starts_with("bis") & !contains(c("total", "attention", "motor", "nonplanning")) &
          !ends_with(c("1", "7", "8", "9", "10", "12", "13", "15", "20", "29", "30"))),
        .fns = ~ set_labels(.x, labels = c(
          "1" = "rarely/ never",
          "2" = "occasionally",
          "3" = "often",
          "4" = "almost always/ always"
        ))
      ),

      # bis item coding (reversed items) "1", "7", "8", "9", "10", "12", "13", "15", "20", "29", "30"
      across(
        .cols = c(starts_with("bis") & !contains(c("total", "attention", "motor", "nonplanning")) &
          ends_with(c("1", "7", "8", "9", "10", "12", "13", "15", "20", "29", "30"))),
        .fns = ~ set_labels(.x, labels = c(
          "1" = "almost always/ always",
          "2" = "often",
          "3" = "occasionally",
          "4" = "rarely/ never"
        ))
      )
    )

  ## Output Results =========================================================================================================

  results <- list(
    "demographics"   = demographics,
    "task_meta_data" = task_meta_data,
    "task_data"      = task_data,
    "questionnaires" = combined_questionnaires
  )

  return(results)
}

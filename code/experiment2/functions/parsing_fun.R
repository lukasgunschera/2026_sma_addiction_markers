## ======================================================================================================================= ##
## Script:    Parsing Function
## ======================================================================================================================= ##
## Authors:   Clelia Veith
## Date:      Tue Nov 18 07:59:02 2025
## ======================================================================================================================= ##
## Description: 
##  Parsing function for processing JATOS .txt data files
##  Based on Mehrhof & Nord (2025) parsing function
##
## Function arguments:
##  file: relative path to .txt data file or existing R object 
##  control_group: is it control group (TRUE) or addiction group (FALSE) data? 
##  screening_included: does data include screening component (TRUE) or not (FALSE)? 
##  demographic_dat: include demographic data in output?
##  task_dat: include task data in output?
##  modelling_dat: include task data formatted for modelling?
##  tiktok_dat: include TikTok use data in output?
##  questionnaire_dat: include questionnaire data in output?
## exclude_subjs: subjects to be excluded due to faulty data
##
## Notes: 
##  _d denotes devalued, _b denotes baseline
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
                        questionnaire_dat = TRUE,
                        exclude_subjs = c("dummy")){


  # Does data still have to be imported?
  if(class(file) == "list"){
    
    raw_dat <- file
    
    } else if(class(file) == "character") {
      
      if(length(file) == 1){
        
      raw_dat <- jsonlite::fromJSON(
        sprintf('[%s]', paste(readLines(file, encoding="UTF-8", warn=F), 
                              collapse = ',')))
      } else if(length(file) > 1){
        
      raw_dat <- list()
      for(i in 1:length(file)){
        
        raw_dat <- append(
          raw_dat, jsonlite::fromJSON(
            sprintf('[%s]',paste(readLines(file[i],encoding="UTF-8", warn=F),
                                 collapse = ','))))
        }
      }
    }
  
  # Source helper functions
  source(here::here("code", "functions", "helper_functions.R"))
  
  
  ### Exclusion of incomplete data sets  --------------------------------------
  
  # Check if questionnaire component included 
  # Extract index of questionnaire component
  res_questionnaire <- which(lapply(1:length(raw_dat), function (i) 
    any(grepl("questionnaire", raw_dat[[i]], fixed = TRUE)) &
      !any(grepl(exclude_subjs, raw_dat[[i]], fixed = TRUE))
  ) %in% TRUE)
  
  # Extract all participant IDs
  all_ids <- unique(sapply(raw_dat, function(x) x$prolific_id[1]))
  all_ids <- unlist(all_ids, use.names = FALSE)
  
  # Extract participants who completed all components (including questionnaires)
  complete_ids <- if (questionnaire_dat) {
    sapply(raw_dat[res_questionnaire], function(x) x$prolific_id[1])
  } else {
    all_ids
  }
  
  # Participants with incomplete data
  incomplete_ids <- setdiff(all_ids, complete_ids)
  
  # Add IDs of incomplete data sets to exclude_subjs
  exclude_subjs <- unique(c(exclude_subjs, incomplete_ids))
  exclude_subjs_task <- unlist(exclude_subjs, use.names = FALSE)
  # Combine IDs into single string (so that grepl later works)
  exclude_subjs <- paste(exclude_subjs_task, collapse = "|")
  
  
  ### SCREENING ---------------------------------------------------------------

 if (screening_included){
   
  res_screening <- which(lapply(1:length(raw_dat), function (i) 
    any(grepl("screening", raw_dat[[i]], fixed = TRUE)) &
      !any(grepl(exclude_subjs, raw_dat[[i]], fixed = TRUE))) %in% TRUE)
    
  screening_df <- as.data.frame(matrix(ncol = 3, 
                                       nrow = (length(res_screening)))) 
  colnames(screening_df) <- c("subj_id",
                              "consent",
                              "screening")
  
  
  for (comp_i in seq_along(res_screening)) {
      
    screening <- sapply(raw_dat[[res_screening[comp_i]]], na.omit)
    
    screening_df[comp_i, "subj_id"] <- screening$prolific_id[1]
    screening_df[comp_i, "consent"] <- screening$consent_confirmation[1]
    screening_df[comp_i, "screening"] <- screening$screening[1]
    
    screening_df <- as.data.frame(screening_df)
  }
 }
  
  
  ### COMPONENT 1: Intake data ------------------------------------------------
  
  # Extract data from intake component
  res_comp_1 <- which(lapply(1:length(raw_dat), function (i) 
    any(grepl("intake", raw_dat[[i]], fixed=T)) &
      !any(grepl(exclude_subjs, raw_dat[[i]]))) %in% TRUE)
  
  # Create data frame for intake data
  comp_1_df <- as.data.frame(matrix(ncol = 27, nrow = (length(res_comp_1))))
  comp_1_df <- as.data.frame(comp_1_df)
  colnames(comp_1_df) <- c("subj_id",
                           "consent",
                           "group",
                           "order",
                           "age", 
                           "gender",
                           "gender_other",
                           "ethnicity",
                           "ethnicity_other",
                           "income_value",
                           "income_currency",
                           "neuro_screening",
                           "neuro_ms",
                           "neuro_epilepsy",
                           "neuro_tbi",
                           "neuro_headache",
                           "neuro_parkinsons",
                           "neuro_stroke",
                           "neuro_other",
                           "mh_screening",
                           "mh_mdd",
                           "mh_bpd",
                           "mh_gad",
                           "mh_ocd",
                           "mh_autism",
                           "mh_other",
                           "device")
  
  for(comp_i in seq_along(res_comp_1)) {
    
    intake <- sapply(raw_dat[[res_comp_1[comp_i]]], na.omit)

    # Demographics
    #comp_1_df[comp_i, "subj_id"] <- id_shuffle(unique(intake$prolific_id))
    comp_1_df[comp_i, "subj_id"] <- intake$prolific_id[1]
    
    # If consent response missing, extract it from screening component
    comp_1_df[comp_i, "consent"] <- if (screening_included) {
      comp_1_df[comp_i, "consent"] <- screening_df$consent[1]
      } else {
        comp_1_df[comp_i, "consent"] <- intake$consent_confirmation[1]
      }
    
    # If group variable missing, extract response from screening component
    comp_1_df[comp_i, "group"] <-
      if ("group" %in% names(intake)) {
        intake$group[1]
      } else {
        ifelse(screening_df$screening[1] %in% c(0,1), "control",
               ifelse(screening_df$screening[1] %in% c(3,4), "addicted", NA))
      }
    
    comp_1_df[comp_i, "order"] <- intake$condition[1]
    
    comp_1_df[comp_i, "age"] <- as.numeric(intake$age)
  
    comp_1_df[comp_i, "gender"] <- intake$gender[1]
    comp_1_df[comp_i, "gender_other"] <- ifelse(
      !is.null(intake$gender_other), intake$gender_other, NA)
    
    comp_1_df[comp_i, "ethnicity"] <- intake$ethnicity[1]
    comp_1_df[comp_i, "ethnicity_other"] <- ifelse(
      !is.null(intake$ethnicity_other), intake$ethnicity_other, NA)
    
    comp_1_df[comp_i, "income_value"] <- intake$income_value[1]
    comp_1_df[comp_i, "income_currency"] <- intake$income_currency[1]
    
    # Neurological & mental health conditions
    if (intake$neuro_screen[1] == 1) {
      comp_1_df[comp_i, "neuro_screening"] <- TRUE
      
      neuro_conditions <- unlist(intake$neuro_conditions[1])
      
      comp_1_df[comp_i, "neuro_ms"] <- "ms" %in% neuro_conditions
      comp_1_df[comp_i, "neuro_epilepsy"] <- "epilepsy" %in% neuro_conditions
      comp_1_df[comp_i, "neuro_tbi"] <- "tbi" %in% neuro_conditions
      comp_1_df[comp_i, "neuro_headache"] <- "headache" %in% neuro_conditions
      comp_1_df[comp_i, "neuro_parkinsons"] <- "parkinsons" %in%neuro_conditions
      comp_1_df[comp_i, "neuro_stroke"] <- "stroke" %in% neuro_conditions
      
      if ("other" %in% neuro_conditions) {
        comp_1_df[comp_i, "neuro_other"] <- intake$neuro_other[1]
      } else {
        comp_1_df[comp_i, "neuro_other"] <- NA
      }
             
    } else {
      comp_1_df[comp_i, c("neuro_screening", "neuro_ms", "neuro_epilepsy", 
                          "neuro_tbi","neuro_headache", "neuro_parkinsons",
                          "neuro_stroke", "neuro_other")] <- FALSE
      comp_1_df[comp_i, "neuro_other"] <- NA
    }
    
    
    # Mental health conditions
    if (intake$mh_screen[1] == 1) {
      comp_1_df[comp_i, "mh_screening"] <- TRUE
      
      mh_conditions <- unlist(intake$mh_conditions[1])
      
      comp_1_df[comp_i, "mh_mdd"] <- "mdd" %in% mh_conditions
      comp_1_df[comp_i, "mh_bpd"] <- "bpd" %in% mh_conditions
      comp_1_df[comp_i, "mh_gad"] <- "gad" %in% mh_conditions
      comp_1_df[comp_i, "mh_ocd"] <- "ocd" %in% mh_conditions
      comp_1_df[comp_i, "mh_autism"] <- "autism" %in% mh_conditions
      
      if ("other" %in% mh_conditions) {
        comp_1_df[comp_i, "mh_other"] <- intake$mh_other[1]
      } else {
        comp_1_df[comp_i, "mh_other"] <- NA
      }
      
    } else {
      comp_1_df[comp_i, c("mh_screening", "mh_mdd", "mh_bpd", 
                          "mh_gad", "mh_ocd", "mh_autism")] <- FALSE
      comp_1_df[comp_i, "mh_other"] <- NA
    }
    
    comp_1_df[comp_i, "device"] <- intake$device_type[1]
    
  comp_1_df <- as.data.frame(comp_1_df)
  comp_1_df$age <- as.numeric(comp_1_df$age)
  comp_1_df$income_value <- as.numeric(comp_1_df$income_value)
  }
  
  
  ### COMPONENT 2: Task data --------------------------------------------------

  if(task_dat){
    
    res_comp_2 <- which(lapply(1:length(raw_dat), function (i) 
      any(grepl("This is the start of the game", raw_dat[[i]], fixed=T)) &
        !any(grepl(exclude_subjs, raw_dat[[i]]))) %in% TRUE)
      
      comp_2_df <- data.frame(matrix(ncol = 14, nrow = 0))
      comp_2_meta_df <- data.frame(matrix(ncol = 5, nrow = 0))
      
      for(comp_i in seq_along(res_comp_2)) {
        
        task <- raw_dat[[res_comp_2[comp_i]]]
        
        # Detect & correct double clicking error
        if(length(task$rt) != length(task$phase)){
          warning(paste("Double clicking error in subject number", 
                        comp_1_df[comp_i,"subj_id"], sep = " "))
          # Changed to < 5 (instead of <= 5)
          del <- which(abs(c(diff(task$rt), 6)) < 5 & task$rt != 999) 
      task$rt <- task$rt[-del]
      task$choice <- task$choice[-del]
        }
        
        # Detect & remove extra calibration trials
        if(length(task$clicks) > length(task$phase)){
          del <- length(task$clicks) - length(task$phase)
          task$clicks <- task$clicks[-seq_len(del)]
        }
    
        task_meta_data <- data.frame(
         #subj_id = id_shuffle(unique(raw_dat[[res_comp_2[comp_i]-1]]$prolific_id)),
          subj_id = raw_dat[[res_comp_2[comp_i]-1]]$prolific_id,
          game_id = task$subjID,
          start_time = as_datetime(task$date[1]),
          end_time = as_datetime(task$date[2]),
          task_condition = NA,
          clicking_calibration = 
            mean(task$clicks[task$phase == "calibration"][2:3])
        )
        
        task_data <- data.frame(
          #subj_id = rep(id_shuffle(unique(raw_dat[[res_comp_2[comp_i]-1]]$prolific_id)), length(task$phase)),
          subj_id = rep(raw_dat[[res_comp_2[comp_i]-1]]$prolific_id, 
                        length(task$phase)),
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
          points = ifelse(task$phase == "calibration", task$points, 
                          task$points[-1]),
          clicking_calibration = 
            rep(mean(task$clicks[task$phase =="calibration"][2:3]), 
            length(task$phase)))
        
        # Define task condition based on experiment order
        if (grepl("baseline", task$start[1], fixed = TRUE)) {
          task_data$task_condition[task$phase == "game"] <- "baseline"
        } else if (grepl("devalued", task$start[1], fixed = TRUE)) {
          task_data$task_condition[task$phase == "game"] <- "devalued"
        }
        
        task_data$task_condition[is.na(task_data$task_condition)] <- "NA"
        
        # Task condition for meta data
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
        comp_2_df$clicking_calibration <- comp_2_df$clicking_calibration[1]
        comp_2_meta_df$clicking_calibration <- 
          comp_2_meta_df$clicking_calibration[1]
        
        
        if(modelling_dat){
          
          modelling_df <- list()

          # added task_condition (5)
          data_modelling <- comp_2_df[comp_2_df$phase == "game", 
                                      c(1, 2, 5, 7, 9, 10, 11)]
          
          data_modelling$offerEffort <- standardization(
            data_modelling$offerEffort, 0, unique(data_modelling$offerEffort))
          
          data_modelling$offerReward <- standardization(
            data_modelling$offerReward, 0, unique(data_modelling$offerReward))
          
          colnames(data_modelling) <- c("subjID", "gameID", "task_condition", 
                                      "trial", "effort_a", "amount_a", "choice")
          
          data_modelling <- cbind(
            data_modelling, "effort_b" = rep(0, dim(data_modelling)[1]), 
            "amount_b" = rep(0, dim(data_modelling)[1]))
          
          modelling_df <- data_modelling
        }
      }
  }
  
  # Remove rows of excluded participants
  comp_2_df <- comp_2_df[!comp_2_df$subj_id %in% exclude_subjs_task, ]
  comp_2_meta_df <- comp_2_meta_df[!comp_2_meta_df$subj_id %in% exclude_subjs_task, ]
  
  if (modelling_dat) {
    modelling_df <- modelling_df[!modelling_df$subjID %in% exclude_subjs_task, ]
  }
  
  ### COMPONENT 3: Trial session data -----------------------------------------
  
  # Extract data from trial component
  res_comp_3 <- which(lapply(1:length(raw_dat), function (i) 
    any(grepl("trial1", raw_dat[[i]], fixed=T)) &
      !any(grepl(exclude_subjs, raw_dat[[i]]))) %in% TRUE)
  
  comp_3_df <- as.data.frame(matrix(ncol = 3, nrow = (length(res_comp_3)))) 
  colnames(comp_3_df) <- c("subj_id",
                           "screentime_before",
                           "screentime_before_min")
  
  
  for(comp_i in seq_along(res_comp_3)) {
    
    trial <- raw_dat[[res_comp_3[comp_i]]]
    trial <- trial[!sapply(trial, is.null)]
    
    #comp_3_df[comp_i, "subj_id"] <- id_shuffle(unique(demographics$prolific_id))
    comp_3_df[comp_i, "subj_id"] <- trial$prolific_id[1]
    comp_3_df[comp_i, "screentime_before"] <- trial$screentime_before[1]
    
    hours_minutes <- strsplit(trial$screentime_before[1], ":")[[1]]
    hours <- as.numeric(hours_minutes[1])
    minutes <- as.numeric(hours_minutes[2])
    comp_3_df[comp_i, "screentime_before_min"] <- hours * 60 + minutes
  }
  
  comp_3_df <- as.data.frame(comp_3_df)
  comp_3_df$screentime_before_min <- as.numeric(comp_3_df$screentime_before_min)
    
    
  ### COMPONENT 4: Main session baseline data ---------------------------------
 
  # Extract data from baseline main session component
  res_comp_4 <- which(lapply(1:length(raw_dat), function (i) 
    any(grepl("main_b", raw_dat[[i]], fixed=T)) &
    !any(grepl(exclude_subjs, raw_dat[[i]]))) %in% TRUE)
    
    comp_4_df <- as.data.frame(matrix(ncol = 10, nrow = (length(res_comp_4)))) 
    colnames(comp_4_df) <- c("subj_id",
                             "task_compliance_b",
                             "task_compliance_text_b",
                             "task_strategy_b",
                             "task_strategy_text_b",
                             "liking_1_b",
                             "liking_2_b",
                             "liking_b",
                             "screentime_after_b",
                             "screentime_after_b_min")
    
    for(comp_i in seq_along(res_comp_4)) {
      
      main_b <- raw_dat[[res_comp_4[comp_i]]]
      main_b <- main_b[!sapply(main_b, is.null)]
      
      comp_4_df[comp_i, "subj_id"] <- main_b$prolific_id[1]
      #comp_4_df[comp_i, "session"] <- main_b$component[1]
      
      # Task questions
      comp_4_df[comp_i, "task_compliance_b"] <- main_b$task_question1[1]
      comp_4_df[comp_i, "task_compliance_text_b"] <- ifelse(
        !is.null(main_b$task_explanation1), main_b$task_explanation1, NA)
      
      comp_4_df[comp_i, "task_strategy_b"] <- main_b$task_question2[1]
      comp_4_df[comp_i, "task_strategy_text_b"] <- ifelse(
        !is.null(main_b$task_explanation2), main_b$task_explanation2, NA)
      
      # Liking measures     
      comp_4_df[comp_i, "liking_1_b"] <- as.numeric(
        main_b$liking_during)

      comp_4_df[comp_i, "liking_2_b"] <- as.numeric(
        main_b$liking_after)
      
      comp_4_df[comp_i, "liking_b"] <- mean(
        c(as.numeric(main_b$liking_during), as.numeric(main_b$liking_after)),
        na.rm = TRUE
      )
      
      # Screen time
      comp_4_df[comp_i, "screentime_after_b"] <- main_b$screentime_after_b[1]
      
      hours_minutes <- strsplit(main_b$screentime_after_b[1], ":")[[1]]
      hours <- as.numeric(hours_minutes[1])
      minutes <- as.numeric(hours_minutes[2])
      comp_4_df[comp_i, "screentime_after_b_min"] <- hours * 60 + minutes
    }
 
    comp_4_df <- as.data.frame(comp_4_df)
    comp_4_df$liking_1_b <- as.numeric(comp_4_df$liking_1_b)
    comp_4_df$liking_2_b <- as.numeric(comp_4_df$liking_2_b)
    comp_4_df$liking_b <- as.numeric(comp_4_df$liking_b)
    comp_4_df$screentime_after_b_min <- 
      as.numeric(comp_4_df$screentime_after_b_min)

    
    ### COMPONENT 5: Main session devalued data -------------------------------
    
    # Extract data from devalued main session component
    res_comp_5 <- which(lapply(1:length(raw_dat), function (i) 
      any(grepl("main_d", raw_dat[[i]], fixed=T)) &
        !any(grepl(exclude_subjs, raw_dat[[i]]))) %in% TRUE)
    
    comp_5_df <- as.data.frame(matrix(ncol = 10, nrow = (length(res_comp_5)))) 
    colnames(comp_5_df) <- c("subj_id",
                             "task_compliance_d",
                             "task_compliance_text_d",
                             "task_strategy_d",
                             "task_strategy_text_d",
                             "liking_1_d",
                             "liking_2_d",
                             "liking_d",
                             "screentime_after_d",
                             "screentime_after_d_min"
    )
    
    for(comp_i in seq_along(res_comp_5)) {
      
      main_d <- raw_dat[[res_comp_5[comp_i]]]
      main_d <- main_d[!sapply(main_d, is.null)]

      comp_5_df[comp_i, "subj_id"] <- main_d$prolific_id[1]

      # Task questions
      comp_5_df[comp_i, "task_compliance_d"] <- main_d$task_question1[1]
      comp_5_df[comp_i, "task_compliance_text_d"] <- ifelse(
        !is.null(main_d$task_explanation1), main_d$task_explanation1, NA)
      
      comp_5_df[comp_i, "task_strategy_d"] <- main_d$task_question2[1]
      comp_5_df[comp_i, "task_strategy_text_d"] <- ifelse(
        !is.null(main_d$task_explanation2), main_d$task_explanation2, NA)
      
      # Liking measures     
      comp_5_df[comp_i, "liking_1_d"] <- as.numeric(
        main_d$liking_during)
      
      comp_5_df[comp_i, "liking_2_d"] <- as.numeric(
        main_d$liking_after)
      
      comp_5_df[comp_i, "liking_d"] <- mean(
        c(as.numeric(main_d$liking_during), as.numeric(main_d$liking_after)),
        na.rm = TRUE
      )
      
      # Screen time
      comp_5_df[comp_i, "screentime_after_d"] <- main_d$screentime_after_d[1]
      
      hours_minutes <- strsplit(main_d$screentime_after_d[1], ":")[[1]]
      hours <- as.numeric(hours_minutes[1])
      minutes <- as.numeric(hours_minutes[2])
      comp_5_df[comp_i, "screentime_after_d_min"] <- hours * 60 + minutes
    }
    
    comp_5_df <- as.data.frame(comp_5_df)
    comp_5_df$liking_1_d <- as.numeric(comp_5_df$liking_1_d)
    comp_5_df$liking_2_d <- as.numeric(comp_5_df$liking_2_d)
    comp_5_df$liking_d <- as.numeric(comp_5_df$liking_d)
    comp_5_df$screentime_after_d_min <- 
      as.numeric(comp_5_df$screentime_after_d_min)
    
    
    
    ### COMPONENT 6: Questionnaire data --------------------------------------
    
    # Extract data from questionnaire component
    res_comp_6 <- which(
      lapply(1:length(raw_dat),
             function (i) any(grepl("questionnaires", raw_dat[[i]], fixed=T)) &
               !any(grepl(exclude_subjs, raw_dat[[i]]))) %in% TRUE)

    comp_6_df <- data.frame(matrix(ncol = 0, nrow = 0))

    for(comp_i in seq_along(res_comp_6)) {

      questionnaires <- raw_dat[[res_comp_6[comp_i]]]
      
      comp_6_df[comp_i, "subj_id"] <- questionnaires$prolific_id[1]
      
      
      # BSMAS (scale: 1 to 5)
      bsmas_cols <- grep("^bsmas_\\d{2}$", names(questionnaires), value = TRUE)
      bsmas <- as.data.frame(lapply(questionnaires[bsmas_cols], 
                                    function(x) as.numeric(x) + 1))
      
      for(col in bsmas_cols) {comp_6_df[comp_i, col] <- bsmas[[col]]}
      
      comp_6_df[comp_i, "bsmas_sum"] <- sum(unlist(bsmas))
      
      bsmas_rt_cols <- grep("^bsmas_\\d{2}_rt$", 
                            names(questionnaires), value = TRUE)
      
      bsmas_rt <- questionnaires[bsmas_rt_cols]
      
      for(col in bsmas_rt_cols) { comp_6_df[comp_i, col] <- bsmas_rt[[col]] }
      
      
      # PHQ-8 (scale: 0 to 3)
      phq8_cols <- grep("^phq8_\\d{2}$", names(questionnaires), value = TRUE)
      phq8 <- as.data.frame(lapply(questionnaires[phq8_cols], 
                                    function(x) as.numeric(x)))
      
      for(col in phq8_cols) { comp_6_df[comp_i, col] <- phq8[[col]] }
      
      comp_6_df[comp_i, "phq8_sum"] <- sum(unlist(phq8))
      
      phq8_rt_cols <- grep("^phq8_\\d{2}_rt$", 
                           names(questionnaires), value = TRUE)
      phq8_rt <- questionnaires[phq8_rt_cols]
      for(col in phq8_rt_cols) { comp_6_df[comp_i, col] <- phq8_rt[[col]] }

     
      # GAD-7 (scale: 0 to 3)
      gad7_cols <- grep("^gad7_\\d{2}$", names(questionnaires), value = TRUE)
      gad7 <- as.data.frame(lapply(questionnaires[gad7_cols], 
                                   function(x) as.numeric(x)))
      
      for(col in gad7_cols) { comp_6_df[comp_i, col] <- gad7[[col]] }
      
      comp_6_df[comp_i, "gad7_sum"] <- sum(unlist(gad7))
      
      gad7_rt_cols <- grep("^gad7_\\d{2}_rt$", 
                           names(questionnaires), value = TRUE)
      gad7_rt <- questionnaires[gad7_rt_cols]
      for(col in gad7_rt_cols) { comp_6_df[comp_i, col] <- gad7_rt[[col]] }
      
      
      # BISR-21 (scale: 1 to 4)
      bisr21_cols <- grep("^bisr21_\\d{2}_adj$", 
                              names(questionnaires), value = TRUE)
      bisr21 <- questionnaires[bisr21_cols]
      bisr21 <- as.data.frame(lapply(questionnaires[bisr21_cols], 
                                   function(x) as.numeric(x))) # already 1-4
       
      for(col in bisr21_cols) { comp_6_df[comp_i, col] <- bisr21[[col]] }
      comp_6_df[comp_i, "bisr21_sum"] <- sum(unlist(bisr21))
      
      bisr21_rt_cols <- grep("^bisr21_\\d{2}_rt$", 
                             names(questionnaires), value = TRUE)
      bisr21_rt <- questionnaires[bisr21_rt_cols]
      
      for(col in bisr21_rt_cols) { comp_6_df[comp_i, col] <- 
          bisr21_rt[[col]] }
      
      
      # WSAS-5 (scale: 0 to 8)
      wsas5_cols <- grep("^wsas5_\\d{2}$", names(questionnaires), value = TRUE)
      wsas5 <- as.data.frame(lapply(questionnaires[wsas5_cols], 
                                   function(x) as.numeric(x)))
      
      for(col in wsas5_cols) { comp_6_df[comp_i, col] <- wsas5[[col]] }
      
      comp_6_df[comp_i, "wsas5_sum"] <- sum(unlist(wsas5))
      

      wsas5_rt_cols <- grep("^wsas5_\\d{2}_rt$", 
                            names(questionnaires), value = TRUE)
      wsas5_rt <- questionnaires[wsas5_rt_cols]
      for(col in wsas5_rt_cols) { comp_6_df[comp_i, col] <- wsas5_rt[[col]] }

      
      # Compliance
      if (!is.null(questionnaires$tiktok_compliance_confirmation)) {
        comp_6_df[comp_i, "tiktok_compliance_confirmation"] <- 
          questionnaires$tiktok_compliance_confirmation
      } else {
        comp_6_df[comp_i, "tiktok_compliance_confirmation"] <- NA
      }
      
      if (!is.null(questionnaires$tiktok_non_compliance_session)) {
        comp_6_df[comp_i, "tiktok_non_compliance_session"] <- 
          paste(unlist(questionnaires$tiktok_non_compliance_session), 
                collapse = ", ")
      } else {
        comp_6_df[comp_i, "tiktok_non_compliance_session"] <- NA
      }
      
      if (!is.null(questionnaires$tiktok_non_compliance_explanation)) {
        comp_6_df[comp_i, "tiktok_non_compliance_explanation"] <- 
          questionnaires$tiktok_non_compliance_explanation
      } else {
        comp_6_df[comp_i, "tiktok_non_compliance_explanation"] <- NA
      }
      
      if (!is.null(questionnaires$dev_compliance_confirmation)) {
        comp_6_df[comp_i, "dev_compliance_confirmation"] <- 
          questionnaires$dev_compliance_confirmation
      } else {
        comp_6_df[comp_i, "dev_compliance_confirmation"] <- NA
      }
      
      if (!is.null(questionnaires$dev_non_compliance_session)) {
        comp_6_df[comp_i, "dev_non_compliance_session"] <- 
          paste(unlist(questionnaires$dev_non_compliance_session), 
                collapse = ", ")
      } else {
        comp_6_df[comp_i, "dev_non_compliance_session"] <- NA
      }
      
      if (!is.null(questionnaires$dev_non_compliance_explanation)) {
        comp_6_df[comp_i, "dev_non_compliance_explanation"] <- 
          questionnaires$dev_non_compliance_explanation
      } else {
        comp_6_df[comp_i, "dev_non_compliance_explanation"] <- NA
      }
      
      
      # Final comment
      comp_6_df[comp_i, "final_comment"] <- ifelse(
        is.null(questionnaires$final_comment), NA, questionnaires$final_comment)


      # Catch questions
      catch_easy_cols <- grep("^catch_easy\\d+$", names(questionnaires), 
                              value = TRUE)
      catch_hard_cols <- grep("^catch_hard\\d+$", names(questionnaires), 
                              value = TRUE)
      
      catch_easy <- as.data.frame(lapply(questionnaires[catch_easy_cols], 
                                         function(x) as.numeric(x)))
      catch_hard <- as.data.frame(lapply(questionnaires[catch_hard_cols], 
                                         function(x) as.numeric(x)))
      
      for(col in catch_easy_cols) {comp_6_df[comp_i, col] <- catch_easy[[col]]}
      for(col in catch_hard_cols) {comp_6_df[comp_i, col] <- catch_hard[[col]]}
      
      
      catch_expected_easy <- c(catch_easy1 = 3, catch_easy2 = 0)
      catch_expected_hard <- c(catch_hard1 = 0, catch_hard2 = 0)
      
      # pass if both correct (all)
      catch_pass_easy <- all(mapply(`==`, unlist(catch_easy), 
                                    catch_expected_easy[names(catch_easy)]))
      # pass if at least one correct (any)
      catch_pass_hard <- any(mapply(`==`, unlist(catch_hard), 
                                    catch_expected_hard[names(catch_hard)]))
      

      comp_6_df[comp_i, "catch_pass_easy"] <- catch_pass_easy
      comp_6_df[comp_i, "catch_pass_hard"] <- catch_pass_hard
      
      comp_6_df <- as.data.frame(comp_6_df)
    
    }
  
      
    # Combine components 3, 4 & 5 (all TikTok use data)
    comp_345_df <- merge(comp_3_df, comp_4_df, by = "subj_id", all = TRUE)
    comp_345_df <- merge(comp_345_df, comp_5_df, by = "subj_id", all = TRUE)
    
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

    
    ### OUTPUT ----------------------------------------------------------------
    
    results <- list()
    
    if(demographic_dat){
    results <- append(results, list("demographics" = comp_1_df))
    }
     if(task_dat){
       results <- append(results, list("game" = comp_2_df))
       results <- append(results, list("game_meta" = comp_2_meta_df))
     }
     if(modelling_dat){
      results <- append(results, list("modelling_data" = modelling_df))
     }
    if(tiktok_dat){
      results <- append(results, list("tiktok_use" = comp_345_df))
    }
    if(questionnaire_dat){
      comp_3_df[comp_3_df == "NA"] <- NA
     results <- append(results, list("questionnaires" = comp_6_df))
    }
    

    # Add group variable to each data frame 
    results <- lapply(results, \(df) {
      
      # Don't add group variable to intake
      if (!"group" %in% names(df) && "subj_id" %in% names(df)) {
        
        df$group <- results$demographics$group[
          match(df$subj_id, results$demographics$subj_id)]
      }
      # Special case for modelling_data
      else if ("subjID" %in% names(df)) {
        df$group <- results$demographics$group[
          match(df$subjID, results$demographics$subj_id)]
      }
      return(df)
    })
    
    
    return(results)
}

#test <- parsing_fun("/Users/cleliaveith/2025_devaluation/data/raw/jatos_results_20251128080833.txt") h
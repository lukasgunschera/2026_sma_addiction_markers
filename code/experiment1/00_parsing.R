## ======================================================================================================================= ##
## Script:       DATA PARSING
## ======================================================================================================================= ##
## Authors:      Lukas Gunschera
## Contact:      l.gunschera@outlook.com
##
## Date created: 2025-01-25
## ======================================================================================================================= ##
##
## ======================================================================================================================= ##

## SETUP ====================================================================================================================

set.seed(777) # set seed for random processes

library(renv)
renv::restore() # reproducible environment

# load packages
pacman::p_load("here", "plyr", "psych", "dplyr", "tibble", "ggpubr", "magrittr", "labelled", "sjlabelled")

### Load custom functions ---------------------------------------------------------------------------------------------------
source(here::here("code", "experiment1", "functions", "fun_parsing.R"))

## LOAD DATA ================================================================================================================

# create object containing the names of all participant datafiles
data_files <- list.files(
  path = here::here("data", "experiment1", "raw"),
  recursive = TRUE,
  pattern = ".txt",
  full.names = TRUE
)

# create object containing all participant data
all_jatos_dat <- list()
for (i in seq_along(data_files)) {
  jatos_txt_file <- data_files[i]
  all_jatos_dat[[i]] <- jsonlite::fromJSON(sprintf(
    "[%s]",
    paste(
      readLines(jatos_txt_file, encoding = "UTF-8", warn = FALSE),
      collapse = ","
    )
  ))
}

### Parse all participant data ----------------------------------------------------------------------------------------------
data <- fun_parsing(file = data_files)

### Exclude Participants ----------------------------------------------------------------------------------------------------

# extract participant identifiers who failed the (hard) catch questions
f_hard <- data$questionnaires %>%
  filter(catch_questions_hard == "fail") %>%
  select(subj_id) %>%
  pull()

# extract participant identifiers who failed the (easy) catch questions
f_easy <- data$questionnaires %>%
  filter(catch_questions_easy == "fail") %>%
  select(subj_id) %>%
  pull()

subj_ids_to_exclude <- unique(c(f_hard, f_easy))

# return number of excluded subjects
paste0("Excluded participants: ", length(subj_ids_to_exclude))

# filter each data component for excluded participants
data$demographics <- subset(data$demographics, !(subj_id %in% subj_ids_to_exclude))
data$task_meta_data <- subset(data$task_meta_data, !(subj_id %in% subj_ids_to_exclude))
data$task_data <- subset(data$task_data, !(subj_id %in% subj_ids_to_exclude))
data$questionnaires <- subset(data$questionnaires, !(subj_id %in% subj_ids_to_exclude))

# save data
data %>% saveRDS(., here::here("data", "experiment1", "processed", "data.RDS"))

## CREATE CODEBOOK ==========================================================================================================

# extract labels and convert to tibble
codebook_names <- purrr::map_dfr(data, ~ enframe(get_label(.x)))

# compute descriptives and select relevant columns
codebook_descriptives <- purrr::map_dfr(
  data,
  ~ psych::describe(.x) %>%
    dplyr::as_tibble() %>%
    dplyr::select("n", "min", "max", "mean", "sd", "skew", "kurtosis")
)

# combine results
codebook <- cbind(codebook_names, codebook_descriptives)

# save codebook
saveRDS(codebook, here::here("output", "documentation", "codebook_exp1.RDS"))

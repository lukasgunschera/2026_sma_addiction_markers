## ======================================================================================================================= ##
## Script:       MODEL PREPROCESSING FUNCTION
## ======================================================================================================================= ##
## Authors:      Lukas Gunschera
## Contact:      l.gunschera@outlook.com
##
## Date created: 2025-02-10
## ======================================================================================================================= ##
##
## ======================================================================================================================= ##

modeling_cleandata <- function(data_to_model) {
  source(here::here("code", "experiment1", "functions", "fun_helper.R"))
  modelling_df <- list()

  data_modelling <- data_to_model[data_to_model$phase == "game", c("subj_id", "session_id", "trial", "offerEffort", "offerReward", "choice")]

  data_modelling <- data_modelling[complete.cases(data_modelling), ]

  # merge tiktok and netflix data across sessions (tiktok 1 and 2, netflix 1 and 2)
  data_modelling %<>%
    dplyr::rename(effort_a = offerEffort, amount_a = offerReward) |>
    dplyr::mutate(
      session_id = dplyr::case_when(
        grepl("tiktok", session_id, ignore.case = TRUE) ~ "tiktok",
        grepl("netflix", session_id, ignore.case = TRUE) ~ "netflix",
        TRUE ~ session_id # keep the original value if no match is found
      )
    )

  data_modelling$effort_a <- standardization(data_modelling$effort_a, 0, unique(data_modelling$effort_a))
  data_modelling$amount_a <- standardization(data_modelling$amount_a, 0, unique(data_modelling$amount_a))
  data_modelling <- cbind(data_modelling, "effort_b" = rep(0, dim(data_modelling)[1]), "amount_b" = rep(0, dim(data_modelling)[1]))

  modelling_df$tiktok <- data_modelling[data_modelling$session_id == "tiktok", ]
  modelling_df$netflix <- data_modelling[data_modelling$session_id == "netflix", ]

  return(modelling_df)
}

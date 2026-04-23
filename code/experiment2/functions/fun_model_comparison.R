## ======================================================================================================================= ##
## Script:       MODEL COMPARISON FUNCTION
## ======================================================================================================================= ##
## Authors:      Sara Mehrhof, Lukas Gunschera
## Contact:      sara.mehrhof@gmail.com
##
## Date created: 2025-02-10
## ======================================================================================================================= ##
##
## Conducts model comparison in a memory saving manner
##
## ======================================================================================================================= ##

## Parameters ===============================================================================================================
# @ model_paths: character vector with directory paths to models
# @ loo_paths: character vector with directory paths to loo fits
# @ model_names: character vector with model names
# @ LOO_plot: visualize model comparison based on loo?
# @ ELPD_plot: visualize model comparison based on elpd?

## Function: model_comparison ===============================================================================================

model_comparison <- function(model_paths = NULL,
                             loo_paths = NULL,
                             model_names = NULL,
                             LOO_plot = FALSE,
                             LOO_title = "Model comparision based on LOO",
                             LOO_col = "#fd9668",
                             ELPD_plot = FALSE,
                             ELPD_title = "Model comparision based on ELPD",
                             ELPD_col = "#9e2f7f",
                             LOO_ELPD_plot = TRUE,
                             LOO_ELPD_title = "LOO and ELPD model comparison",
                             LOO_ELPD_col = c("#fd9668", "#9e2f7f")) {
  library(ggplot2)
  library(ggpubr)

  loo_output <- list()

  res <- list()

  if (!is.null(loo_paths)) {
    for (i in seq_along(loo_paths)) {
      loo_output[[model_names[i]]] <- readRDS(loo_paths[i])
    }
  } else {
    for (i in seq_along(model_paths)) {
      model_fit <- readRDS(model_paths[i])
      if (is.null(model_names)) {
        loo_output[[model_fit$metadata()$model_name]] <- model_fit$loo()
      } else {
        loo_output[[model_names[i]]] <- model_fit$loo()
      }
    }
  }
  res$loo_output <- purrr::map(loo_output, function(x) x$estimates)

  if (LOO_plot) {
    model_comp_loo_dat <- data.frame(
      "model" = names(loo_output),
      "loo" = as.numeric(lapply(loo_output, function(x) x$estimates[3, 1])),
      "loo_sd" = as.numeric(lapply(loo_output, function(x) x$estimates[3, 2]))
    )

    model_comp_loo_plot <- model_comp_loo_dat %>%
      ggplot2::ggplot(., aes(x = model, y = loo)) +
      ggplot2::geom_bar(stat = "identity", position = position_dodge(), fill = LOO_col) +
      ggplot2::geom_errorbar(aes(ymin = loo - loo_sd, ymax = loo + loo_sd), width = .2, position = position_dodge(.9)) +
      # geom_text(aes(label = round(loo)), position = position_dodge(0.9), hjust=1.5, size = 3) +
      ggplot2::ylab("Leave-one-out information criterion (± SE)") +
      xlab("Model") +
      # ggplot2::ggtitle(LOO_title) + (remove title)
      ggplot2::coord_flip() +
      theme_bw(base_size = 10, base_line_size = 0.5, base_rect_size = 0.5, base_family = "sans") +
      ggplot2::theme(
        legend.position = "none",
        text = element_text(size = 10),
        plot.title = element_text(size = 10),
        axis.title = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        family = "sans",
        axis.text = element_text(size = 10),
        axis.title.y = element_text(vjust = +3),
        axis.title.x = element_text(vjust = -1)
      )

    res$LOO_plot <- model_comp_loo_plot
  }

  if (ELPD_plot) {
    model_comp_elpd_dat <- data.frame(
      "model" = names(loo_output),
      "elpd" = as.numeric(lapply(loo_output, function(x) x$estimates[1, 1])),
      "elpd_sd" = as.numeric(lapply(loo_output, function(x) x$estimates[1, 2]))
    )

    model_comp_elpd_plot <- ggplot2::ggplot(model_comp_elpd_dat, aes(x = model, y = elpd)) +
      ggplot2::geom_bar(stat = "identity", position = position_dodge(), fill = ELPD_col) +
      ggplot2::geom_errorbar(aes(ymin = elpd - elpd_sd, ymax = elpd + elpd_sd),
        width = .2,
        position = position_dodge(.9)
      ) +
      # geom_text(aes(label = round(elpd)), position = position_dodge(0.9), hjust=-0.5, size = 3) +
      ggplot2::ylab("Expected log posterior density (± SE)") +
      xlab("Model") +
      ggplot2::ggtitle(ELPD_title) +
      ggplot2::coord_flip() +
      ggplot2::theme(
        legend.position = "none",
        plot.title = element_text(size = 10),
        axis.title = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10)
      )

    res$ELPD_plot <- model_comp_elpd_plot
  }

  if (LOO_ELPD_plot) {
    model_comp_loo_elpd_dat <- data.frame(
      "model" = rep(names(loo_output), 2),
      "measure" = rep(c("loo", "elpd"), each = length(names(loo_output))),
      "loo_elpd" = c(
        as.numeric(lapply(loo_output, function(x) x$estimates[3, 1])),
        as.numeric(lapply(loo_output, function(x) x$estimates[1, 1]))
      ),
      "loo_elpd_sd" = c(
        as.numeric(lapply(loo_output, function(x) x$estimates[3, 2])),
        as.numeric(lapply(loo_output, function(x) x$estimates[1, 2]))
      )
    )

    model_comp_loo_elpd_plot <- ggplot2::ggplot(model_comp_loo_elpd_dat, aes(x = model, y = loo_elpd, fill = measure)) +
      ggplot2::geom_bar(stat = "identity", position = "identity", alpha = 0.8) +
      ggplot2::scale_fill_manual(values = LOO_ELPD_col) +
      ggplot2::geom_errorbar(aes(ymin = loo_elpd - loo_elpd_sd, ymax = loo_elpd + loo_elpd_sd),
        width = .2,
        position = "identity"
      ) +
      ggplot2::ylab("Information criterion (± SE)") +
      ggplot2::xlab("") +
      ggplot2::ggtitle(LOO_ELPD_title) +
      ggplot2::coord_flip() +
      ggplot2::labs(fill = "Measure") +
      ggplot2::theme(
        legend.position = "bottom",
        plot.title = element_text(size = 10),
        axis.title = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.key.size = unit(0.25, "cm")
      )

    indiv_plot <- annotate_figure(model_comp_loo_elpd_plot,
      top = ggpubr::text_grob(LOO_ELPD_title,
        face = "bold", size = 10
      )
    )

    res$LOO_ELPD_plot <- model_comp_loo_elpd_plot
  }

  return(res)
}

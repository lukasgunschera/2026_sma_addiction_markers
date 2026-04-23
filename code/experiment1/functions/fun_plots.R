## ======================================================================================================================= ##
## Script:       PLOT FUNCTIONS
## ======================================================================================================================= ##
## Authors:      Sara Mehrhof, Lukas Gunschera
## Contact:      sara.mehrhof@gmail.com
##
## Date created: 2025-02-10
## ======================================================================================================================= ##
##
## ======================================================================================================================= ##

## Plot: acceptance rate per effort and reward level ========================================================================
# @ task_data: Task data to generate plot for
# @ main_title: Main title for the generated plot
# @ direction: Plot effort by reward or reward by effort

task_plot <- function(
  task_dat,
  main_title = "Acceptance proportion by effort and reward level",
  direction = "effort_by_reward",
  plot_type = "bar_plot",
  arrange_cols = 2,
  arrange_rows = 2
) {
  library(ggplot2)
  library(ggpubr)

  # manage with other subject identifier
  if ("subjID" %in% names(task_dat) && !"subj_id" %in% names(task_dat)) {
    task_dat$subj_id <- task_dat$subjID
  }

  if ("effort_a" %in% names(task_dat) && !"offerEffort" %in% names(task_dat)) {
    task_dat$offerEffort <- task_dat$effort_a
  }

  if ("amount_a" %in% names(task_dat) && !"offerReward" %in% names(task_dat)) {
    task_dat$offerReward <- task_dat$amount_a
  }

  choiceMean_subj <- stats::aggregate(
    choice ~ subj_id + offerEffort + offerReward,
    FUN = function(x) {
      mean(x) * 100
    },
    dat = task_dat
  )
  choiceMean <- stats::aggregate(
    choice ~ offerEffort + offerReward,
    FUN = mean,
    dat = choiceMean_subj
  )
  choiceSE_subj <- stats::aggregate(
    choice ~ subj_id + offerEffort + offerReward,
    FUN = function(x) {
      (sd(x) / sqrt(length(x))) * 100
    },
    dat = task_dat
  )
  choiceSE_subj$choice[is.na(choiceSE_subj$choice)] <- 0
  choiceSE <- stats::aggregate(
    choice ~ offerEffort + offerReward,
    FUN = mean,
    dat = choiceSE_subj
  )

  choicesPlotDat <- cbind(choiceMean, choiceSE[, 3])
  colnames(choicesPlotDat) <- c("offerEffort", "offerReward", "mean", "se")
  choicesPlotDat$offerEffort <- as.factor(choicesPlotDat$offerEffort)
  choicesPlotDat$offerReward <- as.factor(choicesPlotDat$offerReward)
  choicesPlotDat <- choicesPlotDat[choicesPlotDat$offerReward != 999, ]

  choicePlot <- list()

  if (direction == "effort_by_reward") {
    rewards <- sort(unique(task_dat$offerReward))

    if (plot_type == "bar_plot") {
      for (i in 1:4) {
        choicePlot[[i]] <- ggplot(
          data = choicesPlotDat[choicesPlotDat$offerReward == rewards[i], ],
          aes(x = offerEffort, y = mean, fill = offerEffort)
        ) +
          ggplot2::geom_bar(stat = "summary", fun = "mean", alpha = 0.8) +
          ggplot2::geom_errorbar(
            aes(ymin = mean - se, ymax = mean + se),
            width = .2,
            position = position_dodge(.9)
          ) +
          scale_fill_viridis_d(option = "magma", begin = 0.15, end = 0.75, name = NULL) +
          ggplot2::theme(
            legend.position = "none",
            title = element_text(size = 10),
            axis.title = element_text(size = 10),
            axis.text.x = element_text(size = 10),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_text(size = 10),
            axis.ticks.x = element_blank()
          ) +
          ggplot2::ggtitle(paste("Reward Level ", i)) +
          ggplot2::scale_x_discrete(labels = c("1", "2", "3", "4")) +
          ggplot2::scale_y_continuous(breaks = c(0, 50, 100), limits = c(0, 100))
      }
    } else {
      choicePlot <- ggplot(
        data = choicesPlotDat,
        aes(x = offerEffort, y = mean, group = offerReward)
      ) +
        ggplot2::geom_point(
          aes(color = offerReward, shape = offerReward),
          position = position_dodge(0.2),
          size = 2.5
        ) +
        ggplot2::geom_line(
          aes(color = offerReward),
          position = position_dodge(0.2),
          size = 0.75
        ) +
        ggplot2::geom_errorbar(
          aes(color = offerReward, ymin = mean - se, ymax = mean + se),
          width = .2,
          position = position_dodge(0.2)
        ) +
        scale_colour_viridis_d(option = "magma", begin = 0.15, end = 0.75, name = "Reward level", labels = c("1", "2", "3", "4")) +
        ggplot2::scale_shape_manual(
          values = c(15, 16, 17, 18),
          name = "Reward level",
          labels = c("1", "2", "3", "4")
        ) +
        ggplot2::theme(
          legend.position = "right",
          title = element_text(size = 12),
          axis.title = element_text(size = 12),
          axis.text.x = element_text(size = 12),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.ticks.x = element_blank()
        ) +
        ggplot2::scale_x_discrete(labels = c("1", "2", "3", "4")) +
        ggplot2::scale_y_continuous(
          breaks = c(0, 25, 50, 75, 100),
          limits = c(0, 100)
        ) +
        ggplot2::xlab("Effort Level") +
        ggplot2::ylab("% Accepted (SE)")
    }
  } else {
    efforts <- sort(unique(task_dat$offerEffort))
    for (i in 1:4) {
      choicePlot[[i]] <- ggplot2::ggplot(
        data = choicesPlotDat[choicesPlotDat$offerEffort == efforts[i], ],
        aes(x = offerReward, y = mean, fill = offerReward)
      ) +
        ggplot2::geom_bar(stat = "summary", fun = "mean", alpha = 0.8) +
        ggplot2::geom_errorbar(
          aes(ymin = mean - se, ymax = mean + se),
          width = .2,
          position = position_dodge(.9)
        ) +
        scale_fill_viridis_d(option = "magma", begin = 0.15, end = 0.75, name = NULL) +
        ggplot2::theme(
          legend.position = "none",
          plot.title = element_text(size = 8),
          axis.title = element_text(size = 6),
          axis.text.x. = element_text(size = 6),
          axis.text.y = element_text(size = 6)
        ) +
        ggplot2::ggtitle(paste("Effort Level ", i)) +
        ggplot2::scale_x_discrete(labels = c("2", "3", "4", "5")) +
        ggplot2::scale_y_continuous(
          breaks = c(0, 50, 100),
          limits = c(0, 100)
        ) +
        ggplot2::xlab("Reward Level") +
        ggplot2::ylab("% Accepted")
    }
  }

  if (arrange_cols != 0 | arrange_rows != 0) {
    choicePlot <- ggpubr::ggarrange(
      choicePlot[[1]] + rremove("xlab"),
      choicePlot[[2]] + rremove("ylab") + rremove("xlab"),
      choicePlot[[3]],
      choicePlot[[4]] + rremove("ylab"),
      ncol = arrange_cols,
      nrow = arrange_rows,
      common.legend = FALSE
    )

    choicePlot <- ggpubr::annotate_figure(
      choicePlot,
      top = text_grob(main_title, face = "bold", size = 18),
      left = text_grob("% Accepted (SE)", size = 10, rot = 90),
      bottom = text_grob("Effort level", size = 10)
    )
  }

  return(choicePlot)
}


## Plot: raincloud plot =====================================================================================================
# @ dat: Data to plot
# @ title: Main title for the generated plot
# @ xlab: x axis label (when direction = horizontal this will be the y axis)
# @ ylab: y axis label (when direction = horizontal this will be the x axis)
# @ predictor_var: name of predictor variable
# @ outcome_var: name of outcome variable
# @ predictor_tick_lab: tick labels for predictor variable
# @ col: color(s) to use
# @ direction: plot horizontally or vertically?
# @ include_grouping: plot grouping variable?
# @ group_var: (if include_grouping is TRUE) name of grouping variable
# @ legendlab: (if include_grouping is TRUE) title of group legend

raincloud_plot <- function(
  dat,
  title,
  xlab,
  ylab,
  predictor_var,
  outcome_var,
  predictor_tick_lab,
  col,
  direction = "vertical",
  include_grouping = FALSE,
  group_var,
  legendlab = "",
  scale_seq = c(0, 6, 2)
) {
  require(ggplot2)
  require(PupillometryR)

  if (include_grouping == FALSE) {
    rain_plot <- ggplot2::ggplot(
      data = dat,
      aes_string(y = outcome_var, x = predictor_var, fill = predictor_var)
    ) +
      ggplot2::geom_flat_violin(
        position = position_nudge(x = 0.2, y = 0),
        alpha = 0.6
      ) +
      ggplot2::geom_point(
        aes_string(y = outcome_var, color = predictor_var),
        position = position_jitter(width = 0.15),
        size = 0.75,
        alpha = 0.6
      ) +
      ggplot2::geom_boxplot(width = 0.1, outlier.shape = NA, alpha = 0.5) +
      ggplot2::guides(fill = "none", color = "none") +
      ggplot2::labs(title = title, x = xlab, y = ylab)
  } else {
    rain_plot <- ggplot2::ggplot(
      data = dat,
      aes_string(y = outcome_var, x = predictor_var, fill = group_var)
    ) +
      geom_flat_violin(position = position_nudge(x = 0.2, y = 0), alpha = 0.6) +
      ggplot2::geom_point(
        aes_string(y = outcome_var, color = group_var),
        position = position_jitter(width = 0.15),
        size = 0.75,
        alpha = 0.6
      ) +
      ggplot2::geom_boxplot(width = 0.1, outlier.shape = NA, alpha = 0.5) +
      ggplot2::guides(
        color = guide_legend(override.aes = list(size = 10)),
        fill = "none"
      ) +
      ggplot2::labs(title = title, x = xlab, y = ylab, color = legendlab)
  }
  rain_plot <- rain_plot +
    ggplot2::scale_fill_manual(values = col) +
    ggplot2::scale_color_manual(values = col) +
    ggplot2::scale_x_discrete(
      expand = c(0.1, 0.1),
      labels = predictor_tick_lab
    ) +
    ggplot2::scale_y_continuous(
      breaks = seq(scale_seq[1], scale_seq[2], scale_seq[3]),
      limits = c(scale_seq[1], scale_seq[2])
    ) +
    ggplot2::theme(
      plot.title = element_text(size = 10),
      axis.title = element_text(size = 10),
      axis.title.x = element_text(size = 10),
      axis.title.y = element_text(size = 10),
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 10),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 10),
      legend.position = "top"
    )

  if (direction == "horizontal") {
    rain_plot <- rain_plot + ggplot2::coord_flip()
  }
  rain_plot
}


## Plot: Cumulative lag plot ================================================================================================
# @ dat: Data to plot
# @ var_labels: Variable names for legend
# @ title: Main title for the generated plot
# @ x_label: Label for x-axis
# @ y_label: Label for y-axis
# @ col: Color to use

cum_plot <- function(
  dat,
  var_labels,
  title,
  x_label,
  y_label,
  y_lim,
  col,
  shape = 1
) {
  cum_plot <- ggplot2::ggplot(
    dat,
    aes(
      x = trial,
      y = mean_value,
      group = variable,
      color = variable
    )
  ) +
    ggplot2::geom_line() +
    ggplot2::geom_point(size = 0.75, shape = shape) +
    ggplot2::geom_ribbon(
      aes(
        ymin = se_lower,
        ymax = se_upper,
        x = trial,
        group = variable,
        color = variable,
        fill = variable
      ),
      outline.type = "both",
      alpha = 0.25
    ) +
    ggplot2::scale_fill_manual(
      values = col,
      name = " ",
      labels = var_labels
    ) +
    ggplot2::scale_color_manual(
      values = col,
      name = " ",
      labels = var_labels
    ) +
    ggplot2::labs(
      title = title,
      x = x_label,
      y = y_label
    ) +
    ggplot2::ylim(y_lim) +
    ggplot2::theme(
      plot.title = element_text(size = 12, hjust = 0.5, face = "bold"),
      axis.title = element_text(size = 12),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 12)
    )

  cum_plot
}

## Plot: posterior predictive checks ========================================================================================
# @ ppc_dat: PPC data to visualize (output from "posterior_predictions" function)
# @ group_plot: Group level plot (compares observed and predicted group-level
# acceptance proportions per effort and reward level)?
# @ indiv_plot: Individual level plot (observed vs. predicted acceptance
# proportions on a subject level for each effort and reward level)?

ppc_plots <- function(
  ppc_dat,
  group_plot = TRUE,
  indiv_plot = TRUE,
  group_plot_title = "Observed vs. model predicted acceptance proportions",
  indiv_plot_title = "Subject wise observed vs. model predicted acceptance proportions ",
  show_legend = TRUE # New argument to control legend display
) {
  require(ggplot2)
  require(ggpubr)

  amount <- sort(unique(ppc_dat$posterior_predictions_trial_type$offerReward))

  res_plots <- list()

  # Plot 1 - group wise
  if (group_plot == TRUE) {
    obs_mean <- stats::aggregate(
      ppc_dat$posterior_predictions_trial_type$observation ~
        ppc_dat$posterior_predictions_trial_type$offerEffort +
        ppc_dat$posterior_predictions_trial_type$offerReward,
      FUN = function(x) {
        mean(x) * 100
      }
    )
    obs_se <- stats::aggregate(
      ppc_dat$posterior_predictions_trial_type$observation ~
        ppc_dat$posterior_predictions_trial_type$offerEffort +
        ppc_dat$posterior_predictions_trial_type$offerReward,
      FUN = function(x) {
        (sd(x) / sqrt(length(x))) * 100
      }
    )

    observed <- cbind(obs_mean, obs_se[, 3])
    colnames(observed) <- c("offerEffort", "offerReward", "mean", "se")
    observed$offerEffort <- as.factor(observed$offerEffort)
    observed$offerReward <- as.factor(observed$offerReward)

    pred_mean <- stats::aggregate(
      ppc_dat$posterior_predictions_trial_type$prediction_mean ~
        ppc_dat$posterior_predictions_trial_type$offerEffort +
        ppc_dat$posterior_predictions_trial_type$offerReward,
      FUN = function(x) {
        mean(x) * 100
      }
    )
    pred_se <- stats::aggregate(
      ppc_dat$posterior_predictions_trial_type$prediction_mean ~
        ppc_dat$posterior_predictions_trial_type$offerEffort +
        ppc_dat$posterior_predictions_trial_type$offerReward,
      FUN = function(x) {
        (sd(x) / sqrt(length(x))) * 100
      }
    )
    predicted <- cbind(pred_mean, pred_se[, 3])
    colnames(predicted) <- c("offerEffort", "offerReward", "mean", "se")
    predicted$offerEffort <- as.factor(predicted$offerEffort)
    predicted$offerReward <- as.factor(predicted$offerReward)

    group_plot_dat <- rbind(observed, predicted)
    group_plot_dat <- cbind(
      group_plot_dat,
      "group" = paste(
        rep(c("pred", "real"), each = 16),
        rep(1:4, 4),
        rep(1:4, each = 4),
        sep = ""
      )
    )

    group_plot <- list()

    for (i in 1:4) {
      p <- ggplot2::ggplot(
        group_plot_dat[group_plot_dat$offerReward == amount[i], ],
        aes(x = offerEffort, y = mean, fill = group)
      ) +
        ggplot2::geom_bar(
          stat = "identity",
          position = position_dodge(),
          alpha = 0.9
        ) +
        ggplot2::geom_errorbar(
          aes(ymin = mean - se, ymax = mean + se),
          width = .3,
          position = position_dodge(0.95)
        ) +
        ggplot2::scale_fill_manual(
          values = c(
            "#fed1ab", # Group 1 Predicted
            "#faa392", # Group 2 Predicted
            "#cf5db6", # Group 3 Predicted
            "#a635d9", # Group 4 Predicted
            "#febb81", # Group 1 Observed
            "#f8765c", # Group 2 Observed
            "#982d80", # Group 3 Observed
            "#5f187f" # Group 4 Observed
          ),
          labels = c(
            "Predicted",
            "Predicted",
            "Predicted",
            "Predicted",
            "Observed",
            "Observed",
            "Observed",
            "Observed"
          )
        ) +
        ggplot2::ylab("% Accepted") +
        xlab("Effort level") +
        ggplot2::ggtitle(paste("Reward Level", i)) +
        ggplot2::scale_x_discrete(labels = 1:4) +
        ggplot2::theme(
          plot.title = element_text(size = 9),
          axis.title = element_text(size = 9),
          axis.text.x = element_text(size = 9),
          axis.text.y = element_text(size = 9),
          legend.title = element_text(size = 9),
          legend.text = element_text(size = 9)
        ) +
        theme_bw()

      # Conditionally add legend guides
      if (show_legend) {
        p <- p + ggplot2::guides(fill = guide_legend(
          title = " ",
          nrow = 2,
          byrow = TRUE
        ))
      } else {
        p <- p + theme(legend.position = "none")
      }

      group_plot[[i]] <- p
    }

    group_plot <- ggpubr::ggarrange(
      group_plot[[1]],
      group_plot[[2]],
      group_plot[[3]],
      group_plot[[4]],
      ncol = 2,
      nrow = 2,
      common.legend = show_legend, # Only use common legend if showing legend
      legend = if (show_legend) "bottom" else "none"
    )

    group_plot <- ggpubr::annotate_figure(
      group_plot,
      top = ggpubr::text_grob(group_plot_title, face = "bold", size = 14)
    )

    res_plots[["group_plot"]] <- group_plot
  }

  # Plot 2 - subject wise
  if (indiv_plot == TRUE) {
    # plot by effort level
    indiv_plot_effort_dat <- ppc_dat$posterior_predictions_effort
    indiv_plot_effort_dat$offerEffort <- as.factor(indiv_plot_effort_dat$offerEffort)

    R_squared_effort <- round(
      cor(
        indiv_plot_effort_dat$observation,
        indiv_plot_effort_dat$prediction_mean
      )^2,
      3
    )

    indiv_plot_effort <- ggplot2::ggplot(
      indiv_plot_effort_dat,
      aes(x = observation, y = prediction_mean, color = offerEffort)
    ) +
      ggplot2::geom_point(size = 2, alpha = 0.45) +
      ggplot2::geom_errorbar(
        aes(ymin = prediction_hdi_lower, ymax = prediction_hdi_higher),
        width = .025,
        alpha = 0.1
      ) +
      ggplot2::scale_color_manual(
        values = c("#febb81", "#f8765c", "#982d80", "#5f187f"),
        labels = 1:4
      ) +
      ggplot2::xlim(0, 1) +
      ylim(0, 1) +
      ggplot2::geom_abline(linetype = 3) +
      ggplot2::ylab("Predicted (± 95% HDI)") +
      xlab("Observed") +
      ggplot2::ggtitle(bquote(
        "Across effort levels:" ~
          R^{
            2
          } ==
            .(R_squared_effort)
      )) +
      theme_bw() +
      ggplot2::theme(
        plot.title = element_text(size = 9),
        axis.title = element_text(size = 9),
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 9)
      )

    # Conditionally add legend
    if (show_legend) {
      indiv_plot_effort <- indiv_plot_effort +
        ggplot2::guides(color = guide_legend(title = "Effort/Reward level"))
    } else {
      indiv_plot_effort <- indiv_plot_effort + theme(legend.position = "none")
    }

    # plot by reward level
    indiv_plot_reward_dat <- ppc_dat$posterior_predictions_reward
    indiv_plot_reward_dat$offerReward <- as.factor(indiv_plot_reward_dat$offerReward)

    R_squared_reward <- round(
      cor(
        indiv_plot_reward_dat$observation,
        indiv_plot_reward_dat$prediction_mean
      )^2,
      3
    )

    indiv_plot_reward <- ggplot(
      indiv_plot_reward_dat,
      aes(x = observation, y = prediction_mean, color = offerReward)
    ) +
      ggplot2::geom_point(size = 2, alpha = 0.45) +
      ggplot2::geom_errorbar(
        aes(ymin = prediction_hdi_lower, ymax = prediction_hdi_higher),
        width = .025,
        alpha = 0.1
      ) +
      ggplot2::scale_color_manual(
        values = c("#febb81", "#f8765c", "#982d80", "#5f187f"),
        labels = 1:4
      ) +
      xlim(0, 1) +
      ylim(0, 1) +
      ggplot2::geom_abline(linetype = 3) +
      ggplot2::ylab("Predicted (± 95% HDI)") +
      xlab("Observed") +
      ggplot2::ggtitle(bquote(
        "Across reward levels:" ~
          R^{
            2
          } ==
            .(R_squared_reward)
      )) +
      theme_bw() +
      ggplot2::theme(
        plot.title = element_text(size = 9),
        axis.title = element_text(size = 9),
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 9)
      )

    # Conditionally add legend
    if (show_legend) {
      indiv_plot_reward <- indiv_plot_reward +
        ggplot2::guides(color = guide_legend(title = "Effort/Reward level"))
    } else {
      indiv_plot_reward <- indiv_plot_reward + theme(legend.position = "none")
    }

    indiv_plot <- ggpubr::ggarrange(
      indiv_plot_effort,
      indiv_plot_reward,
      ncol = 2,
      nrow = 1,
      common.legend = show_legend, # Only use common legend if showing legend
      legend = if (show_legend) "bottom" else "none"
    )

    indiv_plot <- ggpubr::annotate_figure(
      indiv_plot,
      top = ggpubr::text_grob(indiv_plot_title, face = "bold", size = 10)
    )

    res_plots[["indiv_plot"]] <- indiv_plot
  }
  return(res_plots)
}

## Plot: parameter recovery =================================================================================================
# @ recovery_data: tibble with underlying and recovered parameter values
# @ plot_title: title for plot
# @ col: color to use

params_recovery_plot <- function(recovery_data, plot_title, col) {
  params_recovery_plot <- ggplot2::ggplot(
    recovery_data,
    aes(
      x = real,
      y = recovered,
      color = col
    )
  ) +
    ggplot2::geom_point(size = 2, alpha = 0.6, color = col) +
    ggplot2::geom_abline(linetype = 3) +
    ggplot2::ylab("Recovered parameter estimates") +
    xlab("Underlying parameters") +
    ggplot2::labs(title = plot_title) +
    theme_bw() +
    ggplot2::theme(
      plot.title = element_text(size = 10),
      plot.subtitle = element_text(size = 10),
      axis.title = element_text(size = 10),
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 10),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 10),
      legend.position = "none"
    )

  return(params_recovery_plot)
}

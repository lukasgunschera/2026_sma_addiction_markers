## ======================================================================================================================= ##
## Script:    MANUSCRIPT FIGURES
## ======================================================================================================================= ##
## Authors:   Lukas J. Gunschera
## Date:      Mon Mar 30 13:29:19 2026
## ======================================================================================================================= ##
##
## ======================================================================================================================= ##

# setup
source(here::here("code", "00_setup.R"))
pacman::p_load(ggpubr, png, grid, ggplot2, cowplot, magick, dplyr)

# load rds plots
gg_wantlike_exp1 <- readRDS(here::here("output", "rds", "gg_wantlike_exp1.rds"))
gg_wantlike_exp2 <- readRDS(here::here("output", "rds", "gg_wantlike_exp2.rds"))
gg_want_exp1 <- readRDS(here::here("output", "rds", "gg_wanting_exp1.rds"))
gg_want_exp2 <- readRDS(here::here("output", "rds", "gg_wanting_exp2.rds"))
gg_like_exp1 <- readRDS(here::here("output", "rds", "gg_liking_exp1.rds"))
gg_like_exp2 <- readRDS(here::here("output", "rds", "gg_liking_exp2.rds"))

gg_wlbsmas_exp1 <- readRDS(here::here("output", "rds", "gg_wlbsmas_exp1.rds"))
gg_wlbsmas_exp2 <- readRDS(here::here("output", "rds", "gg_wlbsmas_exp2.rds"))
gg_dsbsmas_exp2 <- readRDS(here::here("output", "rds", "gg_dsbsmas_exp2.rds"))

gg_taskbehaviour_exp1 <- readRDS(here::here("output", "rds", "gg_taskbehaviour_exp1.rds"))
gg_taskbehaviour_exp2 <- readRDS(here::here("output", "rds", "gg_taskbehaviour_exp2.rds"))

gg_taskbehaviour_bar_exp1 <- readRDS(here::here("output", "rds", "gg_taskbehaviour_exp1_bar.rds"))
gg_taksbehaviour_bar_exp2 <- readRDS(here::here("output", "rds", "gg_taskbehaviour_exp2_bar.rds"))

gg_kekr_distributions_exp1 <- readRDS(here::here("output", "rds", "kekr_parameter_distributions_exp1.rds"))
gg_kekr_distributions_exp2 <- readRDS(here::here("output", "rds", "kekr_parameter_distributions_exp2.rds"))

# arranged liking plot
ggpubr::ggarrange(
  gg_like_exp1, gg_like_exp2,
  nrow = 1, ncol = 2, align = "hv", labels = c("A", "B"), font.label = list(size = 16, face = "bold")
) |>
  ggplot2::ggsave(
    path = here::here("output", "figures", "manuscript"), filename = "liking.png",
    device = "png", width = 12, height = 4, dpi = 800, bg = "white"
  )

# arranged wanting plot
ggpubr::ggarrange(
  gg_want_exp1, gg_want_exp2,
  nrow = 1, ncol = 2, align = "hv", labels = c("A", "B"), font.label = list(size = 16, face = "bold")
) |>
  ggplot2::ggsave(
    path = here::here("output", "figures", "manuscript"), filename = "wanting.png",
    device = "png", width = 12, height = 4, dpi = 800, bg = "white"
  )

# arranged wanting-liking plot
ggpubr::ggarrange(
  gg_wantlike_exp1, gg_wantlike_exp2,
  nrow = 1, ncol = 2, align = "hv", labels = c("A", "B"), font.label = list(size = 16, face = "bold")
) |>
  ggplot2::ggsave(
    path = here::here("output", "figures", "manuscript"), filename = "wantlike.png",
    device = "png", width = 12, height = 4, dpi = 800, bg = "white"
  )

# arranged wanting-liking BSMAS correlation plot
ggpubr::ggarrange(
  gg_wlbsmas_exp1, gg_wlbsmas_exp2, gg_dsbsmas_exp2,
  nrow = 1, ncol = 3, align = "hv", labels = c("A", "B"), font.label = list(size = 16, face = "bold")
) |>
  ggplot2::ggsave(
    path = here::here("output", "figures", "manuscript"), filename = "wlbsmas.png",
    device = "png", width = 16, height = 4, dpi = 800, bg = "white"
  )

# arranged task behaviour plot
ggpubr::ggarrange(
  gg_taskbehaviour_exp1, gg_taskbehaviour_exp2 + labs(y = ""),
  common.legend = TRUE, legend = "right",
  nrow = 1, ncol = 2, align = "hv", labels = c("A", "B"), font.label = list(size = 16, face = "bold")
) |>
  ggplot2::ggsave(
    path = here::here("output", "figures", "manuscript"), filename = "taskbehaviour.png",
    device = "png", width = 12, height = 6, dpi = 800, bg = "white"
  )

# arrange task behaviour bar plot
ggpubr::ggarrange(
  gg_taskbehaviour_bar_exp1 + labs(y = ""), gg_taksbehaviour_bar_exp2 + scale_y_continuous(name = NULL) + labs(y = NULL),
  common.legend = TRUE, legend = "right",
  nrow = 1, ncol = 2, align = "hv", labels = c("A", "B"), font.label = list(size = 16, face = "bold")
) |>
  ggplot2::ggsave(
    path = here::here("output", "figures", "manuscript"), filename = "taskbehaviour_bar.png",
    device = "png", width = 12, height = 6, dpi = 800, bg = "white"
  )

# parameter distributions
ggpubr::ggarrange(
  gg_kekr_distributions_exp1, gg_kekr_distributions_exp2,
  nrow = 1, ncol = 2, align = "hv", labels = c("", ""), font.label = list(size = 16, face = "bold")
) |>
  ggplot2::ggsave(
    path = here::here("output", "figures", "manuscript"), filename = "kekr_parameter_distributions.png",
    device = "png", width = 12, height = 6, dpi = 800, bg = "white"
  )

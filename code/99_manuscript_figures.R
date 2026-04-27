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
gg_want_group_exp2 <- readRDS(here::here("output", "rds", "gg_wanting_group_exp2.rds"))
gg_like_exp1 <- readRDS(here::here("output", "rds", "gg_liking_exp1.rds"))
gg_like_exp2 <- readRDS(here::here("output", "rds", "gg_liking_exp2.rds"))
gg_devalsens_exp2 <- readRDS(here::here("output", "rds", "gg_devaluation_sensitivity_exp2.rds"))

gg_wlbsmas_exp1 <- readRDS(here::here("output", "rds", "gg_wlbsmas_exp1.rds"))
gg_wlbsmas_exp2 <- readRDS(here::here("output", "rds", "gg_wlbsmas_exp2.rds"))
gg_dsbsmas_exp2 <- readRDS(here::here("output", "rds", "gg_dsbsmas_exp2.rds"))
gg_wantbsmas_exp1 <- readRDS(here::here("output", "rds", "gg_wantingbsmas_exp1.rds"))
gg_wantbsmas_exp2 <- readRDS(here::here("output", "rds", "gg_wantingbsmas_exp2.rds"))

gg_taskbehaviour_exp1 <- readRDS(here::here("output", "rds", "gg_taskbehaviour_exp1.rds"))
gg_taskbehaviour_exp2 <- readRDS(here::here("output", "rds", "gg_taskbehaviour_exp2.rds"))

gg_taskbehaviour_bar_exp1 <- readRDS(here::here("output", "rds", "gg_taskbehaviour_exp1_bar.rds"))
gg_taksbehaviour_bar_exp2 <- readRDS(here::here("output", "rds", "gg_taskbehaviour_exp2_bar.rds"))

gg_kekr_distributions_exp1 <- readRDS(here::here("output", "rds", "kekr_parameter_distributions_exp1.rds"))
gg_kekr_distributions_exp2 <- readRDS(here::here("output", "rds", "kekr_parameter_distributions_exp2.rds"))

# Load images as raster grobs
img_a <- rasterGrob(readPNG(here::here("output", "2025_devaluation_procedure.png")), interpolate = TRUE)
img_b <- rasterGrob(readPNG(here::here("output", "2025_wantingliking_procedure.png")), interpolate = TRUE)

## ARRANGE PLOTS ============================================================================================================

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
    device = "png", width = 12, height = 8, dpi = 800, bg = "white"
  )

# arranged wanting-liking BSMAS correlation plot
axis_theme <- ggplot2::theme(
  axis.title = ggplot2::element_text(size = 24),
  axis.text = ggplot2::element_text(size = 22),
  legend.text = ggplot2::element_text(size = 20),
)


ggpubr::ggarrange(
  gg_wantbsmas_exp1 + axis_theme + ggplot2::scale_x_continuous(breaks = c(-4, -2, 0, 2, 4), limits = c(-5, 5)),
  gg_wantbsmas_exp2 + axis_theme + ggplot2::scale_x_continuous(breaks = c(-4, -2, 0, 2, 4), limits = c(-5, 5)) + labs(x = "Wanting"),
  gg_wlbsmas_exp1 + axis_theme + ggplot2::scale_x_continuous(breaks = c(-4, -2, 0, 2, 4), limits = c(-5, 5)),
  gg_wlbsmas_exp2 + axis_theme + ggplot2::scale_x_continuous(breaks = c(-4, -2, 0, 2, 4), limits = c(-5, 5)),
  gg_dsbsmas_exp2 + axis_theme + ggplot2::scale_x_continuous(breaks = c(-8, -4, 0, 4, 8)),
  nrow = 2, ncol = 3, align = "hv", labels = c("A", "B", "C", "D", "E"), font.label = list(size = 16, face = "bold")
) |>
  ggplot2::ggsave(
    path = here::here("output", "figures", "manuscript"), filename = "wlbsmas.png",
    device = "png", width = 18, height = 12, dpi = 800, bg = "white"
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

# procedures of experiments
p1 <- ggplot() +
  annotation_custom(img_a) +
  theme_void() +
  theme(plot.margin = margin(t = 20)) # bottom margin in pts

p2 <- ggplot() +
  annotation_custom(img_b) +
  theme_void() +
  theme(plot.margin = margin(b = 20))

ggarrange(p2, p1,
  labels = c("A", "B"),
  ncol = 1,
  font.label = list(size = 14, face = "bold")
) |>
  ggplot2::ggsave(
    path = here::here("output", "figures", "manuscript"), filename = "procedures.png",
    device = "png", dpi = 800, bg = "white", width = 6, height = 8
  )

# large arranged plot
ggpubr::ggarrange(
  gg_want_exp1, gg_want_exp2, gg_want_group_exp2, gg_wantlike_exp1, gg_wantlike_exp2, gg_devalsens_exp2,
  align = "hv", labels = c("A", "B", "C", "D", "E", "F"), font.label = list(size = 16, face = "bold")
) |>
  ggplot2::ggsave(
    path = here::here("output", "figures", "manuscript"), filename = "large_arranged_plot.png",
    device = "png", width = 16, height = 12, dpi = 1000, bg = "white"
  )

# Define shared theme for larger axis labels
axis_theme <- ggplot2::theme(
  axis.title = ggplot2::element_text(size = 18),
  axis.text = ggplot2::element_text(size = 16),
  legend.text = ggplot2::element_text(size = 16),
)

ggpubr::ggarrange(
  gg_want_exp1 + axis_theme + ggplot2::scale_x_continuous(breaks = c(-6, -3, 0, 3, 6), limits = c(-6, 6)),
  gg_want_exp2 + axis_theme + ggplot2::scale_x_continuous(breaks = c(-6, -3, 0, 3, 6), limits = c(-6, 6)),
  gg_want_group_exp2 + axis_theme + ggplot2::scale_x_continuous(breaks = c(-6, -3, 0, 3, 6), limits = c(-6, 6)),
  gg_wantlike_exp1 + axis_theme + ggplot2::scale_x_continuous(breaks = c(-4, -2, 0, 2, 4), limits = c(-4, 4)),
  gg_wantlike_exp2 + axis_theme + ggplot2::scale_x_continuous(breaks = c(-4, -2, 0, 2, 4), limits = c(-4, 4)),
  gg_devalsens_exp2 + axis_theme + ggplot2::scale_x_continuous(breaks = c(-6, -3, 0, 3, 6)),
  align = "hv",
  labels = c("A", "B", "C", "D", "E", "F"),
  font.label = list(size = 20, face = "bold")
) |>
  ggplot2::ggsave(
    path = here::here("output", "figures", "manuscript"),
    filename = "large_arranged_plot.png",
    device = "png", width = 18, height = 14, dpi = 1000, bg = "white"
  )

## ====================================================================================================================== ##
## Script:    SETUP
## ====================================================================================================================== ##
## Authors:   Lukas J. Gunschera
## Date:      Wed Apr  1 15:56:41 2026
## ====================================================================================================================== ##
## Sourceable setup script. Controls which sections run via the `firstrun` flag.
##
## USAGE:
##   # First time only — installs packages, creates folders & .gitignores, imports fonts
##   firstrun <- TRUE;  source(here::here("00_setup.R"))
##
##   # Every subsequent session — loads packages, sets options & plot defaults
##   firstrun <- FALSE; source(here::here("00_setup.R"))
##   — or simply —
##   source(here::here("00_setup.R"))   # defaults to firstrun = FALSE
##
## SECTION SCHEDULE:
##   Section                   │ firstrun = TRUE │ firstrun = FALSE
##   ──────────────────────────┼─────────────────┼─────────────────
##   renv::restore()           │       ✓         │
##   Folder structure          │       ✓         │
##   Package installation      │       ✓         │
##   Package loading           │       ✓         │        ✓
##   extrafont::font_import()  │       ✓         │
##   extrafont::loadfonts()    │       ✓         │        ✓
##   Global options & seed     │       ✓         │        ✓
##   Plot defaults & themes    │       ✓         │        ✓
##   Source custom functions   │       ✓         │        ✓
##   .gitignore creation       │       ✓         │
## ====================================================================================================================== ##

# resolve firstrun — allows bare source() calls to default safely
if (!exists("firstrun")) {
  firstrun <- FALSE
  message("[setup] `firstrun` not set — defaulting to FALSE (skipping install/scaffold steps)")
}

stopifnot("`firstrun` must be TRUE or FALSE" = is.logical(firstrun) && length(firstrun) == 1)
message("[setup] Running with firstrun = ", firstrun)

## 1. ENVIRONMENT [first run only] =========================================================================================


library(renv)

if (firstrun) {
  library(renv)
  renv::restore()
}

options(digits = 7)
set.seed(777)

library(here)
here::i_am("renv.lock") # anchor the project root — always needed

## 2. FOLDER STRUCTURE [first run only] ====================================================================================

if (firstrun) {
  source(here::here("code", "functions", "fun_folderstructure.R"))

  folders <- c(
    "code/stan",
    "code/functions",
    "code/pilot",
    "code/experiment1/functions",
    "code/experiment2/functions",
    "data/experiment1/modelling/parameters",
    "data/experiment2/modelling/parameters",
    "data/experiment1/raw",
    "data/experiment2/raw",
    "data/experiment1/processed",
    "data/experiment2/processed",
    "data/pilot",
    "output/rds",
    "output/documentation",
    "output/figures/manuscript",
    "output/figures/experiment1/analyses",
    "output/figures/experiment2/analyses",
    "output/figures/pilot/experiment1/modelfit",
    "output/figures/pilot/experiment2/modelfit",
    "output/rds"
  )

  create_project_structure(folders)
}


## 3. DEPENDENCIES =========================================================================================================

packages <- c(
  # --- Data wrangling & utilities ---
  "dplyr",
  "tidyr",
  "tidyverse",
  "purrr",
  "pacman",
  "magrittr",
  "stringr",
  "lubridate",
  "janitor",
  "tibble",
  "zoo",
  "glue",
  "readr",
  "writexl",
  "labelled",
  "sjlabelled",

  # --- Statistics & modelling ---
  "nlme",
  "plm",
  "Hmisc",
  "psych",
  "rstatix",
  "effsize",
  "MatchIt",
  "moments",
  "BayesFactor",
  "rstanarm",

  # --- Bayesian modelling ---
  "brms",
  "bayesplot",
  "posterior",
  "tidybayes",
  "bayestestR",

  # --- Visualisation ---
  "ggplot2",
  "ggExtra",
  "ggpubr",
  "GGally",
  "ggcorrplot",
  "ggplotify",
  "ggpmisc",
  "ggwaffle",
  "ggpattern",
  "magick",
  "patchwork",
  "PupillometryR",
  "corrplot",
  "viridis",
  "ragg",

  # --- Fonts & theming ---
  "extrafont",
  "systemfonts",

  # --- Tables & reporting ---
  "gt",
  "gtExtras",
  "knitr",
  "kableExtra"
)

### 3a. Installation [first run only] --------------------------------------------------------------------------------------

if (firstrun) {
  # CRAN packages
  new_packages <- packages[!packages %in% installed.packages()[, "Package"]]
  if (length(new_packages)) install.packages(new_packages)

  # styler
  if (!requireNamespace("styler", quietly = TRUE)) install.packages("styler", verbose = TRUE)

  # devtools (needed for GitHub installs below)
  if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")

  # ggwaffle — GitHub only
  if (!requireNamespace("ggwaffle", quietly = TRUE)) {
    devtools::install_github("liamgilbey/ggwaffle")
  }

  # cmdstanr — Stan r-universe
  if (!requireNamespace("cmdstanr", quietly = TRUE)) {
    install.packages("cmdstanr", repos = c("https://stan-dev.r-universe.dev", getOption("repos")))
  }

  # hBayesDM — GitHub only
  if (!requireNamespace("hBayesDM", quietly = TRUE)) {
    devtools::install_github("CCS-Lab/hBayesDM", subdir = "R")
  }

  message("[setup] Package installation complete.")
}

### 3b. Loading [always] ---------------------------------------------------------------------------------------------------

invisible(lapply(packages, library, character.only = TRUE))
message("[setup] Packages loaded.")


## 4. GLOBAL OPTIONS [always] ==============================================================================================

options(
  brms.backend        = Sys.getenv("BRMS_BACKEND", "rstan"),
  brms.threads        = as.numeric(Sys.getenv("BRMS_THREADS", 1)),
  mc.cores            = as.numeric(Sys.getenv("MAX_CORES", 4)),
  digits              = 2,
  tinytable_tt_digits = 3
)

set.seed(777)


## 5. VISUAL SETTINGS [always] =============================================================================================

# font_import() is slow (~minutes) — only run on first setup, never on every source()
if (firstrun) {
  extrafont::font_import(prompt = FALSE)
  message("[setup] System fonts imported.")
}

extrafont::loadfonts(device = "pdf")
extrafont::loadfonts(device = "postscript")

plot_font <- ifelse(
  exists("is_html") && is_html,
  Sys.getenv("FONT_HTML", "Arial"),
  Sys.getenv("FONT", "Arial")
)

update_geom_defaults("text", list(family = plot_font))

theme_set(
  theme_linedraw(base_size = 11, base_family = plot_font) +
    theme(
      strip.text       = element_text(color = "black", hjust = 0),
      strip.background = element_rect(color = NA, fill = NA),
      line             = element_line(linewidth = 0.25),
      panel.grid       = element_blank()
    )
)

theme_custom <- function(base_size = 12, base_family = "Arial") {
  theme_bw(base_size = base_size, base_family = base_family) +
    theme(
      text = element_text(family = base_family, size = base_size),
      axis.text = element_text(family = base_family, size = base_size - 2),
      axis.title = element_text(family = base_family, size = base_size),
      legend.text = element_text(family = base_family, size = base_size - 2),
      legend.title = element_text(family = base_family, size = base_size),
      plot.title = element_text(family = base_family, size = base_size),
      plot.subtitle = element_text(family = base_family, size = base_size - 2),
      plot.caption = element_text(family = base_family, size = base_size - 3),
      plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm"),
      panel.spacing = unit(0.01, "lines"),
      panel.grid.major = element_line(color = "gray80", linewidth = 0.2),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6)
    )
}

## 6. GITIGNORE SCAFFOLD [first run only] ==================================================================================

if (firstrun) {
  ### Root -----------------------------------------------------------------------------------------------------------------

  root_gitignore <- r"(
# ── R ────────────────────────────────────────────────────────────────────────
.Rhistory
.RData
.Ruserdata
.Rproj.user/
*.Rproj
renv/library/
renv/local/
renv/cellar/
renv/lock/
renv/python/
renv/staging/
.Rprofile.user

# ── Quarto ───────────────────────────────────────────────────────────────────
/.quarto/
*_files/
*_cache/
*_site/
_freeze/
_site/
*.html
*.pdf
*.docx
*.tex
*.log
*.aux
site_libs/

# ── Data — never commit raw or processed data ────────────────────────────────
data/

# ── Serialised R objects ─────────────────────────────────────────────────────
output/rds/

# ── Stan / CmdStan compiled objects ──────────────────────────────────────────
*.o
*.so
*.dll
*.d
*.hpp
code/stan/*/

# ── OS artefacts ─────────────────────────────────────────────────────────────
.DS_Store
.DS_Store?
Thumbs.db
Desktop.ini
ehthumbs.db
$RECYCLE.BIN/

# ── IDE / editor ─────────────────────────────────────────────────────────────
.vscode/
.idea/
*.swp
*.swo
*~
)"

  writeLines(trimws(root_gitignore), here::here(".gitignore"))
  message("Created: .gitignore (root)")

  ### Data folders — blanket block -----------------------------------------------------------------------------------------

  block_all <- "# Block all files in this folder — data must never be committed\n*\n!.gitignore\n"

  blocked_folders <- c(
    "data/experiment1/raw",
    "data/experiment2/raw",
    "data/experiment1/processed",
    "data/experiment2/processed",
    "data/pilot"
  )

  for (folder in blocked_folders) {
    path <- here::here(folder)
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    writeLines(block_all, file.path(path, ".gitignore"))
    message("Created: ", folder, "/.gitignore  [block all]")
  }

  ### Code & output folders ------------------------------------------------------------------------------------------------

  code_gitignore <- "# Ignore compiled Stan binaries and any data files that drift in\n*.o\n*.so\n*.dll\n*.d\n*.hpp\n*.csv\n*.tsv\n*.xlsx\n*.xls\n*.sav\n*.rds\n*.RData\n"
  output_gitignore <- "# Ignore intermediate/scratch files; keep committed figures only if desired\n*.rds\n*.RData\n*.csv\n"

  code_folders <- c(
    "code/stan",
    "code/functions",
    "code/pilot",
    "code/experiment1",
    "code/experiment1/functions",
    "code/experiment2",
    "code/experiment2/functions"
  )

  output_folders <- c(
    "output/figures",
    "output/figures/experiment1",
    "output/figures/experiment2",
    "output/figures/manuscript",
    "output/figures/experiment1/analyses",
    "output/figures/experiment2/analyses",
    "output/figures/pilot/experiment1/modelfit",
    "output/figures/pilot/experiment2/modelfit"
  )

  for (folder in code_folders) {
    path <- here::here(folder)
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    writeLines(code_gitignore, file.path(path, ".gitignore"))
    message("Created: ", folder, "/.gitignore  [code]")
  }

  for (folder in output_folders) {
    path <- here::here(folder)
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    writeLines(output_gitignore, file.path(path, ".gitignore"))
    message("Created: ", folder, "/.gitignore  [output]")
  }

  all_folders <- c("(root)", blocked_folders, code_folders, output_folders)
  message("\n── .gitignore files written ────────────────────────────────────────────────")
  message(paste0("  ", all_folders, collapse = "\n"))
  message("\nData folders are fully blocked. Nothing inside data/ or output/rds/ can be committed.")
}

message("[setup] Done. firstrun = ", firstrun)

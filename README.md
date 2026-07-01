# Does social media use show established neurocognitive signatures of addiction?

## Overview

This repository contains the reproducible analysis code for the reserach project investigating neurocognitive signatures of addiction. The project implements an in-person, two-session within-subjects experiment (Experiment 1) and an online, single-session within-subjects experiment (Experiment 2). 

### Preregistration

Both experiments were preregistered ahead of data collection and the preregistrations for Experiment 1 and 2 are available on OSF at https://doi.org/10.17605/OSF.IO/UWZ5B and https://doi.org/10.17605/OSF.IO/MNK3B. 

## Experiment 1

Run the below scripts in the order they are listed to reproduce the results presented in the manuscript. To retain participant anonymity, we share the processed participant data (steps outlined in 00_parsing.R).

(1) 01_modeling.R 
(2) 02_modelvalidation.R
(3) 03_analyses.qmd
(4) 04_sensitivity_analyses.qmd

## Experiment 2

Run the below scripts in the order they are listed to reproduce the results presented in the manuscript. To retain participant anonymity, we share the processed participant data (steps outlined in 01_parsing.R).

(1) 02_descriptives.R
(2) 03_modeling.R
(3) 04_modelvalidation.R
(4) 05_analyses.qmd
(5) 06_sensitivity_analyses.qmd

## Project Structure

```
├── README.md                           # This file
├── LICENSE                             # MIT License
├── data/
│   ├── experiment1/                     # Data for Experiment 1
|   |    ├── processed.R                 # Processed anonymous participant data (shared upon publication)
│   ├── experiment2/                     # Data for Experiment 2
|   |    ├── processed.R                 # Processed anonymous participant data (shared upon publication)
│   ├── pilot/                           # Data for Experiment 2 online pilot
|   |    ├── processed.R                 # Processed anonymous participant data (shared upon publication)
│   └── codebook.md                      # Data dictionary and variable definitions
├── code/
│   ├── functions/
│   ├── 00_setup.R/                      # Setup script for installation of dependencies and setup of project structure
│   ├── 99_manuscript_figures.R/         # Script creating publication-ready figures for the manuscript
│   ├── experiment_1/ 
│   │   ├── 00_parsing.R                 # Data cleaning and preparation based on raw participant data (not reproducible)
│   │   ├── 01_modelling.R               # Fitting computational models to participants choice data
│   │   ├── 02_modelvalidation.R         # Fitting computational models for model comparison and validation
│   │   ├── 03_analyses.qmd              # Comprehensive markdown of main analyses
│   │   └── 04_sensitivity_analyses.qmd  # Comprehensive markdown documenting performed sensitivity analyses
│   │   └── functions/                   # Various functions called in above scripts
│   ├── experiment_2/ 
│   │   ├── 00_pilot_devaluation.R       # Online pilot study data processing and analyses
│   │   ├── 01_parsing.R                 # Data cleaning and preparation based on raw participant data (not reproducible)
│   │   ├── 02_descriptives.R            # Detailed description of sample in Experiment 2
│   │   ├── 03_modelling.qmd             # Fitting computational models to participants choice data
│   │   └── 04_modelvalidation.qmd       # Fitting computational models for model comparison and validation
│   │   └── 05_analyses.qmd              # Comprehensive markdown of main analyses
│   │   └── 06_sensitivity_analyses.qmd  # Comprehensive markdown documenting performed sensitivity analyses
│   │   └── functions/                   # Various functions called in above scripts
│   ├── stan/                            # Stan models for modelling effort-based decision-making choice data
```

## ======================================================================================================================= ##
## Script:    Helper Functions
## ======================================================================================================================= ##
## Authors:   Clelia Veith
## Date:      Tue Nov 18 07:57:48 2025
## ======================================================================================================================= ##
## Helper functions used throughout project scripts (based on Mehrhof & Nord (2025))
## ======================================================================================================================= ##  

## Population standard deviation -----------------------------------------------
# Arguments:
#  x: A numerical vector of values

sdP <- function(x){
  sqrt(mean(x^2) - (mean(x))^2) 
}

## Standard error --------------------------------------------------------------
# Arguments:
#  x: A numerical vector of values

se <- function(x) sd(x) / sqrt(length(x))


## Task data standardisation --------------------------------------------------
# Arguments:
#   x: A numerical vector of values
#   ref: A single numerical values to be used as the reference
#   levels: Possible values the data to be standardized can take

standardization <- function(x, ref = 0, levels = 1:4){
  (x - ref) / sdP(levels)
}

## Min-max normalisation ------------------------------------------------------
# Arguments:
#   x: A numerical vector of values

normalisation <- function(x) {
  rng <- range(x)
  if (diff(rng) == 0) return(rep(0, length(x))) # Avoid division by 0
  (x - rng[1]) / (rng[2] - rng[1])
}
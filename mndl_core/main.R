######################################################################
# Mendel dev-1.0                                                     #
# Johannes Schütt, Chair of Empirical Methods, University of Potsdam #
# johschuett@uni-potsdam.de                                          #
# License: GNU General Public License v2.0 only                      #
######################################################################

# Version: R 4.0.2

# Set encoding
Sys.setlocale("LC_ALL", "en_US.UTF-8")

# Packages
library(dplyr, warn.conflicts = FALSE)        # Version 1.0.1
library(rapportools, warn.conflicts = FALSE)  # Version 1.0
library(rio, warn.conflicts = FALSE)          # Version 0.5.16
library(stats, warn.conflicts = FALSE)        # Version 4.0.2

# Set standard values for options
caption             <- ""
ci_level            <- 0.05
decimal_places      <- 2
decimal_places_perc <- 0
footer              <- ""
statistical_values  <- c("obs", "med", "mean", "sd")

# Available statistical values
# obs, med, mean, sd, ci, min, max, mode

cat("\n#! Generating TEX file \U0001F529 ...\n\n")

# Scripts
source("mndl_core/import.R")
source("mndl_core/functions.R")
source("mndl_core/generate.R")
#source("mndl_core/write_output.R")

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

# Get the table type ("means" or "percentages")
# from file (only read first line)
connection <- file("input/type.txt", "r")
table_type <- readLines(connection, n = 1, warn = FALSE)
close(connection)

# Manipulate string (tolower and remove spaces)
table_type <- tolower(gsub(" ", "", table_type))

if (table_type == "means") {

  #----------------------------- Set standard values for options
  #
  caption             <- ""
  ci_level            <- 0.05
  decimal_places      <- 2
  decimal_places_perc <- 0
  footer              <- "Note: Percentages may not add up due to rounding."
  statistical_values  <- c("obs", "med", "mean", "sd")
  # Available statistical values:
  # obs, med, ptiles, mean, sd, ci, min, max, mode, perc
  #-------------------------------------------------------------

  cat("\n#! Generating TEX file \U0001F529 ...\n\n")

  # Scripts
  source("mndl_core/import.R")
  source("mndl_core/means/options.R")
  source("mndl_core/means/functions.R")
  source("mndl_core/means/generate.R")
  source("mndl_core/means/write_output.R")

} else if (table_type == "percentages") {

  #----------------------------- Set standard values for options
  #
  caption             <- ""
  decimal_places      <- 2
  decimal_places_perc <- 0
  footer              <- "Note: Percentages may not add up due to rounding."
  #
  #-------------------------------------------------------------

  cat("\n#! Generating TEX file \U0001F529 ...\n\n")

  # Scripts
  source("mndl_core/import.R")
  source("mndl_core/percentages/options.R")
  #source("mndl_core/percentages/functions.R")
  #source("mndl_core/percentages/generate.R")
  #source("mndl_core/percentages/write_output.R")

} else {

  cat("#! Unknown table type!")

}

# Free memory
rm(connection)

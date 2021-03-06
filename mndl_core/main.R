######################################################################
# Mendel dev-1.0                                                     #
# Johannes Schütt, Chair of Empirical Methods, University of Potsdam #
# johschuett@uni-potsdam.de                                          #
# License: GNU General Public License v2.0 only                      #
######################################################################

# Version: R 4.0.3

# Set encoding
Sys.setlocale("LC_ALL", "en_US.UTF-8")

# Packages
library(dplyr, warn.conflicts = FALSE)        # Version 1.0.2
library(rio, warn.conflicts = FALSE)          # Version 0.5.16
library(stats, warn.conflicts = FALSE)        # Version 4.0.3

# Define is_empty() function
is_empty <- function(x, na.ignore = FALSE) {
  if (na.ignore)
    ifelse(is.na(x), return(FALSE), return(trimws(x) == ""))
  else
    return(trimws(x) == "")
}

# Define %!in% operator
'%!in%' <- function(x, y)!('%in%'(x, y))

# Get data from job file
source("mndl_core/prepare_job.R")

if (table_type == "means") {

  # Scripts
  source("mndl_core/import.R")
  source("mndl_core/means/options.R")
  source("mndl_core/means/functions.R")
  source("mndl_core/means/generate.R")
  source("mndl_core/write_output.R")

} else if (table_type == "percentages") {

  # Scripts
  source("mndl_core/import.R")
  source("mndl_core/percentages/options.R")
  source("mndl_core/percentages/functions.R")
  source("mndl_core/percentages/generate.R")
  source("mndl_core/write_output.R")

} else {

  cat("#! Unknown table type!")

}

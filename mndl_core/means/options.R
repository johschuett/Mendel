# options.R - means
# This script gets custom options if they exist

#----------------------------- Set standard values for options
#
caption             <- ""
ci_level            <- 0.05
decimal_places      <- 2
decimal_places_perc <- 0
footer              <- ""
statistical_values  <- c("obs", "med", "mean", "sd")
# Available statistical values:
# obs, med, ptiles, mean, sd, ci, min, max, mode, perc
#-------------------------------------------------------------

if ("options" %in% ls()) {
  # Get options
  available_options <- c("caption",
                         "ci_level",
                         "decimal_places",
                         "decimal_places_perc",
                         "footer",
                         "statistical_values")

  for (.row in seq_len(nrow(options))) {
    if (tolower(options[.row, "option"]) %in% available_options)
      assign(tolower(options[.row, "option"]), options[.row, "value"])
  }
  # Convert option values to integers
  ci_level <- as.double(ci_level)
  decimal_places <- as.integer(decimal_places)
  decimal_places_perc <- as.integer(decimal_places_perc)

  # Remove all whitespaces from statistical_values
  # and convert to lower space, then split the string;
  # every value can only occur once
  statistical_values <- tolower(gsub(" ", "", statistical_values))
  statistical_values <- strsplit(statistical_values, ",", fixed = TRUE)
  statistical_values <- unique(statistical_values[[1]])

  # Special standard footer if percentages occur as statistical values
  if ("footer" %!in% options$option && "perc" %in% statistical_values)
    footer <- "Note: Percentages may not add up due to rounding."

  # Free memory
  rm(.row, available_options, options)
}

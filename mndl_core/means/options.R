# options.R - means
# This script gets custom options if they exist

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
  # and convert to lower space, then split the string
  statistical_values <- tolower(gsub(" ", "", statistical_values))
  statistical_values <- strsplit(statistical_values, ",", fixed = TRUE)
  statistical_values <- statistical_values[[1]]

  # Free memory
  rm(available_options, options)
}

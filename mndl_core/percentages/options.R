# options.R - percentages
# This script gets custom options if they exist

#----------------------------- Set standard values for options
#
caption             <- ""
decimal_places      <- 2
decimal_places_perc <- 0
footer              <- "Note: Percentages may not add up due to rounding."
#
#-------------------------------------------------------------

if ("options" %in% ls()) {
  # Get options
  available_options <- c("caption",
                         "decimal_places",
                         "decimal_places_perc",
                         "footer")

  for (.row in seq_len(nrow(options))) {
    if (tolower(options[.row, "option"]) %in% available_options)
      assign(tolower(options[.row, "option"]), options[.row, "value"])
  }
  # Convert option values to integers
  decimal_places <- as.integer(decimal_places)
  decimal_places_perc <- as.integer(decimal_places_perc)

  # Free memory
  rm(available_options, options)
}

# options.R - means
# This script gets custom options if they exist

#----------------------------- Set standard values for options
#
ci_level            <- 0.05
decimal_places      <- 2
decimal_places_perc <- 0
statistical_values  <- "obs,med,mean,sd"
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

  # Check CI level for illegal values
  if (ci_level %!in% c(0.01, 0.025, 0.05, 0.1)) (ci_level <- 0.05)

  decimal_places <- as.integer(decimal_places)
  decimal_places_perc <- as.integer(decimal_places_perc)

  # Check decimal places for illegal values
  if (decimal_places > 3) (decimal_places <- 3)
  else if (decimal_places < 0) (decimal_places <- 0)

  # Check decimal places for percentages for illegal values
  if (decimal_places_perc > 3) (decimal_places_perc <- 3)
  else if (decimal_places_perc < 0) (decimal_places_perc <- 0)

  # Remove all whitespaces from statistical_values
  # and convert to lower space, then split the string;
  # every value can only occur once
  statistical_values <- tolower(gsub(" ", "", statistical_values))
  statistical_values <- unlist(strsplit(statistical_values, ",", fixed = TRUE))
  statistical_values <- unique(statistical_values)

  # Generate standard caption
  if ("caption" %!in% options$option) {
    caption <- ""

    # Check amount of dependend survery variables
    if (length(dependend_labels) > 1) {
      .a <- 1 # Counter for dependend labels
      for (.current_d_label in dependend_labels) {
        # Replace label with variable name if empty
        if (is_empty(.current_d_label, na.ignore = TRUE))
          .current_d_label <- dependend_vars[.a]

        if (.a == length(dependend_labels))
          caption <- paste(caption, "and ``", .current_d_label, "'' ", sep = "")
        else if (.a == length(dependend_labels) - 1)
          caption <- paste(caption, "``", .current_d_label, "'' ", sep = "")
        else
          caption <- paste(caption, "``", .current_d_label, "'', ", sep = "")
        .a <- .a + 1
      }
    } else {
      # Replace label with variable name if empty
      if (is_empty(dependend_labels[1], na.ignore = TRUE))
        .current_d_label <- dependend_vars[1]
      else
        .current_d_label <- dependend_labels[1]

      caption <- paste(caption, "``", .current_d_label, "'' ", sep = "")
    }

    caption <- paste(caption, "over ", sep = "")

    # Check amount of independend survey variables
    if (length(independend_labels) > 1) {
        .b <- 1 # Counter for independend variables
        for (.current_ind_label in independend_labels) {
          # Replace label with variable name if empty
          if (is_empty(.current_ind_label, na.ignore = TRUE))
            .current_ind_label <- independend_vars[.b]

          if (.b == length(independend_labels))
            caption <- paste(caption, "and ``", .current_ind_label, "''.", sep = "")
          else if (.b == length(independend_labels) - 1)
            caption <- paste(caption, "``", .current_ind_label, "'' ", sep = "")
          else
            caption <- paste(caption, "``", .current_ind_label, "'', ", sep = "")
          .b <- .b + 1
        }
    } else {
      # Replace label with variable name if empty
      if (is_empty(independend_labels[1], na.ignore = TRUE))
        .current_ind_label <- independend_vars[1]
      else
        .current_ind_label <- independend_labels[1]

      caption <- paste(caption, "``", .current_ind_label, "''.", sep = "")
    }
  }

  # Special standard footer if percentages occur as statistical values
  if ("footer" %!in% options$option && "perc" %in% statistical_values)
    footer <- "Note: Percentages may not add up due to rounding."
  else
    footer <- ""

  # Free memory
  rm(.row, available_options, options)
}

# generate.R
# This script generates the LaTeX code for the twoway table

# Create list for all anwers of all independend survey variables
answer_list <- list()

.b <- 1 # Counter for the independend survey variables
# Get answer values and labels of the independend survey variables
for (.current_independend in independend_vars) {
  # Create empty data frame for answer values and labels
  answers <- data.frame(value = numeric(), label = character())
  .c <- 1 # Index for detecting start/end of answers
  # Get answer values and labels of the current independend survey variable
  if (independend_types[.b] %in% c("F", "M")) {
    while (meta[independend_rows[.b] + .c, "class"] == "SQ") ( .c <- .c + 1 )
    # Get all answer values and labels
    while (meta[independend_rows[.b] + .c, "class"] == "A") {
      # Save value and label
      answers[nrow(answers) + 1, ] <- c(meta[independend_rows[.b] + .c, "name"],
                                      meta[independend_rows[.b] + .c, "text"])
      .c <- .c + 1
    }
    # Save data frame in list
    answer_list[[.b]] <- answers
  } else {
    while (meta[independend_rows[.b] + .c, "class"] == "A") {
      # Save value and label
      answers[nrow(answers) + 1, ] <- c(meta[independend_rows[.b] + .c, "name"],
                                      meta[independend_rows[.b] + .c, "text"])
      .c <- .c + 1
    }
    # Save data frame in list
    answer_list[[.b]] <- answers
  }
  .b <- .b + 1
}

pack <- ""

.a <- 1 # Counter for dependend survey variables
# Iterate through the dependend survey variables
for (.current_dependend in dependend_vars) {
  .b <- 1 # Counter for the independend survey variables
  # Iterate through the independend survey variables
  for (.current_independend in independend_vars) {
    # Iterate through the values of the current independend survey variable
    for (.current_value in answer_list[[.b]]$value) {
      .current_value <- as.numeric(.current_value)
      # Iterate through the needed statistical values
      for (.current_stat in statistical_values) {
        switch (.current_stat,
                ci   =  paste(pack, write_ci(.current_dependend, .current_independend, .current_value), sep = ""),
                max  =  paste(pack, write_max(.current_dependend, .current_independend, .current_value), sep = ""),
                mean =  paste(pack, write_mean(.current_dependend, .current_independend, .current_value), sep = ""),
                med  =  paste(pack, write_med(.current_dependend, .current_independend, .current_value), sep = ""),
                min  =  paste(pack, write_min(.current_dependend, .current_independend, .current_value), sep = ""),
                mode =  paste(pack, write_mode(.current_dependend, .current_independend, .current_value), sep = ""),
                obs  =  paste(pack, write_obs(.current_dependend, .current_independend, .current_value), sep = ""),
                sd   =  paste(pack, write_sd(.current_dependend, .current_independend, .current_value), sep = "")
        )
      }
    }
    .b <- .b + 1
  }
  .a <- .a + 1
}


# Assemble twoway table
twoway_table <- paste("
\\topcaption{", caption, "} \\label{tab:twoway}
", sep = "")

#for () {
  twoway_table <- paste(twoway_table, "",
  sep = "")
#}

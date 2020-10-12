# generate.R
# This script generates the LaTeX code for the twoway table

# Amount of columns of the twoway table
columns <- length(dependend_vars) * length(statistical_values) + 1
# Vector for the sections of the twoway table
# (every independend survey variable makes one section)
sections <- c()

.b <- 1 # Counter for independend survey variables
# Iterate through the independend survey variables
for (.current_independend in independend_vars) {
  # If there is no label for the current independend survery variable,
  # use the variable name as the label
  if (rapportools::is.empty(independend_labels[.b]))
    .current_ind_label <- .current_independend
  else
    .current_ind_label <- independend_labels[.b]

  # String for LaTeX code of the current section of the twoway table; write Q/SQ label
  pack <- paste("\\multirow[l]{", columns, "}{", .current_ind_label, "}\n", sep = "")
  # Iterate through the answer values of the current independend survey variable
  for (.current_value in answer_list[[.b]]$value) {
    # If there is no label for the current answer value,
    # use the answer value as the label
    if (rapportools::is.empty((dplyr::filter(answer_list[[.b]], value == .current_value))$label))
      .current_v_label <- .current_value
    else
      .current_v_label <- (dplyr::filter(answer_list[[.b]], value == .current_value))$label
    # Write answer label
    pack <- paste(pack, .current_v_label, sep = "")
    .current_value <- as.numeric(.current_value)
    .a <- 1 # Counter for the dependend survey variables
    # Iterate through the dependend survey variables
    for (.current_dependend in dependend_vars) {
      # Iterate through the needed statistical values
      for (.current_stat in statistical_values) {
        switch (.current_stat,
                ci   =  pack <- paste(pack, write_ci(.current_dependend, .current_independend, .current_value), sep = ""),
                max  =  pack <- paste(pack, write_max(.current_dependend, .current_independend, .current_value), sep = ""),
                mean =  pack <- paste(pack, write_mean(.current_dependend, .current_independend, .current_value), sep = ""),
                med  =  pack <- paste(pack, write_med(.current_dependend, .current_independend, .current_value), sep = ""),
                min  =  pack <- paste(pack, write_min(.current_dependend, .current_independend, .current_value), sep = ""),
                mode =  pack <- paste(pack, write_mode(.current_dependend, .current_independend, .current_value), sep = ""),
                obs  =  pack <- paste(pack, write_obs(.current_dependend, .current_independend, .current_value), sep = ""),
                perc =  pack <- paste(pack, write_perc(.current_dependend, .current_independend, .current_value), sep = ""),
                sd   =  pack <- paste(pack, write_sd(.current_dependend, .current_independend, .current_value), sep = "")
        )
      }
    }
    # New line
    pack <- paste(pack, " \\\\\n", sep = "")
    .a <- .a + 1
  }
  # Add space between sections
  if (.b < length(independend_vars))
    pack <- paste(pack, "\n\\addlinespace[.5cm]\n\n", sep = "")
  # Code of this section is complete
  sections[length(sections) + 1] <- pack
  .b <- .b + 1
}


# Assemble twoway table
twoway_table <- paste("
\\topcaption{", caption, "} \\label{tab:twoway}
", sep = "")

# Free memory
rm(.a, .b, .c, .current_dependend, .current_independend, .current_ind_label,
   .current_stat, .current_value, .current_v_label, answers)

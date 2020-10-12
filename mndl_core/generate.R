# generate.R
# This script generates the LaTeX code for the twoway table

pack <- ""

.a <- 1 # Counter for dependend survey variables
# Iterate through the dependend survey variables
for (.current_dependend in dependend_vars) {
  pack <- paste(pack, .current_dependend, "\n", sep = "")
  .b <- 1 # Counter for the independend survey variables
  # Iterate through the independend survey variables
  for (.current_independend in independend_vars) {
    pack <- paste(pack, .current_independend, "\n", sep = "")
    # Iterate through the values of the current independend survey variable
    for (.current_value in answer_list[[.b]]$value) {
      pack <- paste(pack, .current_value, sep = "")
      .current_value <- as.numeric(.current_value)
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
      pack <- paste(pack, " \\\\\n", sep = "")
    }
    .b <- .b + 1
  }
  .a <- .a + 1
}


# Assemble twoway table
twoway_table <- paste("
\\topcaption{", caption, "} \\label{tab:twoway}
", sep = "")

# Free memory
rm(.a, .b, .c, .current_dependend, .current_independend, .current_stat,
   .current_value, answers)

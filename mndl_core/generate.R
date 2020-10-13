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
  pack <- paste("\t\\multirow[t]{", columns, "}{*}{", .current_ind_label, "} \\\\\n", sep = "")
  # Iterate through the answer values of the current independend survey variable
  for (.current_value in answer_list[[.b]]$value) {
    # If there is no label for the current answer value,
    # use the answer value as the label
    if (rapportools::is.empty((dplyr::filter(answer_list[[.b]], value == .current_value))$label))
      .current_v_label <- .current_value
    else
      .current_v_label <- (dplyr::filter(answer_list[[.b]], value == .current_value))$label
    # Write answer label
    pack <- paste(pack, "\t\\hskip5mm ", .current_v_label, sep = "")
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
    pack <- paste(pack, "\t[\\normalbaselineskip]\n\n", sep = "")
  # Code of this section is complete
  sections[length(sections) + 1] <- pack
  .b <- .b + 1
}

## LaTeX
# Create tablefirshead and tablehead
tablefirsthead <- "\\tablefirsthead {\n"
tablehead <- "\\tablehead {\n"
headstructure <- "\t\\toprule\n\t"
stat_labels <- c()

# Get labels for statistical values
for (.current_stat in statistical_values) {
  switch (.current_stat,
          ci   = stat_labels[length(stat_labels) + 1] <- paste("CI (", (1 - ci_level) * 100, "\\%)", sep = ""),
          max  = stat_labels[length(stat_labels) + 1] <- "Max.",
          mean = stat_labels[length(stat_labels) + 1] <- "Mean",
          med  = stat_labels[length(stat_labels) + 1] <- "Median",
          min  = stat_labels[length(stat_labels) + 1] <- "Min.",
          mode = stat_labels[length(stat_labels) + 1] <- "Mode",
          obs  = stat_labels[length(stat_labels) + 1] <- "Obs.",
          perc = stat_labels[length(stat_labels) + 1] <- "\\%",
          sd   = stat_labels[length(stat_labels) + 1] <- "St. Dev."
  )
}

# Building headstructure
for (.current_d_label in dependend_labels) {
  # Insert label for current dependend survey variable
  headstructure <- paste(headstructure, " & \\multicolumn{", (columns - 1) / length(dependend_labels), "}{c}{", .current_d_label, "}", sep = "")
}

# End row and creat cmidrule
headstructure <- paste(headstructure, " \\\\\n\t\\cmidrule(l{2mm}r{2mm}){2-", columns, "}", sep = "")

# New line
headstructure <- paste(headstructure, " \n\t", sep = "")

for (.current_d_label in dependend_labels) { # This for loop is not optimal
  for (.current_s_label in stat_labels) {
    # Insert label for current statistical value
    headstructure <- paste(headstructure, " & \\mc{", .current_s_label, "} ", sep = "")
  }
}

# End row
headstructure <- paste(headstructure, "\\\\\n\t\\midrule", sep = "")

# Assemble tablefirshead and tablehead
tablefirsthead <- paste(tablefirsthead, headstructure, "\n}\n\n", sep = "")
tablehead <- paste(tablehead, headstructure, "\n}\n\n", sep = "")

# Create column types; calculate width for answer label column
# based on the longest string * 1.6mm
answer_collection <- c()

for (.answers in answer_list)
  for (.current_answer in .answers$label)
    answer_collection[length(answer_collection) + 1] <- .current_answer

longest_string <- max(nchar(c(answer_collection, independend_labels)))

xtab_columns <- paste(replicate(columns - 1, "r"), sep = "", collapse = "")
xtab_columns <- paste("p{", round(longest_string * 1.6, 2), "mm}", xtab_columns, sep = "") # Alternative: l

# Assemble twoway table
twoway_table <- paste("
%----------------------------------------------------------- twoway table preamble

", tablefirsthead, tablehead, "
\\tabletail {
\t\\bottomrule
\t\\multicolumn{", columns, "}{r}{\\footnotesize \\emph{Continued on next page}} \\\\
}

\\tablecaption{", caption, "}

%-------------------------------------------------------------------------------

\\begin{xtabular}{", xtab_columns, "}

", sep = "")

for (.section in sections)
  twoway_table <- paste(twoway_table, .section, sep = "")

twoway_table <- paste(twoway_table,"
\t\\bottomrule

\\end{xtabular}

\\vspace{.5em}
\\footnotesize
", footer,
"
\\normalsize
", sep = "")

# Free memory
rm(.a, .b, .c, .current_dependend, .current_independend, .current_ind_label,
   .current_stat, .current_value, .current_v_label, answers)

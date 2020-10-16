# generate.R - means
# This script generates the LaTeX code for the twoway table

# Amount of columns of the twoway table (percentiles take three columns instead of one)
if ("ptiles" %in% statistical_values) {
  columns <- length(dependend_vars) * (length(statistical_values) + 2) + 1
} else {
  columns <- length(dependend_vars) * length(statistical_values) + 1
}
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
  pack <- paste("\t\t\t\\multicolumn{", columns, "}{l}{", .current_ind_label, "} \\\\\n", sep = "")
  # Iterate through the answer values of the current independend survey variable
  for (.current_value in answer_list[[.b]]$value) {
    # If there is no label for the current answer value,
    # use the answer value as the label
    if (rapportools::is.empty((dplyr::filter(answer_list[[.b]], value == .current_value))$label))
      .current_v_label <- .current_value
    else
      .current_v_label <- (dplyr::filter(answer_list[[.b]], value == .current_value))$label
    # Write answer label
    pack <- paste(pack, "\t\t\t\\hskip5mm ", .current_v_label, sep = "")
    .current_value <- as.numeric(.current_value)
    .a <- 1 # Counter for the dependend survey variables
    # Iterate through the dependend survey variables
    for (.current_dependend in dependend_vars) {
      # Iterate through the needed statistical values
      for (.current_stat in statistical_values) {
        switch (.current_stat,
                ci     = pack <- paste(pack, write_ci(.current_dependend, .current_independend, .current_value), sep = ""),
                max    = pack <- paste(pack, write_max(.current_dependend, .current_independend, .current_value), sep = ""),
                mean   = pack <- paste(pack, write_mean(.current_dependend, .current_independend, .current_value), sep = ""),
                med    = pack <- paste(pack, write_med(.current_dependend, .current_independend, .current_value), sep = ""),
                min    = pack <- paste(pack, write_min(.current_dependend, .current_independend, .current_value), sep = ""),
                mode   = pack <- paste(pack, write_mode(.current_dependend, .current_independend, .current_value), sep = ""),
                obs    = pack <- paste(pack, write_obs(.current_dependend, .current_independend, .current_value), sep = ""),
                perc   = pack <- paste(pack, write_perc(.current_dependend, .current_independend, .current_value), sep = ""),
                ptiles = pack <- paste(pack, write_ptiles(.current_dependend, .current_independend, .current_value), sep = ""),
                sd     = pack <- paste(pack, write_sd(.current_dependend, .current_independend, .current_value), sep = "")
        )
      }
    }
    # End row
    pack <- paste(pack, " \\\\\n", sep = "")
    .a <- .a + 1
  }
  # Add space between sections
  if (.b < length(independend_vars))
    pack <- paste(pack, "\t\t\t[\\normalbaselineskip]\n\n", sep = "")
  # Code of this section is complete
  sections[length(sections) + 1] <- pack
  .b <- .b + 1
}

## LaTeX
# Create tablefirsthead and tablehead
tablefirsthead <- "\t\\tablefirsthead {\n"
tablehead <- "\t\\tablehead {\n"
headstructure <- "\t\t\\toprule\n\t\t"
stat_labels <- c()

# Get labels for statistical values
for (.current_stat in statistical_values) {
  switch (.current_stat,
          ci     = stat_labels[length(stat_labels) + 1] <- paste("CI (", (1 - ci_level) * 100, "\\%)", sep = ""),
          max    = stat_labels[length(stat_labels) + 1] <- "Max",
          mean   = stat_labels[length(stat_labels) + 1] <- "Mean",
          med    = stat_labels[length(stat_labels) + 1] <- "Median",
          min    = stat_labels[length(stat_labels) + 1] <- "Min",
          mode   = stat_labels[length(stat_labels) + 1] <- "Mode",
          obs    = stat_labels[length(stat_labels) + 1] <- "Obs",
          perc   = stat_labels[length(stat_labels) + 1] <- "Perc",
          ptiles = stat_labels[length(stat_labels) + 1] <- "Percentiles",
          sd     = stat_labels[length(stat_labels) + 1] <- "Std. D."
  )
}

# Building headstructure
if (length(statistical_values) > 1) {
  # Calculate width of one dependend variable "column"
  if ("ptiles" %in% statistical_values) {
    # 5mm is the average width of a statistic label (except for percentiles)
    # The percentiles label has a width of 30mm
    dependend_width <- 5 * (length(statistical_values) - 1) + 30
  } else {
    dependend_width <- 5 * length(statistical_values)
  }

  # CI gets an extra of 12mm
  if ("ci" %in% statistical_values) {
    dependend_width <- dependend_width + 12
  }

  # Set lower margin
  if (dependend_width < 15)
    dependend_width <- 15

  for (.current_d_label in dependend_labels) {
    # Insert label for current dependend survey variable
    #headstructure <- paste(headstructure, " & \\multicolumn{",
    #                      (columns - 1) / length(dependend_labels),
    #                      "}{C{", dependend_width,"mm}}{",.current_d_label, "}",
    #                      sep = "")

    headstructure <- paste(headstructure, " & \\multicolumn{",
                          (columns - 1) / length(dependend_labels),
                          "}{c}{{\\parbox{", dependend_width, "mm}{\\centering ",.current_d_label, "}}}",
                          sep = "")
    }
    # Free memory
    rm(dependend_width)
} else {
  for (.current_d_label in dependend_labels) {
    # Insert label for current dependend survey variable
    headstructure <- paste(headstructure, " & \\multicolumn{",
                          (columns - 1) / length(dependend_labels),
                          "}{c}{",.current_d_label, "}",
                          sep = "")
  }
}

# End row and create cmidrule
headstructure <- paste(headstructure, " \\\\\n\t\t", sep = "")
if ("ptiles" %in% statistical_values) {
  for (.a in seq_len(length(dependend_labels))) {
      if (.a == 1) {
        headstructure <- paste(headstructure, " \\cmidrule(l{2mm}r{2mm}){2-", 1 + length(statistical_values) + 2, "}", sep = "")
      } else {
        headstructure <- paste(headstructure, " \\cmidrule(l{2mm}r{2mm}){",
                               2 + (.a - 1) * (length(statistical_values) + 2),
                               "-", 1 + .a * (length(statistical_values) + 2), "}",
                               sep = "")
      }
  }
} else {
  for (.a in seq_len(length(dependend_labels))) {
    if (.a == 1) {
      headstructure <- paste(headstructure, " \\cmidrule(l{2mm}r{2mm}){2-", 1 + length(statistical_values), "}", sep = "")
    } else {
      headstructure <- paste(headstructure, " \\cmidrule(l{2mm}r{2mm}){",
                             2 + (.a - 1) * length(statistical_values),
                             "-", 1 + .a * length(statistical_values), "}",
                             sep = "")
    }
  }
}

# New line
headstructure <- paste(headstructure, " \n\t\t", sep = "")

# Write labels for statistical values (special case for percentile labels)
if ("ptiles" %in% statistical_values) {
  # Get position of percentiles
  ptiles_pos <- which(statistical_values == "ptiles")[[1]]
  for (.a in seq_len(length(dependend_labels))) {
    .b <- 1 # Counter for stat_labels
    for (.current_s_label in stat_labels) {
      # Insert label for current statistical value
      if (.b != ptiles_pos)
        headstructure <- paste(headstructure, " & \\multirow[c]{3}{*}{", .current_s_label,"}", sep = "")
      else
        headstructure <- paste(headstructure, " & \\multicolumn{3}{c}{", .current_s_label,"}", sep = "")

      .b <- .b + 1
    }
  }

  # End row
  headstructure <- paste(headstructure, " \\\\\n\t\t", sep = "")

  # Write cmidrules for percentiles
  for (.a in seq_len(length(dependend_labels))) {
    headstructure <- paste(headstructure, "\\cmidrule(l{2mm}r{2mm}){",
                           1 + (.a - 1) * (length(statistical_values) + 2) + ptiles_pos, "-",
                           1 + (.a - 1) * (length(statistical_values) + 2) + ptiles_pos + 2, "}",
                           sep = "")
  }

  # New line
  headstructure <- paste(headstructure, "\n\t\t", sep = "")

  # Write percentile labels
  for (.a in seq_len(length(dependend_labels))) {
    for (.b in statistical_values) {
      if (.b != "ptiles")
        headstructure <- paste(headstructure, " &", sep = "")
      else
        headstructure <- paste(headstructure, " & \\mc{25\\%} & \\mc{50\\%} & \\mc{75\\%}", sep = "")
    }
  }
  # Free memory
  rm(ptiles_pos)
} else { # No percentiles
  for (.a in seq_len(length(dependend_labels))) {
    for (.current_s_label in stat_labels) {
      # Insert label for current statistical value
      headstructure <- paste(headstructure, " & \\mc{", .current_s_label, "}", sep = "")
    }
  }
}

# End row
headstructure <- paste(headstructure, " \\\\\n\t\t\\midrule", sep = "")

# Assemble tablefirsthead and tablehead
tablefirsthead <- paste(tablefirsthead, headstructure, "\n\t}\n\n", sep = "")
tablehead <- paste(tablehead, headstructure, "\n\t}\n", sep = "")

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
\t%----------------------------------------------------------- twoway table preamble

", tablefirsthead, tablehead, "
\t\\tabletail {
\t\t\\bottomrule
\t\t\\multicolumn{", columns, "}{r}{\\footnotesize \\emph{Continued on next page}} \\\\
\t}

\t\\tablelasttail {
\t\t\\bottomrule
\t\t\\multicolumn{", columns, "}{l}{\\footnotesize \\emph{", footer, "}} \\\\
\t}

\t\\tablecaption{", caption, "}

\t%-------------------------------------------------------------------------------

\t\\begin{center}

\t\t\\begin{xtabular}{", xtab_columns, "}

", sep = "")

for (.section in sections)
  twoway_table <- paste(twoway_table, .section, sep = "")

twoway_table <- paste(twoway_table,"\n\t\t\\end{xtabular}\n", sep = "")

# Free memory
rm(.a, .answers, .b, .c, .current_answer, .current_dependend, .current_d_label,
   .current_independend, .current_ind_label, .current_stat, .current_value,
   .current_s_label, .current_v_label, .section, answers, answer_collection,
   columns, headstructure, longest_string, pack, sections, stat_labels,
   tablefirsthead, tablehead, xtab_columns)

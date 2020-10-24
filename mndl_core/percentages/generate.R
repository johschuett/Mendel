# generate.R - percentages
# This script generates the LaTeX code for the twoway table

columns <- 1
# Amount of columns of the twoway table
for (.current_answers in independend_answer_list) {
  columns <- columns + length(.current_answers$value) + 1
}

# Vector for the sections of the twoway table
# (every dependend survey variable makes one section)
sections <- c()

.a <- 1 # Counter for dependend survey variables
# Iterate through the dependend survey variables
for (.current_dependend in dependend_vars) {
  # If there is no label for the current dependend survery variable,
  # use the variable name as the label
  if (is_empty(dependend_labels[.a], na.ignore = TRUE))
    .current_d_label <- .current_dependend
  else
    .current_d_label <- dependend_labels[.a]

  # String for LaTeX code of the current section of the twoway table; write Q/SQ label
  pack <- paste("\t\t\t\\multicolumn{", columns, "}{l}{", .current_d_label, "} \\\\\n", sep = "")
  # Iterate through the answer values of the current dependend survey variable
  for (.current_d_value in dependend_answer_list[[.a]]$value) {
    # If there is no label for the current answer value,
    # use the answer value as the label
    if (is_empty((dplyr::filter(dependend_answer_list[[.a]], value == .current_d_value))$label, na.ignore = TRUE))
      .current_dv_label <- .current_d_value
    else
      .current_dv_label <- (dplyr::filter(dependend_answer_list[[.a]], value == .current_d_value))$label
    # Write answer label
    pack <- paste(pack, "\t\t\t\\hskip5mm ", .current_dv_label, sep = "")
    .current_d_value <- as.numeric(.current_d_value)

    .b <- 1 # Counter for independend survey variables
    # Iterate through the independend survey variables
    for (.current_independend in independend_vars) {
      # Iterate through the answer values of the current independend survey variable
      for (.current_i_value in independend_answer_list[[.b]]$value) {
        pack <- paste(pack, write_perc(.current_dependend, .current_independend, .current_d_value, .current_i_value), sep = "")
      }
      # Get observations
      pack <- paste(pack, write_obs(.current_dependend, .current_independend, .current_d_value), sep = "")
      .b <- .b + 1
    }
    # End row
    pack <- paste(pack, " \\\\\n", sep = "")
  }
  # Totals
  pack <- paste(pack, "\t\t\tTotal ", sep = "")

  # End row
  pack <- paste(pack, " \\\\\n", sep = "")

  # Add space between sections
  if (.a < length(dependend_vars))
    pack <- paste(pack, "\t\t\t[\\normalbaselineskip]\n\n", sep = "")
  # Code of this section is complete
  sections[length(sections) + 1] <- pack
  .a <- .a + 1
}

## LaTeX
# Create tablefirsthead and tablehead
tablefirsthead <- "\t\\tablefirsthead {\n"
tablehead <- "\t\\tablehead {\n"
headstructure <- "\t\t\\toprule\n\t\t"

# Building headstructure
.b <- 1 # Index for independend survey variables
for (.current_i_label in independend_labels) {
  if (is_empty(.current_i_label, na.ignore = TRUE))
    .current_i_label <- independend_vars[.b]

  .independend_width <- 15 * (length(independend_answer_list[[.b]]$label) + 1)
  # Insert label for current independend survey variable
  headstructure <- paste(headstructure, " & \\multicolumn{",
                        length(independend_answer_list[[.b]]$label) + 1,
                        "}{c}{{\\parbox{", .independend_width,
                        "mm}{\\centering ",.current_i_label, "}}}",
                        sep = "")
  .b <- .b + 1
}

# End row and create cmidrule
headstructure <- paste(headstructure, " \\\\\n\t\t", sep = "")
.total_length <- 0
for (.a in seq_len(length(independend_labels))) {
  if (.a == 1) {
    .current_length <- 1 + length(independend_answer_list[[1]]$label) + 1
    headstructure <- paste(headstructure, " \\cmidrule(l{2mm}r{2mm}){2-", .current_length, "}", sep = "")
  } else {
    .current_length <- .total_length + length(independend_answer_list[[.a]]$label) + 1
    headstructure <- paste(headstructure, " \\cmidrule(l{2mm}r{2mm}){",
                          .total_length + 1,
                          "-", .current_length, "}",
                          sep = "")
}
  .total_length <- .total_length + .current_length
}

# New line
headstructure <- paste(headstructure, " \n\t\t", sep = "")

# Write independend answer labels
for (.a in seq_len(length(independend_labels))) {
  for (.current_iv_label in independend_answer_list[[.a]]$label) {
    # Insert label for current statistical value
    headstructure <- paste(headstructure, " & \\parbox{15mm}{\\centering ", .current_iv_label, "}", sep = "")
  }
  headstructure <- paste(headstructure, "& \\mc{}", sep = "")
}

# End row
headstructure <- paste(headstructure, " \\\\\n\t\t", sep = "")

# Write independend answer labels
for (.a in seq_len(length(independend_labels))) {
  for (.current_iv_label in independend_answer_list[[.a]]$label) {
    # Insert label for current statistical value
    headstructure <- paste(headstructure, " & \\mc{\\%}", sep = "")
  }
  headstructure <- paste(headstructure, " & \\mc{Obs}", sep = "")
}

# End row
headstructure <- paste(headstructure, " \\\\\n\t\t\\midrule", sep = "")

# Assemble tablefirsthead and tablehead
tablefirsthead <- paste(tablefirsthead, headstructure, "\n\t}\n\n", sep = "")
tablehead <- paste(tablehead, headstructure, "\n\t}\n", sep = "")

# Create column types; calculate width for answer label column
# based on the longest string * 1.6mm
answer_collection <- c()

for (.answers in dependend_answer_list)
  for (.current_answer in .answers$label)
    answer_collection[length(answer_collection) + 1] <- .current_answer

longest_string <- max(nchar(c(answer_collection, dependend_labels)), na.rm = TRUE)

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

\t\\captionsetup[xtabular]{width = .75\\textwidth}
\t\\tablecaption{", caption, "}

\t%-------------------------------------------------------------------------------

\t\\begin{center}

\t\t\\begin{xtabular}{", xtab_columns, "}

", sep = "")

for (.section in sections)
  twoway_table <- paste(twoway_table, .section, sep = "")

twoway_table <- paste(twoway_table,"\n\t\t\\end{xtabular}\n", sep = "")

# Free memory
rm(.a, .current_answers, .current_dependend)

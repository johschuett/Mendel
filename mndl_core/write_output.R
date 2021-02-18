# write_output.R
# This script writes the LaTeX code into a TEX file

# Landscape option if means table has more than 5 statistical values
if(exists("statistical_values")) {
  # Means table
  if(length(statistical_values) > 5L) {
    # More than 5 statistical values
    pdflscape <- "\n\\usepackage{pdflscape}\n"
    landscape <- c("\n\n\t\\begin{landscape}", "\n\n\t\\end{landscape}")
    note <- ""
  } else {
    # Not more than 5 statistical values
    pdflscape <- ""
    landscape <- c("", "")
    note <- "\n% You can try the 'landscape' option for the documentclass\n% or the 'landscape' environment of the 'pdflscape' package if your table is too wide.\n"
  }
} else {
  # Percentages table
  pdflscape <- ""
  landscape <- c("", "")
  note <- "\n% You can try the 'landscape' option for the documentclass\n% or the 'landscape' environment of the 'pdflscape' package if your table is too wide.\n"
}

preamble <- paste("\\documentclass[10pt, twoside]{article}
", note, "
\\usepackage[T1]{fontenc}
\\usepackage[utf8]{inputenc}
\\usepackage[english]{babel}
\\usepackage{array}
\\usepackage{booktabs}
\\usepackage{caption}
\\usepackage{float}
\\usepackage[left = .2in, right = .2in, top = 1in, bottom = 1in]{geometry}
\\usepackage{multirow}", pdflscape, "
\\usepackage{xtab}

\\newcommand{\\mc}[1]{\\multicolumn{1}{c}{#1}}

\\begin{document}", landscape[1], "

\t\\title{", survey_title, "}
\t\\date{}

\t%\\maketitle", sep = "")

output_name <- paste("report_", ticket, ".tex", sep = "")

write(preamble, file = output_name, append = FALSE)

# Write code
write(twoway_table, file = output_name, append = TRUE)

end <- paste("\t\\end{center}", landscape[2], "

\\end{document}

% This document was generated using Mendel.", sep = "")

write(end, file = output_name, append = TRUE)

.syscom <- paste("pdflatex ", output_name, sep = "")
system(.syscom)
system("rm *.aux *log")

# Free memory
rm(.syscom, landscape, note, pdflscape, preamble)

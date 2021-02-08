# write_output.R
# This script writes the LaTeX code into a TEX file

# Landscape option if means table has more than 5 statistical values
if(exists("statistical_values")) {
  # Means table
  if(length(statistical_values) > 5L) {
    # More than 5 statistical values
    landscape <- ", landscape"
    note <- ""
  } else {
    # Not more than 5 statistical values
    landscape <- ""
    note <- "\n% You can try the 'landscape' option for the documentclass if your table is too wide.\n"
  }
} else {
  # Percentages table
  landscape <- ""
  note <- "\n% You can try the 'landscape' option for the documentclass if your table is too wide.\n"
}

preamble <- paste("\\documentclass[10pt, twoside", landscape, "]{article}
", note, "
\\usepackage[T1]{fontenc}
\\usepackage[utf8]{inputenc}
\\usepackage[english]{babel}
\\usepackage{array}
\\usepackage{booktabs}
\\usepackage{caption}
\\usepackage{float}
\\usepackage[left = .2in, right = .2in, top = 1in, bottom = 1in]{geometry}
\\usepackage{multirow}
\\usepackage{xtab}

\\newcommand{\\mc}[1]{\\multicolumn{1}{c}{#1}}

\\begin{document}

\t\\title{", survey_title, "}
\t\\date{}

\t%\\maketitle", sep = "")

output_name <- paste("report_", ticket, ".tex", sep = "")

write(preamble, file = output_name, append = FALSE)

# Write code
write(twoway_table, file = output_name, append = TRUE)

write("\t\\end{center}\n\n\\end{document}\n\n% This document was generated using Mendel.", file = output_name, append = TRUE)

.syscom <- paste("pdflatex ", output_name, sep = "")
system(.syscom)
system("rm *.aux *log")

# Free memory
rm(.syscom, landscape, note, preamble)

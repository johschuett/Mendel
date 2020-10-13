# write_output.R
# This script writes the LaTeX code into a TEX file

preamble <- paste("\\documentclass[10pt, twoside]{article}

% You can try the 'landscape' option for the documentclass if your table is too wide.

\\usepackage[T1]{fontenc}
\\usepackage[utf8]{inputenc}
\\usepackage[english]{babel}
\\usepackage{array}
\\usepackage{booktabs}
\\usepackage{float}
\\usepackage[left = .2in, right = .2in, top = 1in, bottom = 1in]{geometry}
\\usepackage{multirow}
\\usepackage{xtab}

\\newcommand{\\mc}[1]{\\multicolumn{1}{c}{#1}}

\\begin{document}

\\title{", survey_title, "}
\\date{}

%\\maketitle", sep = "")

write(preamble, file = "twoway.tex", append = FALSE)

# Write code
write(twoway_table, file = "twoway.tex", append = TRUE)

write("\\end{document}\n\n% This document was generated using Mendel.", file = "twoway.tex", append = TRUE)

cat("\n#! Building PDF file \U0001F9F1 ...\n\n")

system("pdflatex twoway.tex")
system("rm *.aux *log")

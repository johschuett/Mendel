# write_output.R
# This script writes the LaTeX code into a TEX file

preamble <- paste("\\documentclass[10pt, twoside]{article}

\\usepackage[T1]{fontenc}
\\usepackage[utf8]{inputenc}
\\usepackage[english]{babel}
\\usepackage{array}
\\usepackage{booktabs}
\\usepackage{dcolumn}
\\usepackage{float}
\\usepackage[left = .5in, right = .5in, top = 1in, bottom = 1in]{geometry}
\\usepackage{multirow}
\\usepackage{xtab}

\\newcommand{\\mc}[1]{\\multicolumn{1}{c}{#1}}
\\newcolumntype{.}[1]{D{.}{.}{#1}}

\\begin{document}

\\title{", survey_title, "}
\\date{}

\\maketitle", sep = "")

write(preamble, file = "twoway.tex", append = FALSE)

# Write code

write("\\end{document}", file = "twoway.tex", append = TRUE)

cat("\n#! Building PDF file \U0001F9F1 ...\n\n")

system("pdflatex twoway.tex")
system("rm *.aux *log")
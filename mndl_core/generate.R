# generate.R
# This script generates the LaTeX code for the twoway table

pack <- ""

.a <- 1 # Counter for dependend survey variables
# Iterate through the dependend survey variables
for (.current_dependend in dependend_vars) {
  .b <- 1 # Counter for the independend survey variables
  # Iterate through the independend survey variables
  for (.current_independend in independend_vars) {
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

# generate.R - percentages
# This script generates the LaTeX code for the twoway table

# Statistical values
statistical_values <- c("perc")

# Amount of columns of the twoway table
columns <- length(dependend_vars) * length(statistical_values) + 1

# Vector for the sections of the twoway table
# (every dependend survey variable makes one section)
sections <- c()

.a <- 1 # Counter for dependend survey variables
# Iterate through the dependend survey variables
for (.current_dependend in dependend_vars) {
  .a <- .a + 1
}

# Assemble twoway table
twoway_table <- ""

# Free memory
rm(.a, .current_dependend)

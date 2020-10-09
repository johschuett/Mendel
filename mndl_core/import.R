# import.R
# This script imports the data from the CSV files and sorts them

# Import data from file
data <- import("csv/data.csv")
# Convert missing entries to NAs
data[data == ""] <- NA
# Import metadata, options and the twoway table plan from files
meta <- import("csv/meta.csv")
options <- import("csv/options.csv")
plan <- import("csv/plan.csv")

# Get survey title
line_of_title <- which(meta$name == "surveyls_title")

if (is.integer(line_of_title) && length(line_of_title) == 0L) {
  survey_title <- "Untitled"
} else if (is.vector(line_of_title)) {
  survey_title <- meta[line_of_title[1], 5]
} else {
  survey_title <- meta[line_of_title, 5]
}

# Sort the survey variables
dependend_vars <- c()
independend_vars <- c()

for (.row in seq_len(nrow(plan))) {
  if (!is.empty(plan[.row, 1]))
    dependend_vars[length(dependend_vars) + 1] <- plan[.row, 1]

  if (!is.empty(plan[.row, 2]))
    independend_vars[length(independend_vars) + 1] <- plan[.row, 2]
}

# Get the survey variable types
dependend_types <- c()
independend_types <- c()

for (.el in dependend_vars) {
  .row <- which(meta$name == .el)
  dependend_types[length(dependend_types) + 1] <- meta[.row, 3]
}

print(all(dependend_types[1] == dependend_types))

for (.el in independend_vars) {
  .row <- which(meta$name == .el)
  independend_types[length(independend_types) + 1] <- meta[.row, 3]
}

print(all(independend_types[1] == independend_types))

# Get options
for (.row in seq_len(nrow(options))) {
  assign(options[.row, 1], options[.row, 2])
}

# Convert missing value to integer
missings <- as.integer(missings)

# Split string of statistical values into vector
statistical_values <- strsplit(statistical_values, ",", fixed = TRUE)
statistical_values <- statistical_values[[1]]

# Free memory
rm(.row, line_of_title, options)

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
  survey_title <- meta[line_of_title[1], "text"]
} else {
  survey_title <- meta[line_of_title, "text"]
}

# Sort the survey variables
dependend_vars <- c()
independend_vars <- c()

for (.row in seq_len(nrow(plan))) {
  if (!rapportools::is.empty(plan[.row, "col"]))
    dependend_vars[length(dependend_vars) + 1] <- plan[.row, "col"]

  if (!rapportools::is.empty(plan[.row, 2]))
    independend_vars[length(independend_vars) + 1] <- plan[.row, "row"]
}

# Get the survey variable types
dependend_types <- c()
independend_rows <- c()
independend_types <- c()

for (.el in dependend_vars) {
  .row <- which(meta$name == .el)
  dependend_types[length(dependend_types) + 1] <- meta[.row, "type/scale"]

  .i <- 1 # Index for going back the rows until hitting a type
  while (rapportools::is.empty(dependend_types[length(dependend_types)])) {
    dependend_types[length(dependend_types)] <- meta[.row - .i, "type/scale"]
    .i <- .i + 1
  }
}

for (.el in independend_vars) {
  .row <- which(meta$name == .el)
  independend_rows[length(independend_rows) + 1] <- .row
  independend_types[length(independend_types) + 1] <- meta[.row, "type/scale"]
}

#Set standard values for options
caption <- ""
ci_level <- 0.95
decimal_places <- 2
decimal_places_perc <- 0
footer <- ""
statistical_values <- "obs,med,mean,sd"

# Get options
available_options <- c("caption",
                       "ci_level",
                       "decimal_places",
                       "decimal_places_perc",
                       "footer",
                       "statistical_values") # (obs, med, mean, sd, ci, min, max, mode)

for (.row in seq_len(nrow(options))) {
  if (tolower(options[.row, "option"]) %in% available_options)
    assign(tolower(options[.row, "option"]), options[.row, "value"])
}

# Convert option values to integers
ci_level <- as.double(ci_level)
decimal_places <- as.integer(decimal_places)
decimal_places_perc <- as.integer(decimal_places_perc)

# Remove all whitespaces from statistical_values
# and convert to lower space, then split the string
statistical_values <- tolower(gsub(" ", "", statistical_values))
statistical_values <- strsplit(statistical_values, ",", fixed = TRUE)
statistical_values <- statistical_values[[1]]

# Free memory
rm(.i, .row, line_of_title, options)

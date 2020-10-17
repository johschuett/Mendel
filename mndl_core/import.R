# import.R
# This script imports the data from the CSV files and sorts them

# Import data from file
data <- import("input/data.csv")
# Import metadata and the twoway table plan from files
meta <- import("input/meta.csv")
plan <- import("input/plan.csv")
# Import options file if it exists
if (file.exists("input/options.csv")) (options <- import("input/options.csv"))

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
  if (!is_empty(plan[.row, "dependend"]))
    dependend_vars[length(dependend_vars) + 1] <- plan[.row, "dependend"]

  if (!is_empty(plan[.row, 2]))
    independend_vars[length(independend_vars) + 1] <- plan[.row, "independend"]
}

# Create new dataset containing only the dependend and independend survey variables
data <- data[, unique(c(dependend_vars, independend_vars))]
# Convert empty entries to NAs
data[is_empty(data)] <- NA
# Clean the data from non-numerical entries
data[] <- lapply(data, function(x) as.numeric(as.character(x)))

# Get the labels, rows and types for the survey variables
dependend_labels <- c()
dependend_types <- c()
independend_labels <- c()
independend_rows <- c()
independend_types <- c()

for (.el in dependend_vars) {
  .row <- which(meta$name == .el)
  dependend_labels[length(dependend_labels) + 1] <- meta[.row, "text"]
  dependend_types[length(dependend_types) + 1] <- meta[.row, "type/scale"]

  .i <- 1 # Index for going back the rows until hitting a type
  while (is_empty(dependend_types[length(dependend_types)])) {
    dependend_types[length(dependend_types)] <- meta[.row - .i, "type/scale"]
    .i <- .i + 1
  }
  # Custom label for matrix and multiple choice survey variables (Q-label: SQ-label)
  if (dependend_types[length(dependend_types)] %in% c("F", "M")) {
    .current_row <- .row
    .current_class = meta[.current_row, "class"]
    while (.current_class != "Q") {
      .current_row <- .current_row - 1
      .current_class <- meta[.current_row, "class"]
    }
    # Check if matrix/multiple choice label or SQ label is empty
    if(!is_empty(meta[.current_row, "text"]) && !is_empty(dependend_labels[length(dependend_labels)])) {
      dependend_labels[length(dependend_labels)] <- paste(meta[.current_row, "text"], ": ",
                                                          dependend_labels[length(dependend_labels)],
                                                          sep = "")
    }
  }
}

if (!all(dependend_types %in% c("F", "L", "M", "N")))
  stop("\n#! Illegal dependend variable type(s)!\n#! No output produced.")

for (.el in independend_vars) {
  .row <- which(meta$name == .el)
  independend_labels[length(independend_labels) + 1] <- meta[.row, "text"]
  independend_rows[length(independend_rows) + 1] <- .row
  independend_types[length(independend_types) + 1] <- meta[.row, "type/scale"]

  .i <- 1 # Index for going back the rows until hitting a type
  while (is_empty(independend_types[length(independend_types)])) {
    independend_types[length(independend_types)] <- meta[.row - .i, "type/scale"]
    .i <- .i + 1
  }
  # Custom label for matrix and multiple choice survey variables (Q-label: SQ-label)
  if (independend_types[length(independend_types)] %in% c("F", "M")) {
    .current_row <- .row
    .current_class = meta[.current_row, "class"]
    while (.current_class != "Q") {
      .current_row <- .current_row - 1
      .current_class <- meta[.current_row, "class"]
    }
    # Check if matrix/multiple choice label is empty
    if(!is_empty(meta[.current_row, "text"]) && !is_empty(independend_labels[length(independend_labels)])) {
      independend_labels[length(independend_labels)] <- paste(meta[.current_row, "text"], ": ",
                                                              independend_labels[length(independend_labels)],
                                                              sep = "")
    }
  }
}

if (!all(independend_types %in% c("F", "L", "M")))
  stop("\n#! Illegal independend variable type(s)!\n#! No output produced.")

# Create list for all anwers of all independend survey variables
answer_list <- list()

.b <- 1 # Counter for the independend survey variables
# Get answer values and labels of the independend survey variables
for (.current_independend in independend_vars) {
  # Create empty data frame for answer values and labels
  answers <- data.frame(value = numeric(), label = character())
  .c <- 1 # Index for detecting start/end of answers
  # Get answer values and labels of the current independend survey variable
  if (independend_types[.b] %in% c("F", "M")) {
    while (meta[independend_rows[.b] + .c, "class"] == "SQ") ( .c <- .c + 1 )
    # Get all answer values and labels
    while (meta[independend_rows[.b] + .c, "class"] == "A") {
      # Save value and label
      answers[nrow(answers) + 1, ] <- c(meta[independend_rows[.b] + .c, "name"],
                                        meta[independend_rows[.b] + .c, "text"])
      .c <- .c + 1
    }
    # Save data frame in list
    answer_list[[.b]] <- answers
  } else {
    while (meta[independend_rows[.b] + .c, "class"] == "A") {
      # Save value and label
      answers[nrow(answers) + 1, ] <- c(meta[independend_rows[.b] + .c, "name"],
                                        meta[independend_rows[.b] + .c, "text"])
      .c <- .c + 1
    }
    # Save data frame in list
    answer_list[[.b]] <- answers
  }
  .b <- .b + 1
}

# Free memory
rm(.current_class, .current_row, .el, .i, .row, line_of_title, plan)

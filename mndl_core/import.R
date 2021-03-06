# import.R
# This script imports the data and sorts it

# Get survey title
line_of_title <- which(meta$name == "surveyls_title")

if (is.integer(line_of_title) && length(line_of_title) == 0L) {
  survey_title <- "Untitled"
} else if (is.vector(line_of_title)) {
  survey_title <- meta[line_of_title[1], "text"]
} else {
  survey_title <- meta[line_of_title, "text"]
}

# Create new dataset containing only the dependend and independend survey variables
data <- data[, unique(c(dependend_vars, independend_vars))]
# Convert empty entries to NAs
data[sapply(data, is_empty)] <- NA
# Clean the data from non-numerical entries
data[] <- sapply(data, function(x) as.numeric(as.character(x)))

# Get the labels, rows and types for the survey variables
dependend_labels <- c()
dependend_rows <- c()
dependend_types <- c()
independend_labels <- c()
independend_rows <- c()
independend_types <- c()

for (.el in dependend_vars) {
  .row <- which(meta$name == .el)
  dependend_labels[length(dependend_labels) + 1] <- meta[.row, "text"]
  dependend_rows[length(dependend_rows) + 1] <- .row
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
    if(!is_empty(meta[.current_row, "text"], na.ignore = TRUE) && !is_empty(dependend_labels[length(dependend_labels)], na.ignore = TRUE)) {
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
    if(!is_empty(meta[.current_row, "text"], na.ignore = TRUE) && !is_empty(independend_labels[length(independend_labels)], na.ignore = TRUE)) {
      independend_labels[length(independend_labels)] <- paste(meta[.current_row, "text"], ": ",
                                                              independend_labels[length(independend_labels)],
                                                              sep = "")
    }
  }
}

if (!all(independend_types %in% c("F", "L", "M")))
  stop("\n#! Illegal independend variable type(s)!\n#! No output produced.")

# Create list for all anwers of all dependend survey variables (only for percentage tables)
if (table_type == "percentages") {
  dependend_answer_list <- list()

  .a <- 1 # Counter for the dependend survey variables
  # Get answer values and labels of the dependend survey variables
  for (.current_dependend in dependend_vars) {
    # Create empty data frame for answer values and labels
    answers <- data.frame(value = numeric(), label = character())
    .c <- 1 # Index for detecting start/end of answers
    # Get answer values and labels of the current dependend survey variable
    if (dependend_types[.a] %in% c("F", "M")) {
      while (meta[dependend_rows[.a] + .c, "class"] == "SQ") (.c <- .c + 1)
      # Get all answer values and labels
      while (meta[dependend_rows[.a] + .c, "class"] == "A") {
        # Save value and label
        answers[nrow(answers) + 1, ] <- c(meta[dependend_rows[.a] + .c, "name"],
                                          meta[dependend_rows[.a] + .c, "text"])
        .c <- .c + 1
      }
      # Save data frame in list
      dependend_answer_list[[.a]] <- answers
    } else {
      while (meta[independend_rows[.a] + .c, "class"] == "A") {
        # Save value and label
        answers[nrow(answers) + 1, ] <- c(meta[dependend_rows[.a] + .c, "name"],
                                          meta[dependend_rows[.a] + .c, "text"])
        .c <- .c + 1
      }
      # Save data frame in list
      dependend_answer_list[[.a]] <- answers
    }
    .a <- .a + 1
  }
  # Free memory
  rm(.a, .c, .current_dependend)
}

# Create list for all anwers of all independend survey variables
independend_answer_list <- list()

.b <- 1 # Counter for the independend survey variables
# Get answer values and labels of the independend survey variables
for (.current_independend in independend_vars) {
  # Create empty data frame for answer values and labels
  answers <- data.frame(value = numeric(), label = character())
  .c <- 1 # Index for detecting start/end of answers
  # Get answer values and labels of the current independend survey variable
  if (independend_types[.b] %in% c("F", "M")) {
    while (meta[independend_rows[.b] + .c, "class"] == "SQ") (.c <- .c + 1)
    # Get all answer values and labels
    while (meta[independend_rows[.b] + .c, "class"] == "A") {
      # Save value and label
      answers[nrow(answers) + 1, ] <- c(meta[independend_rows[.b] + .c, "name"],
                                        meta[independend_rows[.b] + .c, "text"])
      .c <- .c + 1
    }
    # Save data frame in list
    independend_answer_list[[.b]] <- answers
  } else {
    while (meta[independend_rows[.b] + .c, "class"] == "A") {
      # Save value and label
      answers[nrow(answers) + 1, ] <- c(meta[independend_rows[.b] + .c, "name"],
                                        meta[independend_rows[.b] + .c, "text"])
      .c <- .c + 1
    }
    # Save data frame in list
    independend_answer_list[[.b]] <- answers
  }
  .b <- .b + 1
}

# Delete this object, if it exists. Necessary for the landscape option in
# write_output.R where we need to check whether this variable was created
# by the script in means/options.R.
if(exists("statistical_values"))
  rm(statistical_values)

# Free memory
rm(.b, .c, .current_class, .current_independend, .current_row,
   .el, .i, .row, answers, line_of_title)

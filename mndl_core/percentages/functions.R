# functions.R - percentages
# This script contains functions used by generate.R

# Filter and subset the dataset
get_subset <- function(dependend, independend, value) {
  .com <- paste("numbers <- dplyr::filter(data, ", independend, " == ", value, ")", sep = "")
  eval(parse(text = .com))

  .com <- paste("numbers <- numbers$", dependend, sep = "")
  eval(parse(text = .com))

  return(numbers)
}

## STATISTICS ##

# Observations
write_obs <- function(dependend, independend, value) {
  # Get data
  numbers <- get_subset(dependend, independend, value)

  # Count observations
  result <- length(numbers[!is.na(numbers)])

  # Create LaTeX code
  chunk <- paste(" & ", format(result, nsmall = decimal_places), sep = "")

  return(chunk)
}

# Percentage
write_perc <- function(dependend, independend, value) {
  # Get data
  numbers <- get_subset(dependend, independend, value)

  # Get all answer values
  .index <- which(independend_vars == independend)[[1]]
  current_values <- answer_list[[.index]]$value

  # Get the amount of observations
  .com <- paste("obs_value <- filter(data, !is.na(", dependend, ") & ", independend, " %in% current_values)", sep = "")
  eval(parse(text = .com))

  .com <- paste("obs_value <- length(obs_value$", dependend,")", sep = "")
  eval(parse(text = .com))

  # Count observations
  result <- length(numbers[!is.na(numbers)]) / obs_value * 100

  # Create LaTeX code
  chunk <- paste(" & ", format(round(result, decimal_places_perc), nsmall = decimal_places_perc), sep = "")

  return(chunk)
}

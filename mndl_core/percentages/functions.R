# functions.R - percentages
# This script contains functions used by generate.R

# Filter and subset the dataset
get_subset <- function(dependend, independend, value) {
  .com <- paste("numbers <- dplyr::filter(data, ", dependend, " == ", value, ")", sep = "")
  eval(parse(text = .com))

  .com <- paste("numbers <- numbers$", independend, sep = "")
  eval(parse(text = .com))

  return(numbers)
}

## STATISTICS ##

# Observations
write_obs <- function(dependend, independend, d_value) {
  # Get data
  numbers <- get_subset(dependend, independend, d_value)

  # Count observations
  result <- length(numbers[!is.na(numbers)])

  # Get position of the independend survey variable in the totals list
  .b <- which(independend_vars == independend)

  # Store result in the data frame inside the totals list
  .answer_row[length(.answer_row) + 1] <<- result
  totals[[.b]] <<- rbind.data.frame(totals[[.b]], .answer_row)

  # Clear the row vector, for observations is the last column in each row
  .answer_row <<- c()

  # Create LaTeX code
  chunk <- paste(" & ", format(result, nsmall = decimal_places), sep = "")

  return(chunk)
}

# Percentage
write_perc <- function(dependend, independend, d_value, i_value) {
  # Get data
  .com <- paste("numbers <- dplyr::filter(data, ", independend, " == ", i_value, ")", sep = "")
  eval(parse(text = .com))

  .com <- paste("numbers <- numbers$", dependend, sep = "")
  eval(parse(text = .com))

  # Count observations
  total <- length(numbers[!is.na(numbers)])
  partial <- length(numbers[!is.na(numbers) & numbers == d_value])

  # Calculate percentage
  result <- partial / total * 100

  # Store result in the total vector
  .answer_row[length(.answer_row) + 1] <<- round(result, decimal_places_perc)

  # Create LaTeX code
  chunk <- paste(" & ", format(round(result, decimal_places_perc), nsmall = decimal_places_perc), sep = "")

  return(chunk)
}

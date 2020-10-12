# functions.R
# This script contains functions used by generate.R

# Gets the mode
get_mode <-  function(x, na.rm = TRUE) { # Source: https://www.politikwissenschaften.ch/pdf.php?id=11
  if (na.rm) {
    x <- na.omit(x)
  }
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

# Filter and subset the dataset
get_subset <- function(dependend, independend, value) {
  .com <- paste("numbers <- dplyr::filter(data, ", independend, " == ", value, ")", sep = "")
  eval(parse(text = .com))

  .com <- paste("numbers <- numbers$", dependend, sep = "")
  eval(parse(text = .com))

  return(numbers)
}

## STATISTICS ##

# Confidence interval (t-distribution)
write_ci <- function(dependend, independend, value) {
  # Get data
  numbers <- get_subset(dependend, independend, value)

  # Get required values for calculating the CI
  mean_value <- mean(numbers, na.rm = TRUE)
  sd_value <- stats::sd(numbers, na.rm = TRUE)
  obs_value <- length(numbers[!is.na(numbers)])

  # Calculate the error bound
  error_value <- stats::qt(1 - ci_level / 2, df = obs_value - 1) * sd_value / sqrt(obs_value)

  # Calculate the margins
  left <- mean_value - error_value
  right <- mean_value + error_value

  # Check if results are real numbers. If not, write "[ - ; - ]" in LaTeX code
  if (!is.infinite(c(left, right)) && !is.na(c(left, right)) && !is.nan(c(left, right)))
    chunk <- paste(" & [ ", format(round(left, decimal_places), nsmall = decimal_places), " ; ",
                   format(round(right, decimal_places), nsmall = decimal_places), " ]", sep = "")
  else
    chunk <- " & [ - ; - ]"

  return(chunk)
}

# Maxima
write_max <- function(dependend, independend, value) {
  # Get data
  numbers <- get_subset(dependend, independend, value)

  # Calculate the max
  result <- max(numbers, na.rm = TRUE)

  # Check if result is a real number. If not, create a "-" in LaTeX code
  if (!is.infinite(result) && !is.na(result) && !is.nan(result))
    chunk <- paste(" & ", format(round(result, decimal_places), nsmall = decimal_places), sep = "")
  else
    chunk <- " & -"

  return(chunk)
}

write_mean <- function(dependend, independend, value) {
  # Get data
  numbers <- get_subset(dependend, independend, value)

  # Calculate the mean
  result <- mean(numbers, na.rm = TRUE)

  # Check if result is a real number. If not, create a "-" in LaTeX code
  if (!is.infinite(result) && !is.na(result) && !is.nan(result))
    chunk <- paste(" & ", format(round(result, decimal_places), nsmall = decimal_places), sep = "")
  else
    chunk <- " & -"

  return(chunk)
}

# Median
write_med <- function(dependend, independend, value) {
  # Get data
  numbers <- get_subset(dependend, independend, value)

  # Calculate the mean
  result <- stats::median(numbers, na.rm = TRUE)

  # Check if result is a real number. If not, create a "-" in LaTeX code
  if (!is.infinite(result) && !is.na(result) && !is.nan(result))
    chunk <- paste(" & ", format(round(result, decimal_places), nsmall = decimal_places), sep = "")
  else
    chunk <- " & -"

  return(chunk)
}

# Minima
write_min <- function(dependend, independend, value) {
  # Get data
  numbers <- get_subset(dependend, independend, value)

  # Calculate the mean
  result <- min(numbers, na.rm = TRUE)

  # Check if result is a real number. If not, create a "-" in LaTeX code
  if (!is.infinite(result) && !is.na(result) && !is.nan(result))
    chunk <- paste(" & ", format(round(result, decimal_places), nsmall = decimal_places), sep = "")
  else
    chunk <- " & -"

  return(chunk)
}

# Mode
write_mode <- function(dependend, independend, value) {
  # Get data
  numbers <- get_subset(dependend, independend, value)

  # Calculate the mode
  result <- get_mode(numbers)

  # Check if result is a real number. If not, create a "-" in LaTeX code
  if (!is.infinite(result) && !is.na(result) && !is.nan(result))
    chunk <- paste(" & ", format(round(result, decimal_places), nsmall = decimal_places), sep = "")
  else
    chunk <- " & -"

  return(chunk)
}

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

  # Count observations
  result <- length(numbers[!is.na(numbers)]) / nrow(data)

  # Create LaTeX code
  chunk <- paste(" & ", format(round(result, decimal_places), nsmall = decimal_places), sep = "")

  return(chunk)

}

# Standard deviation
write_sd <- function(dependend, independend, value) {
  # Get data
  numbers <- get_subset(dependend, independend, value)

  # Calculate the standard deviation
  result <- stats::sd(numbers, na.rm = TRUE)

  # Check if result is a real number. If not, create a "-" in LaTeX code
  if (!is.infinite(result) && !is.na(result) && !is.nan(result))
    chunk <- paste(" & ", format(round(result, decimal_places), nsmall = decimal_places), sep = "")
  else
    chunk <- " & -"

  return(chunk)
}

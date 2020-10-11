# functions.R
# This script contains functions used by generate.R

# Gets the absolute frequency
get_absolute_freq <- function(var, value) (as.numeric(count(dplyr::filter(data, eval(parse(text = var)) == value))))

# Gets the mode
get_mode <-  function(x, na.rm = FALSE) { # Source: https://www.politikwissenschaften.ch/pdf.php?id=11
  if (na.rm) {
    x <- na.omit(x)
  }
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

get_subset <- function(dependend, independend, value) {
  # Filter and subset the dataset
  .com <- paste("numbers <- dplyr::filter(data, ", independend, " == ", value, ")", sep = "")
  eval(parse(text = .com))

  .com <- paste("numbers <- numbers$", dependend, sep = "")
  eval(parse(text = .com))

  return(numbers)
}

write_ci <- function(dependend, independend, value) {
  numbers <- get_subset(dependend, independend, value)

  a <- mean(numbers)
  s <- sd(numbers)
  n <- length(numbers)

  error <- qnorm(ci_level)*s/sqrt(n)
  left <- a - error
  right <- a + error

  # Check if result is a real number. If not, create a "-" in LaTeX code
  if (!is.infinite(c(left, right)) && !is.na(c(left, right)) && !is.nan(c(left, right)))
    chunk <- paste(" & [ ", format(round(left, decimal_places), nsmall = decimal_places), " ; ",
                  format(round(right, decimal_places), nsmall = decimal_places), " ]", sep = "")
  else
    chunk <- "[ - ; - ]"

  cat(paste("CI:", chunk, "\n"))
  return(chunk)
}

write_max <- function(dependend, independend, value) {
  # Get data
  numbers <- get_subset(dependend, independend, value)

  # Calculate the max
  result <- max(numbers)

  # Check if result is a real number. If not, create a "-" in LaTeX code
  if (!is.infinite(result) && !is.na(result) && !is.nan(result))
    chunk <- paste(" & ", format(round(result, decimal_places), nsmall = decimal_places), sep = "")
  else
    chunk <- " & -"

  cat(paste("Max:", chunk, "\n"))
  return(chunk)
}

write_mean <- function(dependend, independend, value) {
  # Get data
  numbers <- get_subset(dependend, independend, value)

  # Calculate the mean
  result <- mean(numbers)

  # Check if result is a real number. If not, create a "-" in LaTeX code
  if (!is.infinite(result) && !is.na(result) && !is.nan(result))
    chunk <- paste(" & ", format(round(result, decimal_places), nsmall = decimal_places), sep = "")
  else
    chunk <- " & -"

  cat(paste("Mean:", chunk, "\n"))
  return(chunk)
}

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

  cat(paste("Median:", chunk, "\n"))
  return(chunk)
}

write_min <- function(dependend, independend, value) {
  # Get data
  numbers <- get_subset(dependend, independend, value)

  # Calculate the mean
  result <- min(numbers)

  # Check if result is a real number. If not, create a "-" in LaTeX code
  if (!is.infinite(result) && !is.na(result) && !is.nan(result))
    chunk <- paste(" & ", format(round(result, decimal_places), nsmall = decimal_places), sep = "")
  else
    chunk <- " & -"

  cat(paste("Min:", chunk, "\n"))
  return(chunk)
}

write_mode <- function(dependend, independend, value) {
  cat("MODE!\n")
}

write_obs <- function(dependend, independend, value) {
  cat("OBS!\n")
}

write_sd <- function(dependend, independend, value) {
  cat("SD!\n")
}

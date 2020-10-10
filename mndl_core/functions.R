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

write_ci <- function(dependend, independend, value) {
  cat("CI!\n")
}

write_max <- function(dependend, independend, value) {
  cat("MAX!\n")
}

write_mean <- function(dependend, independend, value) {
  cat("MEAN!\n")
}

write_med <- function(dependend, independend, value) {
  cat("MED!\n")
}

write_min <- function(dependend, independend, value) {
  cat("MIN!\n")
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

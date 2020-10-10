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

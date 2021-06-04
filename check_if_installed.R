packages = c("sparklyr", "ggplot2", "dbplot", "dplyr", "purrr", "ggmosaic", "forcats", "tidyr", "plumber", "callr", "httr")

package.check <- lapply(
  packages, 
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      # library(x, character.only = TRUE)
    }
  }
)

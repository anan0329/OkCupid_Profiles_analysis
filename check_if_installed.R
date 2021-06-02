packages = c("sparklyr", "ggplot2", "dbplot", "dplyr", "ggmosaic", "forcats", "tidyr")

package.check <- lapply(
  packages, 
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

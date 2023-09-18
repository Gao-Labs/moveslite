#' @name dev.R
#' @description Render the package!
#'

# Assume working directory is the project/package directory.

# Unload package if present
unloadNamespace(ns = "moveslite"); rm(list = ls()); remove.packages("moveslite")

# Build the package.

# Document the package
devtools::document()

# Test package
# devtools::load_all()
# catviz::core$keywords
# input = list(geoid = "36109", year = "2020", pollutant = "98")
# catviz::procure_p(input = input)
# Unload package
# unloadNamespace(ns = "catviz"); rm(list = ls()); remove.packages("catviz")

# Build the package!
devtools::build(path = getwd(), vignettes = FALSE)

# Unload it if present
unloadNamespace(ns = "moveslite"); rm(list = ls()); remove.packages("moveslite")

# Install package from source
install.packages("~/rstudio/moveslite/moveslite_0.1.0.tar.gz", type = "source")

# Restart R
.rs.restartR() # Only works in RStudio



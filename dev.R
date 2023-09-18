#' @name dev.R
#' @description Render the package!
#'

# Assume working directory is the project/package directory.
getwd()

# Unload package if present
unloadNamespace(ns = "moveslite"); rm(list = ls()); remove.packages("moveslite")

# Build the package.

# Document the package
devtools::document(".")

# Test package
# devtools::load_all()
# moveslite::keywords
# Unload package
# unloadNamespace(ns = "moveslite"); rm(list = ls()); remove.packages("moveslite")

# Build the package!
devtools::build(path = getwd(), vignettes = FALSE)

# Unload it if present
unloadNamespace(ns = "moveslite"); rm(list = ls()); remove.packages("moveslite")

# Install package from source
install.packages("~/rstudio/moveslite/moveslite_0.1.0.tar.gz", type = "source")

# Restart R
.rs.restartR() # Only works in RStudio



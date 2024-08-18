#' @name dev.R
#' @description Render the package!
#'

# Set directory to a package version
setwd(paste0(rstudioapi::getActiveProject(), "/v2"))

getwd()

# Unload package if present
unloadNamespace(ns = "moveslite"); rm(list = ls()); remove.packages("moveslite")

# Build the package.

# Document the package
devtools::document(".")
warnings()
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
# install.packages("~/rstudio/moveslite/moveslite_0.1.0.tar.gz", type = "source")

# # Copy to cat dashboard
# file.copy(from = "~/rstudio/moveslite/moveslite_0.1.0.tar.gz",
#           to = "~/rstudio/cat_dashboard/visualizer/z/moveslite_0.1.0.tar.gz",
#           overwrite = TRUE)
# Restart R
# .rs.restartR() # Only works in RStudio



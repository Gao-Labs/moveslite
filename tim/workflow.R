#' @name workflow.R
#'
#' @description A demo of the intended workflow for a `movesai` user.

library(dplyr)
library(DBI)


library(shiny)
library(shinyjs)
library(bslib)


source("R/analyze.R")

analyze()

usethis::create_tidy_package(path = ".", copyright_holder = "Gao-Labs")

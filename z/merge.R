#' @name merge.R
#' @author Tim Fraser
#' @description Script to help merge commits from one branch to another, when needed.
#'


system("git checkout tim")

system("git merge yan -- R/merge.R")

  "R/project.R",
  "R/convert.R",
  "R/find_transformation.R",
  "data/transformations.rda",
  "data/make_data.R",
  sep = " "))


% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/project.R
\name{project}
\alias{project}
\title{project}
\usage{
project(
  m,
  data,
  .newx,
  .cats = "year",
  .exclude = "geoid",
  .context = TRUE,
  .ci = 0.95
)
}
\arguments{
\item{m}{(character) "m" = model object}

\item{data}{data.frame of default data collected from CAT Grand Database}

\item{.newx}{data.frame, named vector, or list of new values for 1 or more x variables.}

\item{.cats}{vector of stratifying variable names, for which we should get estimates for each (eg. year)}

\item{.exclude}{vector of variable names to exclude from data.frame - eg. id variables}
}
\description{
Function to generate data.frames of projected emissions given
}
\author{
Tim Fraser & Yan Guo
}

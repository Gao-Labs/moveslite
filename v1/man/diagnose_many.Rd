% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/diagnose_many.R
\name{diagnose_many}
\alias{diagnose_many}
\title{Diagnostics for **many** models based on formula}
\usage{
diagnose_many(
  .geoid = "36109",
  .pollutant = 98,
  .by = 8,
  .type = 42,
  .formulas = list()
)
}
\arguments{
\item{.geoid}{(char) ".geoid" = location/county name}

\item{.pollutant}{(char) ".pollutant" = type of pollutant want to make prediction on}

\item{.by}{(char) ".by" = by = 16 = Overall, by = 8 = Sourcetype, by = 12 = Regulatory Class, by = 14 = Fueltype, by = 15 = Roadtype}

\item{.type}{(char) ".type" = depends on the .by}

\item{.formulas}{(list) ".formulas" = Statistical formula list}
}
\description{
For a list of formula, evaluate each model's goodness of fit and return identifiers.
}
\author{
Tim Fraser & Yan Guo
}

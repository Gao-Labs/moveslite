#' @name make_data.R
#' @author Tim Fraser
#' @description
#' This script generates data for use in the MOVESLite R package.
#' This data will be saved within the R package when rendered.
#' @importFrom dplyr `%>%` filter collect %>% tbl


library(dplyr)
library(readr)

# Possible transformations to search for
pattern_log = paste0(
  "log[(].*.",
  # Either...
  "(",
  # something like log(var ),
  "[ ]+",
  # OR
  "|",
  # Something like log(var)
  "",
  ")",
  # Followed by closing parenthesis.
  "[)]")

pattern_log10 = paste0(
  "log[(].*.",
  # comma
  "[,]",
  # Either no space or many
  "([ ]+|)",
  # The number 10
  "10",
  # Close
  "[)]")

pattern_sqrt = paste0(
  # Either"
  "(",
  # Standard sqrt(var) notation
  "sqrt[(].*.[)]",
  "|",
  # OR using I(var^.5) or I(var^(1/2)) or I(var^0.5)
  "I[(].*.[/^](0.5|.5|[(]1[/]2[)])[)]",
  # End of conditional
  ")")

pattern_cubert = paste0(
  # I(), with...
  "I[(].*.",
  # caret symbol
  "[/^]",
  # Fraction
  "[(]1[/]3[)]",
  # Closing parenthesis
  "[)]")

pattern_squared = "I[(].*.[/^]2[)]"
pattern_cubed = "I[(].*.[/^]3[)]"
#pattern_power = "I[(].*.[/^][0-9]+[)]"


transformations = bind_rows(
  tibble(trans = "log", backtrans = "exp(y)", pattern = pattern_log),
  tibble(trans = "log10", backtrans = "y^10", pattern = pattern_log10),
  tibble(trans = "sqrt", backtrans = "y^2", pattern = pattern_sqrt),
  tibble(trans = "cubert", backtrans = "y^3", pattern = pattern_cubert),
  tibble(trans = "squared", backtrans = "sqrt(y)", pattern = pattern_squared),
  tibble(trans = "cubed", backtrans = "y^(1/3)", pattern = pattern_cubed),
  tibble(trans = "asis", backtrans = NA, pattern = NA)
) %>%
  # Split into a list!
  split(.$trans)


save(transformations, file = "data/transformations.rda")

rm(list = ls())

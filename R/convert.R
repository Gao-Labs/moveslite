#' @name convert()
#' @author Tim Fraser
#' @description Function to convert outcome predictions and standard error back from a transformation, using simulation.
#' @importFrom dplyr %>% tibble
#' @importFrom stringr str_detect
#' @importFrom stats rnorm sd quantile
#' @importFrom base parse eval

convert = function(y, se, backtrans, df, ci = 0.95){
  # Execute the backtransformation on the original single estimate
  y_backtransformed = backtrans %>% parse(text = .) %>% eval()

  # Get 1000 simulated values
  # normally distributed around the original prediction y,
  # with a standard deviation of sigma
  # (Must call the output vector y, so that the parsing expression exp(y), etc. works on it)

  # If sample size were really large, we could do this
  # y = rnorm(n = 1000, mean = y, sd = se)

  # Since sample size is pretty small, we should do this.
  y = y + rt(n = 1000, df = df) * se

  # Now compute the backtransformation on the vector, producing ydist, in original units
  y_dist_backtransformed = backtrans %>% parse(text = .) %>% eval()

  # Calculate alpha level (eg. for 95% CI, alpha = 0.05)
  alpha = 1 - ci
  lower_ci = alpha / 2
  upper_ci = 1 - (alpha / 2)

  output = tibble(
    emissions = y_backtransformed,
    se = y_dist_backtransformed %>% sd(na.rm = TRUE),
    lower = y_dist_backtransformed %>% quantile(probs = lower_ci),
    upper = y_dist_backtransformed %>% quantile(probs = upper_ci),
  )

  return(output)
}

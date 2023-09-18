#' @name 03_ranges.R
#' @author Tim Fraser
#' @description Overall ridgeline plots!
#'

db = dbConnect(RSQLite::SQLite(), "diagnostics/diagnostics.sqlite")

# Get median for each set
stat = db %>%
  tbl("samples") %>%
  group_by(set_id, pollutant, by, type, formula_id, formula) %>%
  summarize(median = median(adjr, na.rm = TRUE),
            min = min(adjr, na.rm = TRUE),
            max = max(adjr, na.rm = TRUE)) %>%
  mutate(range = max - min) %>%
  collect()

stat %>%
  ggplot(mapping = aes(x = range, y = median, color = factor(formula_id) )) +
  geom_point() +
  scale_x_continuous(trans = "log")



# Find for me the median a
stat = db %>%
  tbl("samples") %>%
  group_by(set_id, formula_id, formula) %>%
  summarize(median = median(adjr, na.rm = TRUE),
            min = min(adjr, na.rm = TRUE),
            max = max(adjr, na.rm = TRUE),
            mean = mean(adjr, na.rm = TRUE),
            sd = sd(adjr, na.rm = TRUE),
            n = n()) %>%
  mutate(range = max - min) %>%
  group_by(formula_id, formula) %>%
  collect()


points = stat %>%
  summarize(
    min = min(median, na.rm = TRUE),
    lower = quantile(median, probs = 0.25),
    max = max(median, na.rm = TRUE),
    upper = quantile(median, probs = 0.75),
    range = max - min,
    iqr = upper - lower,
    median = median(median, na.rm = TRUE))


points %>%
  pivot_longer(cols = c(range, iqr), names_to = "type", values_to = "value") %>%
  ggplot(mapping = aes(x = value, y = median, label = formula_id)) +
  geom_point(shape = 21, color = "#373737", fill = "white", stroke = 1.2, size = 10) +
  geom_text(check_overlap = TRUE) +
  facet_wrap(~type, scales = "free_x")

# Clear
dbDisconnect(db); rm(list = ls()); gc()

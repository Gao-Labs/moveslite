#' @name 03_best_by_pollutant.R
#' @author Tim Fraser
#'


# 0. Packages ###########################################################

library(DBI)
library(RSQLite)
library(readr)
library(dplyr)
library(ggplot2)

db = dbConnect(RSQLite::SQLite(), "diagnostics/diagnostics.sqlite")

#db %>% tbl("samples") %>% summarize(n = n())

criteria = tribble(
  ~id, ~name,
  98, "CO2e",
  87, "VOC",
  100, "PM10",
  110, "PM2.5",
  31, "SO2",
  33, "NO2",
  2, "CO"
)


# What is the median / rank for each formula OVERALL?
stat_overall = db %>%
  tbl("samples") %>%
  filter(pollutant %in% !!criteria$id) %>%
  group_by(formula_id, formula) %>%
  summarize(median = median(adjr, na.rm = TRUE),
            valid = sum(adjr >= 0.90, na.rm = TRUE) / n()
  ) %>%
  ungroup() %>%
  collect() %>%
  mutate(valid = paste0(round(valid*100, 0), "%")) %>%
  arrange(desc(stat)) %>%
  mutate(rank = 1:n())

best_overall = stat_overall %>% filter(rank == 1)

# What percentage of models for each pollutant exceed these thresholds?
# Among all runs, count the number of geoid-sets where the explanatory power is...
values = db %>%
  tbl("samples") %>%
  filter(pollutant %in% !!criteria$id) %>%
  filter(formula_id == !!best_overall$formula_id) %>%
  collect()

# Booststrap some confidence intervals
n_reps = 1000

# Get bootstrapped confidence intervals!
cis = values %>%
  select(formula_id, pollutant, adjr) %>%
  tidyr::expand_grid(rep = 1:n_reps) %>%
  # Resample with replacement
  group_by(rep, pollutant) %>%
  sample_n(size = n(), replace = TRUE) %>%
  ungroup() %>%
  # Calculate stat
  group_by(rep, pollutant) %>%
  summarize(stat = median(adjr, na.rm = TRUE)) %>%
  ungroup() %>%
  # Calculate most frequent 95% range
  group_by(pollutant) %>%
  summarize(lower = quantile(stat, probs = 0.025, na.rm = TRUE),
            upper = quantile(stat, probs = 0.975, na.rm = TRUE)) %>%
  ungroup()

s = values %>%
  group_by(pollutant) %>%
  summarize(stat = median(adjr, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(by = "pollutant", y = cis) %>%
  mutate(pollutant_name = pollutant %>% dplyr::recode_factor(!!!purrr::set_names(criteria$name, criteria$id))) %>%
  mutate(across(.cols = c(stat, lower, upper),
                .fns = ~round(.x*100, 1))) %>%
  mutate(label = paste0(stat, "%"))

gg = s %>%
  ggplot(mapping = aes(x = reorder(pollutant_name, -stat), y = stat, ymin = lower, ymax = upper, label = label)) +
  geom_crossbar(fill = "#648FFF") +
  shadowtext::geom_shadowtext(bg.r = 0.2, bg.color = "white", color = "#373737") +
  labs(x = "Pollutant", y = "Median Accuracy (%)\n(95% bootstratpped confidence intervals)",
       title = "Expected Accuracy of Best Model (M1) by Pollutant") +
  theme_classic(base_size = 14)

ggsave(gg, filename = "diagnostics/fig_crossbars_by_pollutant.png", dpi = 500, width = 8, height = 6)


dbDisconnect(db); rm(list = ls()); gc()

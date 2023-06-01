#' @name 03_histogram.R
#' @author Tim Fraser
#' @description Overall CO2e histogram plots!
#'

# Clear environment and cache
rm(list = ls()); gc()

# Packages
library(dplyr)
library(ggplot2)
library(DBI)
library(RSQLite)

db = dbConnect(RSQLite::SQLite(), "diagnostics/diagnostics.sqlite")

# Gather Overall data sample
d = db %>%
  tbl("samples") %>%
  filter(pollutant == 98, by == 16) %>%
  collect()

# Get medians for each formula
stat = d %>%
  group_by(formula, formula_id) %>%
  summarize(median = median(adjr, na.rm = TRUE),
            label = scales::percent(median, accuracy = 0.1))


# Visualize Histograms
gg = d %>%
  ggplot(mapping = aes(x = adjr)) +
  geom_density(fill = "#648FFF") +
  shadowtext::geom_shadowtext(
    data = stat, mapping = aes(x = median, y = 100, label = label),
    bg.r = 0.3, bg.color = "white", color = "#373737") +
  facet_wrap(~formula_id, ncol = 5, scales = "free_x") +
  theme_minimal(base_size = 14) +
  theme(panel.border = element_rect(fill = NA,color = "#373737"),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank()) +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1)) +
  labs(x = "Accuracy (%) (Adjusted R2)",
       y = "Density",
       title = "Distribution of Model fit\nfor CO2e Aggregated Overall (n = 886 models)")

ggsave(gg, filename = "diagnostics/fig_histogram.png", dpi = 500, width = 8, height = 6)

# Clear environment and cache
dbDisconnect(db); rm(list = ls()); gc()

#' @name 03_grades.R
#' @title Visualization code for 'grade' diagnostics
#' @author Tim Fraser

# 0. Packages ###########################################################

library(DBI)
library(RSQLite)
library(readr)
library(dplyr)
library(ggplot2)

# 1. Grades #############################################################

db = dbConnect(RSQLite::SQLite(), "diagnostics/diagnostics.sqlite")

#db %>% tbl("samples") %>% summarize(n = n())
read_rds("diagnostics/runs_sample.rds") %>% nrow()

# stat = db %>%
#   tbl("samples") %>%
#   group_by(set_id, pollutant, by, type, formula_id, formula) %>%
#   summarize(median = median(adjr, na.rm = TRUE)) %>%
#   collect()


nruns = db %>%
  tbl("samples") %>%
  summarize(n = n()) %>%
  collect() %>%

  with(n)

nsets = db %>%
  tbl("samples") %>%
  select(set_id) %>%
  distinct() %>%
  summarize(n = n()) %>%
  collect() %>%

  with(n)

ngeoid = db %>%
  tbl("samples") %>%
  select(geoid) %>%
  distinct() %>%
  summarize(n = n()) %>%
  collect() %>%
  with(n)
#
#
# # Let's make a plot where...
# # - each panel is a model
# # - showing histograms of median explanatory power among sets
# # - showing what percentage of sets fall into the good, okay, and bad zone.
# zones = bind_rows(
#   tibble(type = "Danger", x = c(0, 0.90)),
#   tibble(type = "Warning", x = c(0.90, 0.95)),
#   tibble(type = "Success", x = c(0.95, 0.99)),
#   tibble(type = "Excellent", x = c(0.99, 1))
# ) %>% mutate(ymin = -Inf, ymax = Inf)
#

# We're going to compute the literal proportion of stats which we greater than/less than a certain zone

# d = stat %>%
#   group_by(formula_id, formula) %>%
#   reframe( median %>% density() %>% broom::tidy() %>%
#              approx(xout = seq(from = min(median, na.rm = TRUE), to = 1, length.out = 30), yleft = 0, yright = 0, method = "linear") %>%
#              as_tibble()
#   )

# limits = stat %>% with(median) %>% range(na.rm = TRUE)
#
# ggplot() +
#   geom_ribbon(data = zones, mapping = aes(x = x, ymin = ymin, ymax =ymax, fill = type),
#               alpha = 0.5) +
#   geom_area(data = d, mapping = aes(x = x, y = y), alpha = 0.5) +
#   facet_wrap(~formula, scales = "free_x", ncol = 5) +
#   coord_cartesian(xlim = limits) +
#   theme(legend.position = "bottom")


# Among all runs, count the number of geoid-sets where the explanatory power is...
blocks = db %>%
  tbl("samples") %>%
  group_by(formula_id, formula) %>%
  summarize(
    count = n(),
    danger = sum(adjr < 0.80, na.rm = TRUE),
    warning = sum(adjr >= 0.80 & adjr < 0.90, na.rm = TRUE),
    fine = sum(adjr >= 0.90 & adjr < 0.95, na.rm = TRUE),
    success = sum(adjr >= 0.95 & adjr < 0.99, na.rm = TRUE),
    excellent = sum(adjr >= 0.99, na.rm = TRUE) ) %>%
  ungroup() %>%
  collect() %>%
  tidyr::pivot_longer(cols = c(danger:excellent),
                      names_to = "quality", values_to = "n") %>%
  mutate(quality = factor(quality, levels = c("danger", "warning", "fine", "success", "excellent"))) %>%
  mutate(percent = n / count,
         label = paste0(round(percent*100, 0), "%"))


gg = ggplot() +
  geom_col(data = blocks, mapping = aes(x = factor(formula_id), y = n, fill = quality), position = 'fill') +
  shadowtext::geom_shadowtext(
    data = blocks, mapping = aes(x = factor(formula_id), y = n, group = quality, label = label),
    color = "#373737",
    position = "fill", bg.r = 0.2, bg.color = "white", hjust = 1, check_overlap = TRUE) +
#  coord_flip() +
  theme_classic(base_size = 14) +
  theme(panel.border = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_line(color = "#373737")) +
  scale_y_continuous(expand = expansion(add = c(0, 0.05)),
                     labels = scales::label_percent(accuracy = 1)) +
  scale_fill_manual(values = c("#648FFF", "#785EF0", "#DC267F", "#FE6100","#FFB000") %>% rev(),
                    breaks = c("danger", "warning", "fine", "success", "excellent"),
                    labels = c("Danger\n(<80%)", "Warning\n(80~89%)", "Fine\n(90-94%)", "Success\n(95~98%)", "Excellent\n(99~100%)")) +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "Model Formula",
       y = "% of Area-Pollutant-Aggregation-Subtype sets",
       fill = "Explanatory Power\n(Adj. R2)",
       title = paste0("Accuracy of Possible MOVESLite Formulas",
       "\nover ", nruns, " combinations", " of ", ngeoid, " areas", " and ", nsets, " settings"))


ggsave(gg, filename = "diagnostics/fig_grades.png", dpi = 500, width = 8, height = 6)

dbDisconnect(db)

rm(list = ls())

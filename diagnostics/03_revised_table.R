#' @name 03_revised_table
#' @description Investigate results of `diagnostics.sqlite` from `02_diagnostic_loop.R`
#' @author Yan Guo and Tim Fraser


# https://cran.r-project.org/web/packages/kableExtra/vignettes/awesome_table_in_html.html

# Can you...
# - Make this into a really nice Table 1
# - Repeat this for the six criterion polluants in a Table 2: CO, SO2, VOCC, CO2e, PM10, PM2.5
# - Estimates for the range of predictive accuracy? Eg. median sigma, or 95% quantile range around adjr?
# Highlight on our final prefer formula, with an explanation for why it's good
# - theoretical sense, high r2, and works for many situations

# Overall by 'by' #################################
##########################################################################################################
# the first plot with by = 16, 8 and 14
##########################################################################################################


library(DBI)
library(RSQLite)
library(readr)
library(dplyr)
library(ggplot2)
library(forcats)

db = dbConnect(RSQLite::SQLite(), "diagnostics/diagnostics.sqlite")


criteria = tribble(
  ~id, ~name,
  16, "Overall",
  8, "By sourcetype",
  14, "By fueltype"
)



# What is the median / rank for each formula for each by
stat_overall = db %>%
  tbl("samples") %>%
  filter(by %in% !!criteria$id) %>%
  group_by(formula_id, formula) %>%
  summarize(stat = median(adjr, na.rm = TRUE),
            valid = sum(adjr >= 0.90, na.rm = TRUE) / 9560
  ) %>%
  ungroup() %>%
  collect() %>%
  mutate(valid = paste0(round(valid*100, 0), "%")) %>%
  arrange(desc(stat)) %>%
  mutate(rank = 1:n())



# What percentage of models for each by exceed these thresholds?
# Among all runs, count the number of geoid-sets where the explanatory power is...
stat_by = db %>%
  tbl("samples") %>%
  filter(by %in% !!criteria$id) %>%
  group_by(formula_id, formula, by) %>%
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
  group_by(by) %>%
  mutate(percent = n / count,
         label = paste0(round(percent*100, 0), "%")) %>%
  mutate(by = by %>% dplyr::recode_factor(
    "16" = "Overall",
    "14" = "By Fueltype",
    "8" =  "By Source")) %>%
  mutate(by = forcats::fct_relevel(by, c("Overall", "By Fueltype", "By Source")),
         test = toupper(by))

best = tibble(formula_id = 51, percent = 1, rank = 1)

blocks = stat_by %>%
  left_join(by = "formula_id", y = stat_overall %>% select(formula_id, stat, rank))

gg = ggplot() +
  geom_col(data = blocks,
           mapping = aes(
             x = reorder(formula_id, -rank),
             y = percent,
             fill = quality),
           position = "fill") +
  geom_col(data = best,
           mapping = aes(x = reorder(formula_id, -rank),
                         y = percent),
           fill = NA, color = "black") +
  coord_flip() +
  facet_wrap(
    facets = ~by, ncol = 3,
    labeller = labeller(test = by)
  ) +
  shadowtext::geom_shadowtext(
    data = blocks, mapping = aes(x = factor(formula_id), y = n, group = quality, label = label),
    color = "#373737",
    position = "fill", size = 2.5, bg.r = 0.2, bg.color = "white", hjust = 1, check_overlap = TRUE) +
  theme_classic(base_size = 14) +
  theme(panel.border = element_blank(),
        strip.text.x = element_text(color = "#373737"),
        strip.background.x = element_blank(),
        axis.text = element_text(size = 8, color = "#373737"),
        axis.title.x = element_text(color = "#373737"),
        axis.title.y = ggtext::element_markdown(color = "#373737"),
        legend.title = element_text(color = "#373737"),
        plot.title = element_text(color = "#373737"),
        axis.line.y = element_blank(),
        panel.spacing.x = unit(1, "cm"),
        plot.margin = margin(0,r = 0.5,0,0, "cm"),
        axis.line.x = element_line(color = "#373737")) +
  scale_y_continuous(expand = expansion(add = c(0, 0.0)),
                     labels = scales::label_percent(accuracy = 1)) +
  scale_fill_manual(values = c("#648FFF", "#785EF0", "#DC267F", "#FE6100","#FFB000") %>% rev(),
                    breaks = c("danger", "warning", "fine", "success", "excellent"),
                    labels = c("Danger\n(<80%)", "Warning\n(80~89%)", "Fine\n(90-94%)", "Success\n(95~98%)", "Excellent\n(99~100%)")) +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  labs(y = "% of Sets",
       fill = "Accuracy\n(Adj. R2)",
       title = paste0("Formula Accuracy in Sample by Aggregation Level"),
       x = "Model Formula ID<br><sup>Sorted by Median Explanatory Power Overall</sup>")


ggsave(gg, filename = "diagnostics/fig_grades_by_slice_test.png", dpi = 500, width = 10, height = 7)
browseURL("diagnostics/fig_grades_by_slice_test.png")

# By Pollutant ################################
##########################################################################################################
# The second plot with six criterion pollutants CO, SO2, VOCC, CO2e, PM10, PM2.5
##########################################################################################################

db = dbConnect(RSQLite::SQLite(), "diagnostics/diagnostics.sqlite")

criteria = tribble(
  ~id, ~name,
  98, "CO2e",
  87, "VOC",
  100, "PM10",
  110, "PM2.5",
  31, "SO2",
  6, "NO2",
  2, "CO"
) %>%
  # Filter out VOC. I don't have confidence in VOC currently.
  filter(id != 87)

# What is the median / rank for each formula OVERALL?
stat_overall = db %>%
  tbl("samples") %>%
  filter(pollutant %in% !!criteria$id) %>%
  group_by(formula_id, formula) %>%
  summarize(stat = median(adjr, na.rm = TRUE),
            valid = sum(adjr >= 0.90, na.rm = TRUE),
            test = n()
  ) %>%
  collect()

stat_overall = stat_overall %>%
  mutate(valid = valid/test) %>%
  ungroup() %>%
  mutate(valid = paste0(round(valid*100, 0), "%")) %>%
  arrange(desc(stat)) %>%
  mutate(rank = 1:n()) %>%
  subset(select = -c(test) )


# What percentage of models for each pollutant exceed these thresholds?
# Among all runs, count the number of geoid-sets where the explanatory power is...
stat_pollutant = db %>%
  tbl("samples") %>%
  filter(pollutant %in% !!criteria$id) %>%
  group_by(formula_id, formula, pollutant) %>%
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
  group_by(pollutant) %>%
  mutate(percent = n / count,
         label = paste0(round(percent*100, 0), "%"))

blocks = stat_pollutant %>%
  left_join(by = "formula_id", y = stat_overall %>% select(formula_id, stat, rank)) %>%
  mutate(pollutant = factor(pollutant,
                            levels = c(98, 110, 100, 31, 6, 2),
                            labels = c("CO2 Equivalent", "PM2.5", "PM10", "SO2", "NO2", "CO")))

best = tibble(formula_id = 51, percent = 1, rank = 1)
gg = ggplot() +
  geom_col(data = blocks,
           mapping = aes(
             x = reorder(formula_id, -rank),
             y = percent,
             fill = quality),
           position = "fill") +
  geom_col(data = best,
           mapping = aes(x = reorder(formula_id, -rank),
                         y = percent),
           fill = NA, color = "black") +
  coord_flip() +
  facet_wrap(
    facets = ~pollutant, ncol = 3
    #labeller = as_labeller(purrr::set_names(criteria$name, criteria$id))
  ) +
  shadowtext::geom_shadowtext(
    data = blocks, mapping = aes(x = factor(formula_id), y = n, group = quality, label = label),
    color = "#373737",
    position = "fill", size = 2.5, bg.r = 0.2, bg.color = "white", hjust = 1, check_overlap = TRUE) +
  theme_classic(base_size = 14) +
  theme(panel.border = element_blank(),
        strip.text.x = element_text(color = "#373737"),
        strip.background.x = element_blank(),
        axis.text = element_text(size = 8, color = "#373737"),
        axis.title.x = element_text(color = "#373737"),
        axis.title.y = ggtext::element_markdown(color = "#373737"),
        legend.title = element_text(color = "#373737"),
        plot.title = element_text(color = "#373737"),
        axis.line.y = element_blank(),
        panel.spacing.x = unit(1, "cm"),
        plot.margin = margin(0,r = 0.5,0,0, "cm"),
        axis.line.x = element_line(color = "#373737")) +
  scale_y_continuous(expand = expansion(add = c(0, 0.0)),
                     labels = scales::label_percent(accuracy = 1)) +
  scale_fill_manual(values = c("#648FFF", "#785EF0", "#DC267F", "#FE6100","#FFB000") %>% rev(),
                    breaks = c("danger", "warning", "fine", "success", "excellent"),
                    labels = c("Danger\n(<80%)", "Warning\n(80~89%)", "Fine\n(90-94%)", "Success\n(95~98%)", "Excellent\n(99~100%)")) +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  labs(y = "% of Sets",
       fill = "Accuracy\n(Adj. R2)",
       title = paste0("Formula Accuracy in Sample by Pollutant"),
       x = "Model Formula<br><sup>Sorted by Median Explanatory Power Overall</sup>")



ggsave(gg, filename = "diagnostics/fig_grades_by_pollutant_test.png",
       dpi = 500, width = 10, height = 11)
browseURL("diagnostics/fig_grades_by_pollutant_test.png")


##########################################################################################################
# table1 with six criterion pollutants CO, SO2, VOCC, CO2e, PM10, PM2.5 with the best model (formula id == 51)
##########################################################################################################

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

s


dbDisconnect(db); rm(list = ls()); gc()




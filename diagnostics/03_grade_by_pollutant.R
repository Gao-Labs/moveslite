#' @name 03_grade_by_pollutant.R
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
  summarize(stat = median(adjr, na.rm = TRUE),
            valid = sum(adjr >= 0.90, na.rm = TRUE) / n()
  ) %>%
  ungroup() %>%
  collect() %>%
  mutate(valid = paste0(round(valid*100, 0), "%")) %>%
  arrange(desc(stat)) %>%
  mutate(rank = 1:n())

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
  left_join(by = "formula_id", y = stat_overall %>% select(formula_id, stat, rank))


gg = ggplot() +
  geom_col(data = blocks,
           mapping = aes(
             x = reorder(formula_id, -rank),
             y = percent,
             fill = quality),
           position = "fill") +
  coord_flip() +
  facet_wrap(
    facets = ~pollutant, ncol = 3,
             labeller = as_labeller(purrr::set_names(criteria$name, criteria$id))
  ) +
  shadowtext::geom_shadowtext(
    data = blocks, mapping = aes(x = factor(formula_id), y = n, group = quality, label = label),
    color = "#373737",
    position = "fill", size = 2.5, bg.r = 0.2, bg.color = "white", hjust = 1, check_overlap = TRUE) +
  theme_classic(base_size = 14) +
  theme(panel.border = element_blank(),
        strip.text.x = element_text(color = "#373737"),
        strip.background.x = element_blank(),
        axis.text = element_text(color = "#373737"),
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



ggsave(gg, filename = "diagnostics/fig_grades_by_pollutant.png", dpi = 500, width = 8, height = 6)


dbDisconnect(db); rm(list = ls()); gc()

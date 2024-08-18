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
  ~id, ~name, ~fullname, ~order,
  98, "CO2e", "<b>CO2 Equivalent</b> (CO2e)", 1,
#  87, "VOC", "<b>Volatile Organic Compounds</b> (VOC)",  6,
  100, "PM10", "<b>Particulate Matter &leq;10µm</b> (PM<sub>10</sub>)", 3,
  110, "PM2.5", "<b>Particulate Matter &leq;2.5µm</b> (PM<sub>2.5</sub>)", 2,
  31, "SO2", "<b>Sulfur Dioxide</b> (SO<sub>2</sub>)",  5,
  6, "N20", "<b>Nitrous Oxide</b> (N<sub>2</sub>O)", 7,
  2, "CO",  "<b>Carbon Monoxide</b> (CO)", 4
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
sample_by_pollutants = db %>%
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
  collect()

# db %>%
#   tbl("samples") %>%
#   filter(pollutant == 33)

# sample_by_pollutants %>%
#   filter(pollutant == 6)

dbDisconnect(db); remove(db)

stat_pollutant = sample_by_pollutants %>%
  tidyr::pivot_longer(cols = c(danger:excellent),
                      names_to = "quality", values_to = "n") %>%
  mutate(quality = factor(quality, levels = c("danger", "warning", "fine", "success", "excellent"))) %>%
  group_by(pollutant) %>%
  mutate(percent = n / count,
         label = paste0(round(percent*100, 0), "%")) %>%
  left_join(y = criteria, by = c("pollutant" = "id"))



blocks = stat_pollutant %>%
  left_join(by = "formula_id", y = stat_overall %>% select(formula_id, stat, rank))

# Create a label for what share of models rate as fine, success, or excellent?
blockgroups = blocks %>%
  group_by(formula_id, formula, order, pollutant, stat, rank) %>%
  summarize(n = sum(n[ quality == "success" | quality == "excellent" ], na.rm = TRUE),
            quality = quality[percent == max(percent)],
            count = count[1],
            percent = n / count,
            label = scales::percent(percent, accuracy = 1))

# Create boxes to go around the best fitting formula
boxes = blockgroups %>%
  filter(formula_id == 51)


# REVISED VISUAL WITH SELECT LABELS ##################################
gg = ggplot() +
  # BLOCKS ##########################
  geom_col(data = blocks,
         mapping = aes(
           x = reorder(formula_id, -rank),
           y = percent,
           fill = quality),
         width = 0.98,
         position = "fill") +
  # BOXES ##########################
  geom_col(
    data = boxes,
    mapping = aes(
      x = reorder(formula_id, -rank),
      y = 1
    ),
    fill = NA, color = "black", size = 0.75
  ) +
  # PERCENTAGES ####################
  shadowtext::geom_shadowtext(
    data = blockgroups,
    mapping = aes(x = reorder(factor(formula_id), -rank), y = percent, group = quality, label = label),
    color = "#373737",
    #position = "fill",
    size = 3, bg.r = 0.3, bg.color = "white", hjust = 1, check_overlap = TRUE) +

  # POSITIONING #####################
  coord_flip() +
    facet_wrap(
      facets = ~order,
      labeller = as_labeller(setNames(nm = criteria$order, criteria$fullname)),
      ncol = 3, nrow = 2
    ) +
  # Y-AXIS #####################
  scale_y_continuous(
    expand = expansion(add = c(0, 0.0)),
    labels = scales::label_percent(accuracy = 1)) +
  # FILL SCALE #######################
  scale_fill_manual(
    values = c("#648FFF", "#785EF0", "#DC267F", "#FE6100","#FFB000") %>% rev(),
    breaks = c("danger", "warning", "fine", "success", "excellent"),
    labels = c("Danger\n(<80%)", "Warning\n(80~89%)", "Fine\n(90-94%)", "Success\n(95~98%)", "Excellent\n(99~100%)")) +
    theme_classic(base_size = 14) +
    theme(
      # STRIPS ######################
      strip.text.x = ggtext::element_markdown(color = "#373737", size = 11),
      strip.background.x = element_blank(),
      # AXES ##############################
      axis.text.y = element_text(color = "#373737", size = 8),
      axis.text.x = element_text(color = "#373737", size = 10),
      axis.title.x = element_text(color = "#373737"),
      axis.title.y = ggtext::element_markdown(color = "#373737"),
      axis.line.y = element_blank(),
      axis.line.x = element_line(color = "#373737"),
      # LEGEND ######################
      legend.title = element_text(color = "#373737"),
      legend.position = "bottom",
      plot.title = element_text(color = "#373737", hjust = 0.5),
      # PANEL ##################
      panel.border = element_blank(),
      panel.spacing.x = unit(0.75, "cm"),
      legend.margin = margin(0,0,b = 0.1,0, "cm"),
      plot.margin = margin(0,r = 0.5,0,0, "cm")
    ) +
    labs(
      y = "% of Sets",
      fill = "Accuracy\n(Adj. R2)",
      title = paste0("Formula Accuracy in Sample by Pollutant"),
      x = "Model Formula<br><sup>Sorted by Median Explanatory Power Overall</sup>"
    )

ggsave(gg, filename = "diagnostics/figure_3.png", dpi = 500, width = 9, height = 11)
browseURL("diagnostics/figure_3.png")


rm(list = ls()); gc()

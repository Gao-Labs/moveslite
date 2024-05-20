
# 1. Overall by 'by' #################################
# the first plot with by = 16, 8 and 14

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


# Create a label for what share of models rate as fine, success, or excellent?
blockgroups = blocks %>%
  group_by(formula_id, formula,by, stat, rank) %>%
  summarize(n = sum(n[quality == "success" | quality == "excellent" ], na.rm = TRUE),
            quality = quality[percent == max(percent)],
            count = count[1],
            percent = n / count,
            label = scales::percent(percent, accuracy = 1))

# Create boxes to go around the best fitting formula
boxes = blockgroups %>%
  filter(formula_id == 51)



gg = ggplot() +
  geom_col(data = blocks,
           mapping = aes(
             x = reorder(formula_id, -rank),
             y = percent,
             fill = quality),
           width = 0.98,
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

  # shadowtext::geom_shadowtext(
  #   data = blocks, mapping = aes(x = factor(formula_id), y = n, group = quality, label = label),
  #   color = "#373737",
  #   position = "fill", size = 2.5, bg.r = 0.2, bg.color = "white", hjust = 1, check_overlap = TRUE) +
  theme_classic(base_size = 14) +
  theme(

    # panel.border = element_blank(),
    #     strip.text.x = element_text(color = "#373737"),
    #     strip.background.x = element_blank(),
    #     axis.text = element_text(size = 8, color = "#373737"),
    #     axis.title.x = element_text(color = "#373737"),
    #     axis.title.y = ggtext::element_markdown(color = "#373737"),
    #     legend.title = element_text(color = "#373737"),
    #     plot.title = element_text(color = "#373737"),
    #     axis.line.y = element_blank(),
    #     panel.spacing.x = unit(1, "cm"),
    #     plot.margin = margin(0,r = 0.5,0,0, "cm"),
    #     axis.line.x = element_line(color = "#373737")

        # STRIPS ######################
        strip.text.x = ggtext::element_markdown(color = "#373737", size = 15),
        strip.background.x = element_blank(),
        # AXES ##############################
        axis.text.y = element_text(color = "#373737", size = 10),
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

ggsave(gg, filename = "diagnostics/figure_2.png", dpi = 500, width = 9, height = 8)
browseURL("diagnostics/figure_2.png")

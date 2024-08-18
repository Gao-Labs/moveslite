# Table A2
# Table of Model 51 Scores, broken down by each aggregation level and each aggregation type

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
  mutate(rank = 1:n()) %>%
  # Filter to best
  filter(rank == 1)

# What percentage of models for each pollutant exceed these thresholds?
# Among all runs, count the number of geoid-sets where the explanatory power is...
stat_by_pollutants = db %>%
  tbl("samples") %>%
  filter(pollutant %in% !!criteria$id) %>%
  # Filter to best overall
  filter(formula_id %in% !!stat_overall$formula_id) %>%
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
         label = paste0(round(percent*100, 0), "%")) %>%
  left_join(y = criteria, by = c("pollutant" = "id"))

stat_by = db %>%
  tbl("samples") %>%
  filter(by %in% !!c(16,8,14)) %>%
  # Filter to best overall
  filter(formula_id %in% !!stat_overall$formula_id) %>%
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


dbDisconnect(db); remove(db)
# Old format
# bind_rows(
#   stat_by %>%
#     ungroup() %>%
#     mutate(qorder = as.integer(quality)) %>%
#     select(name = by, qorder, quality, n, percent, label) %>%
#     arrange(name, desc(qorder)) %>%
#     group_by(name) %>%
#     mutate(cumulative = cumsum(percent) %>% scales::percent(accuracy = 1)),
#
#   stat_pollutant %>%
#     ungroup() %>%
#     mutate(qorder = as.integer(quality)) %>%
#     select(name, qorder, quality, n, percent, label) %>%
#     arrange(desc(qorder)) %>%
#     group_by(name) %>%
#     mutate(cumulative = cumsum(percent) %>% scales::percent(accuracy = 1))
# ) %>%
#   pivot_wider(id_cols = c(qorder, quality), names_from = name, values_from = c(label, cumulative) ) %>%
#   arrange(desc(qorder)) %>%
#   select(quality, contains("Overall"), contains("By Fueltype"), contains("Source"),
#          contains("CO2"), contains("PM2.5"), contains("PM10"),
#          contains("CO"),  contains("SO2"), contains("N20")) %>%
#   write_csv("diagnostics/table_a2.csv")


bind_rows(
  stat_by %>%
    ungroup() %>%
    mutate(qorder = as.integer(quality)) %>%
    select(name = by, qorder, quality, n, percent, label) %>%
    arrange(name, desc(qorder)) %>%
    group_by(name) %>%
    mutate(cumulative = cumsum(percent) %>% scales::percent(accuracy = 1)),

  stat_pollutant %>%
    ungroup() %>%
    mutate(qorder = as.integer(quality)) %>%
    select(name, qorder, quality, n, percent, label) %>%
    arrange(desc(qorder)) %>%
    group_by(name) %>%
    mutate(cumulative = cumsum(percent) %>% scales::percent(accuracy = 1))
) %>%
  select(name, qorder, quality, label, cumulative) %>%
  tidyr::pivot_longer(cols = c(label, cumulative), names_to = "type", values_to = "value") %>%
  mutate(value = parse_number(value)) %>%
  tidyr::pivot_wider(id_cols = c(name, type), names_from = quality, values_from = value) %>%
  write_csv("diagnostics/table_a2.csv")


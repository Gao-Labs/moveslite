#' @name 03_ridgeline.R
#' @author Tim Fraser
#' @description Overall ridgeline plots!
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
            label = scales::percent(median, accuracy = 0.1)) %>%
  ungroup() %>%
  mutate(formula = formula %>% dplyr::recode(
    "poly(vmt, 2) + poly(vehicles, 2) + poly(sourcehours, 2) + +starts + year" =
      c("&beta;<sub>1</sub>year",
        "&beta;<sub>2</sub>vmt",
        "&beta;<sub>3</sub>vmt<sup>2</sup>",
        "&beta;<sub>4</sub>vehicles",
        "&beta;<sub>5</sub>vehicles<sup>2</sup>",
        "&beta;<sub>6</sub>sourcehours",
        "&beta;<sub>7</sub>sourcehours<sup>2</sup>",
        "&beta;<sub>8</sub>starts") %>%
      paste0(collapse = " + "),
    "poly(vmt, 2) + poly(vehicles, 2) + poly(sourcehours, 2) + poly(starts, 2) + year" =
      c("&beta;<sub>1</sub>year",
        "&beta;<sub>2</sub>vmt",
        "&beta;<sub>3</sub>vmt<sup>2</sup>",
        "&beta;<sub>4</sub>vehicles",
        "&beta;<sub>5</sub>vehicles<sup>2</sup>",
        "<br>",
        "&beta;<sub>6</sub>sourcehours",
        "&beta;<sub>7</sub>sourcehours<sup>2</sup>",
        "&beta;<sub>8</sub>starts",
        "&beta;<sub>9</sub>starts<sup>2</sup>") %>%
      paste0(collapse = " + "),
    "poly(vmt, 2) + poly(vehicles, 2) + sourcehours + poly(starts, 2) + year" =
      c("&beta;<sub>1</sub>year",
        "&beta;<sub>2</sub>vmt",
        "&beta;<sub>3</sub>vmt<sup>2</sup>",
        "&beta;<sub>4</sub>vehicles",
        "&beta;<sub>5</sub>vehicles<sup>2</sup>",
        "&beta;<sub>6</sub>sourcehours",
        "&beta;<sub>7</sub>starts",
        "&beta;<sub>8</sub>starts<sup>2</sup>") %>%
      paste0(collapse = " + "),

    "poly(vmt, 2) + vehicles + poly(sourcehours, 2) + poly(starts, 2) + year" =
      c("&beta;<sub>1</sub>year",
        "&beta;<sub>2</sub>vmt",
        "&beta;<sub>3</sub>vmt<sup>2</sup>",
        "&beta;<sub>4</sub>vehicles",
        "&beta;<sub>5</sub>sourcehours",
        "&beta;<sub>6</sub>sourcehours<sup>2</sup>",
        "&beta;<sub>7</sub>starts",
        "&beta;<sub>8</sub>starts<sup>2</sup>") %>%
      paste0(collapse = " + "),

    "poly(vmt, 2) + vehicles + poly(sourcehours, 2) + starts + year" =
      c("&beta;<sub>1</sub>year",
        "&beta;<sub>2</sub>vmt",
        "&beta;<sub>3</sub>vmt<sup>2</sup>",
        "&beta;<sub>4</sub>vehicles",
        "&beta;<sub>5</sub>sourcehours",
        "&beta;<sub>6</sub>sourcehours<sup>2</sup>",
        "&beta;<sub>7</sub>starts") %>%
      paste0(collapse = " + "),


    "poly(vmt, 2) + vehicles + poly(sourcehours, 2) + starts + year + year * vmt + year * sourcehours + vmt * sourcehours" =
      c("&beta;<sub>1</sub>year",
        "&beta;<sub>2</sub>vmt",
        "&beta;<sub>3</sub>vmt<sup>2</sup>",
        "&beta;<sub>4</sub>year&times;vmt",
        "&beta;<sub>5</sub>vehicles",
        "<br>",
        "&beta;<sub>6</sub>sourcehours",
        "&beta;<sub>7</sub>sourcehours<sup>2</sup>",
        "&beta;<sub>8</sub>year&times;sourcehours",
        "&beta;<sub>9</sub>vmt&times;sourcehours",
        "&beta;<sub>10</sub>starts") %>%
      paste0(collapse = " + "),

    "poly(vmt, 3) + year + vmt * year" = c(
      "&beta;<sub>1</sub>year",
      "&beta;<sub>2</sub>vmt",
      "&beta;<sub>3</sub>vmt<sup>2</sup>",
      "&beta;<sub>3</sub>vmt<sup>3</sup>",
      "&beta;<sub>4</sub>year&times;vmt") %>%
      paste0(collapse = " + "),

    "sqrt(vmt) + sqrt(vehicles) + sqrt(sourcehours) + sqrt(starts) + year" =
      c("&beta;<sub>1</sub>year",
        "&beta;<sub>2</sub>&radic;vmt",
        "&beta;<sub>3</sub>&radic;vehicles",
        "&beta;<sub>4</sub>&radic;sourcehours",
        "&beta;<sub>5</sub>&radic;starts") %>%
      paste0(collapse = " + "),
    "vmt + poly(vehicles, 2) + poly(sourcehours, 2) + poly(starts, 2) + year" =
      c("&beta;<sub>1</sub>year",
        "&beta;<sub>2</sub>vmt",
        "&beta;<sub>3</sub>vehicles",
        "&beta;<sub>4</sub>vehicles<sup>2</sup>",
        "&beta;<sub>5</sub>sourcehours",
        "&beta;<sub>6</sub>sourcehours<sup>2</sup>",
        "&beta;<sub>7</sub>starts",
        "&beta;<sub>8</sub>starts<sup>2</sup>") %>%
      paste0(collapse = " + "),

    "year + vmt + sourcehours + vehicles + starts + year * vmt + year * sourcehours + vmt * sourcehours" =
      c("&beta;<sub>1</sub>year",
        "&beta;<sub>2</sub>vmt",
        "&beta;<sub>3</sub>year&times;vmt",
        "&beta;<sub>4</sub>vehicles",
        "&beta;<sub>5</sub>year&times;vehicles",
        "&beta;<sub>6</sub>sourcehours",
        "&beta;<sub>7</sub>year&times;sourcehours",
        "&beta;<sub>8</sub>starts"
      ) %>% paste0(collapse = " + ")
  )) %>%
  arrange(median) %>%
  mutate(rank = 1:n()) %>%
  # Check if there's a boost needed
  mutate(boost = if_else(stringr::str_detect(formula, "[<]br[>]"), true = 1, false = 0))


# Get curve
curve = d %>%
  group_by(formula_id) %>%
  reframe(
    adjr %>% density() %>% broom::tidy() %>%
      approx(xout = seq(from = 0, to = 1, length.out = 1000), yleft = NA, yright = NA, method = "linear") %>%
      as_tibble() ) %>%
  # Normalize as percentage of total
  mutate(y = y / sum(y, na.rm = TRUE) ) %>%
  # Join in rank
  left_join(by = "formula_id", y = stat %>% select(formula_id, rank)) %>%
  # Create an extra yaxis, for the zeros
  mutate(yextra = if_else(is.na(y), 0, y))

# Set crop point for x-axis
xend = 0.3

# Get sample size
n = d %>% nrow()

gg = ggplot() +
  # Plot Model Specifications
  ggtext::geom_richtext(
    data = stat, mapping = aes(x = xend, y = rank + 0.2 + boost * 0.1, label = formula),
    hjust = 0, size = 3, color = "#37373790", fill = NA, label.colour = NA, text.colour = "#37373790") +
  # Plot Ribbons
  geom_ribbon(data = curve, mapping = aes(x = x, ymin = rank, ymax = y*100 + rank, group = formula_id),
              fill = "#648FFF",
              alpha = 0.75) +
  # Plot Thin Line
  geom_line(data = curve, mapping = aes(x = x, y = yextra*100 + rank, group = formula_id), color = "grey", linewidth = 0.25) +
  # Plot Thick Line
  geom_line(data = curve %>% filter(!is.na(y)), mapping = aes(x = x, y = y*100 + rank, group = formula_id), color = "grey", linewidth = 1) +
  # Plot Median Text
  shadowtext::geom_shadowtext(
    data = stat, mapping = aes(x = median, y = rank + 0.5, label = label),
    bg.r = 0.3, bg.color = "white", color = "black") +
  scale_x_continuous(expand = expansion(c(0,0.05))) +
  theme_void(base_size = 14) +
  theme(axis.title.y = ggtext::element_markdown(color = "#373737", angle = 90, margin = margin(0, 0.25, 0, 0, "cm")),
        axis.title.x = element_text(color = "#373737", margin = margin(t = 0.05, 0,0,0, "cm")),
        plot.title = element_text(hjust = 0.5, color = "#373737"),
        plot.subtitle = element_text(hjust = 0.5, color = "#373737") ) +
  coord_cartesian(xlim = c(xend, 1)) +
  labs(x = "Accuracy (%) (Adjusted R2)",
       y = "Frequency<br><sup>Ridgelines ordered by Median Accuracy per Formula</sup>",
       title = "Accuracy Distributions by Model Formula",
       subtitle = paste0(
         "for CO2e Aggregated Overall (", n, " models)"))



ggsave(gg, filename = "diagnostics/fig_ridgeline.png", dpi = 500, width = 8, height = 6)

dbDisconnect(db); rm(list = ls()); gc()

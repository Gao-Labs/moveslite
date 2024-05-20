#' @name 03_figure_4.R
#' @author Tim Fraser and Yan Guo (revised from Yan Guo)
#' @description
#' Case Study Analytics

# Load packages
library(dplyr)
library(broom)
library(DBI)
library(RSQLite)
library(ggplot2)

#' These are the 5 core functions in `moveslite`.
# source("R/connect.R")
# source("R/query.R")
# source("R/setx.R")
# source("R/estimate.R")
# source("R/project.R")
# Load the moveslite functions
devtools::load_all(".")

# Connect to case study mini-CATSERVER example
db = dbConnect(drv = RSQLite::SQLite(), "diagnostics/case_study.sqlite")

# Download data (should end up with ~14 rows)
default = query(
  .db = db,
  .table = "d36109",
  .filters = c(.pollutant = 98, .by = 8, .sourcetype = 42),
  .vars = c("year", "vmt", "vehicles", "sourcehours", "starts"))
# Disconnect
dbDisconnect(db); remove(db)

default
# Compute the model
m = estimate(data = default, .vars = c("vmt", "vehicles", "starts", "sourcehours", "year"), .best = TRUE)
# Equivalent to this:
# formula1 = log(emissions) ~ poly(year,2) + poly(log(vmt), 3) + vehicles + sourcehours + starts
# m = default %>% lm(formula = formula1)


# Let's write a function
# that estimates realistic values for vmt vehicles statrs and sourcehours when vmt or vehicles changes.
new = function(.year = 2020, .vmt =43647,  .vehicles = 10, .mph = 13){
  tibble(
    year = .year,
    vmt = .vmt * .vehicles,
    vehicles = .vehicles,
    sourcehours = .vehicles *.vmt/.mph,
    starts = 1300*.vehicles
  )
}


# Generate predictor values for predictions
d1 = bind_rows(
  new(.year = 2020, .vehicles = 20),
  new(.year = 2025, .vehicles = 30),
  new(.year = 2030, .vehicles = 40),
  new(.year = 2040, .vehicles = 60),
  new(.year = 2050, .vehicles = 80)
)

d2 = bind_rows(
  new(.year = 2020, .vehicles = 40),
  new(.year = 2025, .vehicles = 30),
  new(.year = 2030, .vehicles = 20),
  new(.year = 2040, .vehicles = 15),
  new(.year = 2050, .vehicles = 10)
)

myvmt = 43647
d3 = bind_rows(
  new(.year = 2020, .vmt = myvmt),
  new(.year = 2025, .vmt = myvmt - 1000),
  new(.year = 2030, .vmt = myvmt - 2000),
  new(.year = 2040, .vmt = myvmt - 3000),
  new(.year = 2050, .vmt = myvmt - 4000)
)

output

# Generate predictions and bundle together
output = bind_rows(d1,d2,d3, .id = "id") %>%
  mutate(emissions = predict(m, newdata = .) %>% exp())

# TABLE A3 #########################
output %>%
  mutate(scenario = id %>% recode(
    "1" = "Scenario 1",
    "2" = "Scenario 2",
    "3" = "scenario 3"
  ),
  type = id %>% recode(
    "1" = "Increasing Vehicles",
    "2" = "Decreasing Vehicles",
   "3" =  "Decreasing VMT"
  )
  ) %>%
  select(id, scenario, type, year, vmt, vehicles, sourcehours, starts, emissions) %>%
  mutate(across(vmt:emissions, .fns = ~scales::number(.x, accuracy = 0.1, decimal.mark = ".", big.mark = ","))) %>%
  write_csv("diagnostics/table_a3.csv")

# FIGURE 4 ############################

# Equivalent
# output = bind_rows(
#   moveslite::project(m, data = default, .newx = d1, .ci = 0.95),
#   moveslite::project(m, data = default, .newx = d1, .ci = 0.95),
#   moveslite::project(m, data = default, .newx = d1, .ci = 0.95),
#   .id = "id"
# ) %>%
#   select(id, year, vmt, vehicles, sourcehours, starts, emissions, type)  %>%
#   filter(type == "custom")

viz = bind_rows(
  output %>%
    select(id, year, emissions, value = vmt) %>%
    filter(id == 3) %>%
    mutate(type = "Decreasing VMT", label = scales::number(value, big.mark = ",")),

  output %>%
    select(id, year, emissions, value = vehicles) %>%
    filter(id == 2) %>%
    mutate(type = "Decreasing Vehicles", label = scales::number(value, big.mark = ",")),

  output %>%
    filter(id == 1) %>%
    select(id, year, emissions, value = vehicles) %>%
    mutate(type = "Increasing Vehicles", label = scales::number(value, big.mark = ","))
) %>%
  mutate(id = id %>% recode_factor(
    "1" = "Scenario 1<br><b>Increasing Vehicles</b>",
    "2" =  "Scenario 2<br><b>Decreasing Vehicles</b>",
    "3" = "Scenario 3<br><b>Decreasing VMT</b>"
  ))

stat = viz %>%
  group_by(id) %>%
  summarize(first = value[year == 2020],
            last = value[year == 2050],
            diff = first - last,
            label = scales::number(diff, style_positive = "plus", style_negative = "minus", decimal.mark = ","),
            year = 2035,
            ylabel = 0 + 0.15 *(max(emissions) - min(emissions))
            )

gg = ggplot() +
  geom_ribbon(
    data = viz,
    mapping = aes(x = year, ymin = 0, ymax = emissions),
    fill = "#648FFF", alpha = 0.5
  ) +
  geom_line(
    data = viz,
    mapping = aes(x = year, y = emissions),
    color = "white", size = 2.5) +
  geom_line(
    data = viz,
    mapping = aes(x = year, y = emissions),
    color = "#648FFF", size = 2) +
  geom_label(
    data = viz,
    mapping = aes(x = year, y = emissions, label = label),
    color = "white", fill = "#648FFF",
    label.size = 1, size = 5) +
  geom_label(
    data = viz,
    mapping = aes(x = year, y = emissions, label = label),
    color = "white", fill = "#648FFF",
    label.size = 0.5, size = 5) +

  shadowtext::geom_shadowtext(
    data = stat,
    mapping = aes(x = year,
                  y = ylabel,
                  label = paste0("\n", label, " t")),
    bg.color = "white",
    bg.r = 0.3,
    color = "#648FFF"
  ) +
  shadowtext::geom_shadowtext(
    data = stat,
    mapping = aes(x = year,
                  y = ylabel,
                  label = paste0(
                    "30 year change", "\n")),
    bg.color = "white",
    bg.r = 0.15,
    color = "darkgrey"
  ) +

  facet_wrap(~id, scales = "free_y") +
  labs(x = "Year", y = "Emissions (tons)",   title = "Projected CO2 Equivalent Emissions given Change in Activity") +
  theme_classic(base_size = 14) +
  theme(
    panel.border = element_rect(fill = NA, color = "#373737"),
    plot.title = element_text(hjust = 0.5),
    strip.text = ggtext::element_markdown(hjust = 0.5, size = 10, color = "white"),
    strip.background = element_rect(fill = "#648FFF"),
    panel.grid.major.y = element_line(color = "lightgrey", linewidth = 0.1)
  ) +
  ggplot2::scale_x_continuous(expand = expansion(c(0.20, 0.20))) +
  ggplot2::scale_y_continuous(expand = expansion(c(0.00,0.1)))

ggsave(plot = gg, filename = "diagnostics/figure_4.png", dpi = 500, width = 10, height = 6)

browseURL("diagnostics/figure_4.png")
#
# # Visualize
# g1 = ggplot(
#   data = output %>% filter(id == 1),
#   mapping = aes(x = year, y = emissions, label = vehicles)) +
#   geom_line(color = "#648FFF", size = 1) +
#   geom_label(color = "white", fill = "#648FFF", size = 7) +
#   labs(x = "Year (Increasing Vehicles)", y = "Emission (tons)", title = "") +
#   theme_classic() +
#   theme(panel.border = element_rect(fill = NA, color = "#373737")) +
#   ggplot2::scale_x_continuous(expand = expansion(c(0.25, 0.25)))
#
# g2 = ggplot(
#   data = output %>% filter(id == 2),
#   mapping = aes(x = year, y = emissions, label = vehicles)) +
#   geom_line(color = "#648FFF", size = 1) +
#   geom_label(color = "white", fill = "#648FFF", size = 7) +
#   labs(x = "Year (Decreasing Vehicles)", y = NULL, title = "Year vs. Carbon Emission") +
#   theme_classic() +
#   theme(panel.border = element_rect(fill = NA, color = "#373737")) +
#   ggplot2::scale_x_continuous(expand = expansion(c(0.25, 0.25)))
# g3 = ggplot(
#   data = output %>% filter(id == 3),
#   mapping = aes(x = year, y = emissions, label = vmt)) +
#   geom_line(color = "#648FFF", size = 1) +
#   geom_label(color = "white", fill = "#648FFF", size = 7) +
#   labs(x = "Year (Decreasing VMT)", y = NULL, title = "" ) +
#   theme_classic() +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   theme(panel.border = element_rect(fill = NA, color = "#373737")) +
#   ggplot2::scale_x_continuous(expand = expansion(c(0.25, 0.25)))


#install.packages("ggpubr")
#library(ggpubr)
#gg = ggarrange(plotlist = list(g1,g2,g3), ncol = 3, nrow = 1)
#gg
#ggsave(plot = gg, filename = "radviz.png", dpi = 500, width = 8, height = 4)

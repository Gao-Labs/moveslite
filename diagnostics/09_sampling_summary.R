
# County-pollutant-subsets sampled
read_rds("diagnostics/runs_sample_final.rds") %>% count()
# Census divisions sampled
divs$division %>% unique()

# Initially, we did this validation exercise for a huge number of counties and pollutants and models,
# but we have since limited our scope to contain the size and feasibility of the exercise.

# Here are the finalized samples and runs.

#
# Unique geoid sampled
read_rds("diagnostics/runs_sample_final.rds") %>%
  select(geoid) %>% distinct() %>% count()

# Unique counties sampled per pollutant-subset
read_rds("diagnostics/runs_sample_final.rds") %>%
  group_by(pollutant, set_id) %>%
  summarize(count = n())

# Unique Runs
runs = read_rds("diagnostics/runs_sample_final.rds") %>% select(run_id) %>% distinct() %>% with(run_id)

db = dbConnect(RSQLite::SQLite(), "diagnostics/diagnostics.sqlite")

# Viable Models actually generated
db %>%
  tbl("samples") %>%
  filter(run_id %in% !!runs) %>%
  count()

db %>%
  tbl("samples") %>%
  filter(run_id %in% !!runs) %>%
  filter(!is.na(adjr)) %>%
  count()


# all estimatable counties, out of ~3270
nall = 3110

# County coverage
n = db %>%
  tbl("samples") %>%
  filter(run_id %in% !!runs) %>%
  select(geoid) %>%
  distinct() %>%
  count() %>%
  collect()
n$n / nall

# County coverage by Pollutant
db %>%
  tbl("samples") %>%
  filter(run_id %in% !!runs) %>%
  select(pollutant, geoid) %>%
  distinct() %>%
  group_by(pollutant) %>%
  summarize(count = n() / !!nall)


# How many geoids was each model tested on?
db %>%
  tbl("samples") %>%
  filter(run_id %in% !!runs) %>%
  select(formula_id, geoid) %>%
  distinct() %>%
  group_by(formula_id) %>%
  summarize(count = n() / !!nall) %>%
  summarize(mu = mean(count),
            sigma = sd(count),
            min = min(count),
            max = max(count))


# Our ideal model had the following coverage
db %>%
  tbl("samples") %>%
  filter(run_id %in% !!runs) %>%
  filter(formula_id == 51) %>%
  select(geoid) %>%
  distinct() %>%
  count()


db %>%
  tbl("samples") %>%
  filter(run_id %in% !!runs) %>%
  filter(formula_id == 51) %>%
  group_by(type) %>%
  summarize(stat = median(adjr, na.rm = TRUE)) %>%
  collect() %>%
  summarize(min = min(stat),
            max = max(stat))



db %>%
  tbl("samples") %>%
  filter(run_id %in% !!runs) %>%
  filter(formula_id == 51) %>%
  group_by(pollutant) %>%
  summarize(stat = median(adjr, na.rm = TRUE)) %>%
  collect() %>%
  summarize(min = min(stat),
            max = max(stat))


db %>%
  tbl("samples") %>%
  filter(run_id %in% !!runs) %>%
  filter(formula_id == 51) %>%
  summarize(count = sum(adjr >= 0.90, na.rm = TRUE),
            total = n()) %>%
  collect() %>%
  mutate(percent = count / total)


db %>%
  tbl("samples") %>%
  filter(run_id %in% !!runs) %>%
  filter(formula_id == 51) %>%
  summarize(count = sum(adjr >= 0.95, na.rm = TRUE),
            total = n()) %>%
  collect() %>%
  mutate(percent = count / total)



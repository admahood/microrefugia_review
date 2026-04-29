# microrefugia review summary

library(tidyverse)
library(janitor)

screening <- readxl::read_xlsx('data/Microrefugia review studies.xlsx', skip = 1) |>
  janitor::clean_names()

glimpse(screening)

# included versus not
screening$included |> table()

# reasons for exclusion
screening |>
  filter(included == 'No') |>
  dplyr::group_by(reason_for_exclusion) |>
  dplyr::summarise(n=n()) |>
  ungroup() |>
  arrange(desc(n)) |>
  print(n=15)

# included by year
screening |>
  dplyr::filter(!is.na(reader)) |>
  tidyr::replace_na(list(included = 'Left Blank')) |>
  dplyr::group_by(included, year) |>
  dplyr::summarise(n = n()) |>
  dplyr::ungroup() |> 
  rbind(expand_grid(included = c('No', "Yes", 'Left Blank'), year = 2005:2026, n=0)) |> 
  group_by(included, year) |>
  summarise(n=sum(n)) |>
  ggplot(aes(x=year, y=n, fill = included)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_brewer(palette = 'Set2')
  
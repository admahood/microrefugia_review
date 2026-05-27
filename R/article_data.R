# article data
library(tidyverse)

d <- 
  readxl::read_xlsx('data/Microrefugia literature review table - data collection.xlsx', 
                    sheet = 1,
                    skip = 1) |>
  janitor::clean_names() |>
  slice(2:999) |>
  dplyr::select(-2)

glimpse(d)

# locations - could be a map (requires one person to get lat/longs for all 200+ studies)
d$study_location |> unique()

# yes or no questions

d$is_the_specified_refugia_defined |> table()

# habitats
d |>
  dplyr::select(habitat) |>
  tidyr::separate(habitat, into = c('w','x', 'y', 'z', 'a', 'b'), sep = '\\, ') |>
  tidyr::pivot_longer( c('w','x', 'y', 'z', 'a', 'b'))|> 
  na.omit() |>
  dplyr::mutate(value = str_remove_all(value, "[^A-Za-z0-9]")) |>
  group_by(value) |>
  summarise(n = n()) |>
  ungroup() |>
  arrange(desc(n)) |>
  dplyr::mutate(value = ifelse(n == 1, 'other', value)) |>
  ggplot(aes(x=n, y=fct_reorder(value, n))) +
  geom_bar(stat = 'identity') +
  ggtitle("Habitat")

# species groups
d |>
  dplyr::select(species_group) |>
  tidyr::separate(species_group, into = c('w','x', 'y', 'z', 'a', 'b'), sep = '\\, ') |>
  tidyr::pivot_longer( c('w','x', 'y', 'z', 'a', 'b'))|> 
  na.omit()|>
  group_by(value) |>
  summarise(n = n()) |>
  ungroup() |>
  dplyr::mutate(value = ifelse(n == 1, 'other', value)) |>
  ggplot(aes(x=n, y=fct_reorder(value,n))) +
  geom_bar(stat= 'identity') +
  ggtitle("Species Group")

# framework 
d |>
  dplyr::select(framework) |>
  tidyr::separate(framework, into = c('w','x', 'y', 'z', 'a', 'b'), sep = '\\, ') |>
  tidyr::pivot_longer( c('w','x', 'y', 'z', 'a', 'b'))|> 
  na.omit()|>
  group_by(value) |>
  summarise(n = n()) |>
  ungroup() |>
  dplyr::mutate(value = ifelse(n == 1, 'other', value)) |>
  ggplot(aes(x=n, y=fct_reorder(value,n))) +
  geom_bar(stat= 'identity') +
  ggtitle("Framework")

# refugia term
d |>
  dplyr::select(refugia_term) |>
  tidyr::separate(refugia_term, into = c('w','x', 'y', 'z', 'a', 'b'), sep = '\\, ') |>
  tidyr::pivot_longer( c('w','x', 'y', 'z', 'a', 'b'))|> 
  na.omit()|>
  mutate(value = str_to_lower(value)) |>
  group_by(value) |>
  summarise(n = n()) |>
  ungroup() |>
  dplyr::mutate(value = ifelse(n == 1, 'other', value)) |>
  ggplot(aes(x=n, y=fct_reorder(value,n))) +
  geom_bar(stat= 'identity') +
  ggtitle("Refugia Term (Some Studies cover multiple categories)")

# reference for definition - this needs to be cleaned up (i.e. standardized) to make it easier to summarize
d$refugia_defintion_reference |> table() |> sort()

# position in relation to main range
d |> 
  group_by(y= position_in_relation_to_main_range) |>
  summarise(n = n()) |>
  ungroup() |>
  mutate(y = ifelse(y == 'NA', "NA (entered as such)", y)) |>
  tidyr::replace_na(list(y = "NA (because it was left blank)")) |>
  ggplot(aes(x=n, y=fct_reorder(y,n))) +
  geom_bar(stat = 'identity') +
  ggtitle('Position in relation to main range')

# sampling structure
d |>
  dplyr::select(sampling_structure) |>
  tidyr::separate(sampling_structure, into = c('w','x', 'y', 'z', 'a', 'b'), sep = '\\, ') |>
  tidyr::pivot_longer( c('w','x', 'y', 'z', 'a', 'b'))|> 
  na.omit()|>
  # mutate(value = str_to_lower(value),
  #        value = ifelse(value == 'na', 'entered as NA', value)) |>
  group_by(value) |>
  summarise(n = n()) |>
  ungroup() |>
  dplyr::mutate(value = ifelse(n == 1, 'other', value)) |>
  ggplot(aes(x=n, y=fct_reorder(value,n))) +
  geom_bar(stat= 'identity') +
  ggtitle("Sampling Structure (Some Studies cover multiple categories)")

# climate driver
d |>
  dplyr::select(value = what_kind_of_climate_driver_does_the_study_want_to_capture_ie_which_aspect_is_the_refugia_buffering) |>
  mutate(value = str_to_lower(value),
         value = ifelse(value == 'na', 'entered as NA', value)) |>
  tidyr::replace_na(list(value = "NA (because it was left blank)")) |>
  tidyr::separate(value, into = c('w','x', 'y', 'z', 'a', 'b'), sep = '\\, ') |>
  tidyr::pivot_longer( c('w','x', 'y', 'z', 'a', 'b'))|> 
  na.omit()|>

  group_by(value) |>
  summarise(n = n()) |>
  ungroup() |>
  dplyr::mutate(value = ifelse(n == 1, 'other', value)) |>
  ggplot(aes(x=n, y=fct_reorder(value,n))) +
  geom_bar(stat= 'identity') +
  ggtitle("Climate Driver (Some Studies cover multiple categories)")

# exposure_considered
d |>
  dplyr::select(value = exposure_considered)  |> 
  tidyr::separate(value, into = c('w','x', 'y', 'z', 'a', 'b'), sep = '\\, ') |>
  tidyr::pivot_longer( c('w','x', 'y', 'z', 'a', 'b'))|> 
  na.omit()|>
  group_by(value) |>
  summarise(n = n()) |>
  ungroup() |>
  dplyr::mutate(value = ifelse(n == 1, 'other', value)) |>
  ggplot(aes(x=n, y=fct_reorder(value,n))) +
  geom_bar(stat= 'identity') +
  ggtitle("Exposure Considered (Some Studies cover multiple categories)")

# species_sensitivity_ie_what_is_the_vulnerable_part_of_the_species

d |>
  dplyr::select(value = species_sensitivity_ie_what_is_the_vulnerable_part_of_the_species)  |> 
  mutate(value = str_to_lower(value),
         value = ifelse(value == 'na', 'entered as NA', value)) |>
  tidyr::replace_na(list(value = "NA (because it was left blank)")) |>
  tidyr::separate(value, into = c('w','x', 'y', 'z', 'a', 'b'), sep = '\\, ') |>
  tidyr::pivot_longer( c('w','x', 'y', 'z', 'a', 'b'))|> 
  na.omit()|>
  group_by(value) |>
  summarise(n = n()) |>
  ungroup() |>
  dplyr::mutate(value = ifelse(n == 1, 'other', value)) |>
  ggplot(aes(x=n, y=fct_reorder(value,n))) +
  geom_bar(stat= 'identity') +
  ggtitle("Species Sensitivity (Some Studies cover multiple categories)")


d |>
  dplyr::select(value = is_species_sensitivity_used_to_identify_refugia) |> 
  mutate(value = str_to_lower(value),
         value = ifelse(value == 'na', 'entered as NA', value)) |>
  tidyr::replace_na(list(value = "NA (because it was left blank)"))  |> 
  tidyr::separate(value, into = c('w','x', 'y', 'z', 'a', 'b'), sep = '\\, ') |>
  tidyr::pivot_longer( c('w','x', 'y', 'z', 'a', 'b'))|> 
  na.omit() |>
  group_by(value) |>
  summarise(n = n()) |>
  ungroup() |>
  dplyr::mutate(value = ifelse(n == 1, 'other', value)) |>
  ggplot(aes(x=n, y=fct_reorder(value,n))) +
  geom_bar(stat= 'identity') +
  ggtitle("is Species Sensitivity used to identigy refugia")

# d$species_adaptability_genetic_adaptability_and_dispersal |> table()
d |>
  dplyr::select(value = species_adaptability_genetic_adaptability_and_dispersal) |> 
  mutate(value = str_to_lower(value),
         value = ifelse(value == 'na', 'entered as NA', value)) |>
  tidyr::replace_na(list(value = "NA (because it was left blank)")) |>
  tidyr::separate(value, into = c('w','x', 'y', 'z', 'a', 'b'), sep = '\\, ') |>
  tidyr::pivot_longer( c('w','x', 'y', 'z', 'a', 'b'))|> 
  na.omit()|>
  group_by(value) |>
  summarise(n = n()) |>
  ungroup() |>
  dplyr::mutate(value = ifelse(n == 1, 'other', value)) |>
  ggplot(aes(x=n, y=fct_reorder(value,n))) +
  geom_bar(stat= 'identity') +
  ggtitle("species_adaptability_genetic_adaptability_and_dispersal")

# d$refugia_structure_extrinsic_adaptability_factors |> table()
d |>
  dplyr::select(value = refugia_structure_extrinsic_adaptability_factors) |> 
  mutate(value = str_to_lower(value),
         value = ifelse(value == 'na', 'entered as NA', value)) |>
  tidyr::replace_na(list(value = "NA (because it was left blank)")) |>
  tidyr::separate(value, into = c('w','x', 'y', 'z', 'a', 'b'), sep = '\\, ') |>
  tidyr::pivot_longer( c('w','x', 'y', 'z', 'a', 'b'))|> 
  na.omit()|>
  group_by(value) |>
  summarise(n = n()) |>
  ungroup() |>
  dplyr::mutate(value = ifelse(n == 1, 'other', value)) |>
  ggplot(aes(x=n, y=fct_reorder(value,n))) +
  geom_bar(stat= 'identity') +
  ggtitle("refugia_structure_extrinsic_adaptability_factors")

# d$species_response_to_changing_climate |> table()
d |>
  dplyr::select(value = species_response_to_changing_climate) |>
  mutate(value = str_to_lower(value),
         value = ifelse(value == 'na', 'entered as NA', value)) |>
  tidyr::replace_na(list(value = "NA (because it was left blank)")) |>
  tidyr::separate(value, into = c('w','x', 'y', 'z', 'a', 'b'), sep = '\\, ') |>
  tidyr::pivot_longer( c('w','x', 'y', 'z', 'a', 'b'))|> 
  na.omit()|>
  group_by(value) |>
  summarise(n = n()) |>
  ungroup() |>
  dplyr::mutate(value = ifelse(n == 1, 'other', value)) |>
  ggplot(aes(x=n, y=fct_reorder(value,n))) +
  geom_bar(stat= 'identity') +
  ggtitle("species_response_to_changing_climate")

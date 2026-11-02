# make the really clean datasheet

# article data
library(tidyverse)

# data input ===================================================================
# main sheet
d <- 
  readxl::read_xlsx('data/Microrefugia literature review table - data collection.xlsx', 
                    sheet = 1,
                    skip = 1) |>
  janitor::clean_names() |>
  slice(2:999) |>
  dplyr::select(-2)

# article year
art_year <- readxl::read_xlsx('data/Microrefugia review studies.xlsx', 
                              sheet = 1,
                              skip = 1)  |>
  janitor::clean_names() |>
  dplyr::select(article_id, year) |>
  unique()

# definition recoding
d_def <- 
  readxl::read_xlsx('data/Microrefugia literature review table - data collection.xlsx', 
                    sheet = 'definition_recoding',
                    skip = 1) |>
  janitor::clean_names() |>
  slice(2:999) |>
  dplyr::select(1, main_concept = 3, is_term_defined_new = 4, 
                definition_new = 5, 6:9, method_qualitative_quantitative = 10) |>
  dplyr::mutate(main_concept = str_replace_all(main_concept, ' ', '_'))

d_def_strict <-
  readxl::read_xlsx('data/Microrefugia literature review table - data collection.xlsx', 
                    sheet = 'definition_2',
                    skip = 0)|>
  janitor::clean_names() |>
  slice(2:999) |>
  dplyr::filter(article_id != 'WOS:000388868700010')
glimpse(d_def_strict)

# d_resclim
d_clim <- 
  readxl::read_xlsx('data/Microrefugia literature review table - data collection.xlsx', 
                    sheet = 'resolution_modelling+climate',
                    skip = 1) |>
  janitor::clean_names() |>
  slice(2:999) |> 
  dplyr::select(article_id,
                climate_data_source_check_up,
                climate_data_resolution_new, 
                spatial_resolution_modelling_rs_new)

d_species <- 
  readxl::read_xlsx('data/Microrefugia literature review table - data collection.xlsx', 
                    sheet = 'species',
                    skip = 0) |>
  janitor::clean_names() |>
  slice(2:999) |>
  dplyr::mutate(species_group_new_merged = 
                  ifelse(is.na(species_group_new_merged), species_group_new, species_group_new_merged),
                species_group_new_multiple = 
                  ifelse(is.na(species_group_new_multiple), species_group_new, species_group_new_multiple)) |>
  dplyr::select(-species_group)

d_map <- 
  readxl::read_xlsx('data/Microrefugia literature review table - data collection.xlsx', 
                    sheet = 'map_habitat',
                    skip = 1) |>
  janitor::clean_names() |>
  slice(2:999) |>
  dplyr::select(article_id, continent) |>
  unique()




d_merged <- cbind(d,
      d_def |> dplyr::select(-article_id),
      d_clim |> dplyr::select(-article_id),
      d_species |> dplyr::select(-article_id, -x6)) |>
  # dplyr::slice(115:412) |>
  dplyr::filter(is.na(exclude)) |>
  left_join(art_year) |> 
  left_join(d_map)

glimpse(d_merged)

readr::write_csv(d_merged, "data/cleaned_and_merged_data___preliminary.csv")

readr::write_csv(d_def_strict, 'data/definition_strict.csv')



# post-hoc exclusions
# post_hoc_exclusions <-
#   d_merged |>
#   dplyr::filter(!is.na(exclude)) |>
#   pull(article_id)

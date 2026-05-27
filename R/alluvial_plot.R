library(tidyverse)
library(ggalluvial)
library(janitor)


d <- 
  readxl::read_xlsx('data/Microrefugia literature review table - data collection.xlsx', 
                    sheet = 1,
                    skip = 1) |>
  janitor::clean_names() |>
  slice(2:999) |>
  dplyr::select(-2)

is_defined <- d |> 
  dplyr::select(is_defined = is_the_specified_refugia_defined, article_id_1)

term <- d |>
  dplyr::select(refugia_term, article_id_1) |>
  tidyr::separate(refugia_term, into = c('w','x', 'y', 'z', 'a', 'b'), sep = '\\, ') |>
  tidyr::pivot_longer( c('w','x', 'y', 'z', 'a', 'b'))|> 
  na.omit()|>
  mutate(value = str_to_lower(value)) |>
  dplyr::select(-name, refugia_term=value, article_id_1) |>
  dplyr::mutate(refugia_term = case_when(
    refugia_term == 'microrefugia' ~ 'micro\n(climate)\nrefugia',
    refugia_term == 'climate refugia' ~ 'climate\n(change)\nrefugia',
    refugia_term == 'climate change refugia' ~ 'climate\n(change)\nrefugia',
    refugia_term == 'microclimate refugia' ~ 'micro\n(climate)\nrefugia',
    refugia_term == 'microclimatic refugia' ~ 'micro\n(climate)\nrefugia',
    refugia_term == 'thermal refugia' ~ 'thermal refugia',
    refugia_term == 'thermal refuge' ~ 'thermal refugia',
    refugia_term == 'thermal microrefugia' ~ 'thermal refugia',
    TRUE ~ 'Other'
  ))
  
climate_driver <- d |>
  dplyr::select(value = what_kind_of_climate_driver_does_the_study_want_to_capture_ie_which_aspect_is_the_refugia_buffering,
                article_id_1) |>
  mutate(value = str_to_lower(value),
         value = ifelse(value == 'na', 'entered as NA', value)) |>
  tidyr::replace_na(list(value = "NA (because it was left blank)")) |>
  tidyr::separate(value, into = c('w','x', 'y', 'z', 'a', 'b'), sep = '\\, ') |>
  tidyr::pivot_longer( c('w','x', 'y', 'z', 'a', 'b'))|> 
  na.omit()|>
  dplyr::select(-name, article_id_1, climate_driver = value)|>
  dplyr::mutate(climate_driver = case_when(
    climate_driver == 'climate change (gradual)' ~ 'climate\nchange\n(gradual)',
    climate_driver == 'climate extreme events' ~ 'extreme\nclimate\nevents',
    TRUE~'other'
    
  ))

ssu <- d |>
  dplyr::select(value = is_species_sensitivity_used_to_identify_refugia,
                article_id_1) |> 
  mutate(value = str_to_lower(value),
         value = ifelse(value == 'na', 'entered as NA', value)) |>
  tidyr::replace_na(list(value = "NA (because it was left blank)"))  |> 
  tidyr::separate(value, into = c('w','x', 'y', 'z', 'a', 'b'), sep = '\\, ') |>
  tidyr::pivot_longer( c('w','x', 'y', 'z', 'a', 'b'))|> 
  na.omit() |>
  dplyr::select(-name, article_id_1, ssu = value) |>
  dplyr::mutate(ssu = ifelse(str_detect(ssu, 'NA'), "NA", ssu),
                ssu = ifelse(ssu %in% c('not relevant', 'i dont know', 
                                        "don't know", 'NA'),
                             "irrelevant\ndon't know\nNA", ssu))

df <- is_defined |>
  left_join(term) |>
  left_join(climate_driver) |>
  left_join(ssu) |>
  group_by(climate_driver, ssu, refugia_term, is_defined) |>
  dplyr::summarise(n=n()) |>
  ungroup()

df |>
ggplot(aes(y=n, axis2 = refugia_term, axis3 = climate_driver, axis4 = ssu,
           axis1 = is_defined)) +
  geom_alluvium(aes(fill = is_defined)) +
  geom_stratum() +
  scale_x_discrete(limits = c("Is Refugia\nDefined",
                              "Refugia\nTerm", 
                              "Climate\nDriver",
                              "Is Species\nSensitivity Used"),
                   expand = c(.2, .05)) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  theme_minimal() +
  ggtitle("Definitions and key concepts")
  
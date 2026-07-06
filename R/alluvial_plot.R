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

drd <- 
  readxl::read_xlsx('data/Microrefugia literature review table - data collection.xlsx', 
                    sheet = 'definition_recoding',
                    skip = 1) |>
  janitor::clean_names() |>
  slice(2:999) |>
  dplyr::select(-2) |>
  unique()

framework <-
  d |>
  dplyr::select(article_id_1, value = framework) |>
  tidyr::separate(value, into = c('w','x', 'y', 'z', 'a', 'b'), sep = '\\, ') |>
  tidyr::pivot_longer( c('w','x', 'y', 'z', 'a', 'b'))|> 
  na.omit()|>
  dplyr::select(-name, article_id_1, framework = value) |>
  unique()

space <- drd |>
  dplyr::select(article_id_1 = article_id, space_indef)
time <- drd |>
  dplyr::select(article_id_1 = article_id, time_indef)
climate <- drd |>
  dplyr::select(article_id_1 = article_id, climate_indef)
biological <- drd |>
  dplyr::select(article_id_1 = article_id, biological_indef)

term <- drd |>
  dplyr::select(article_id_1 = article_id, 
                term = what_is_the_single_main_concept_or_term)

is_defined <- d |> 
  dplyr::select(is_defined = is_the_specified_refugia_defined, article_id_1)

# term <- d |>
#   dplyr::select(refugia_term, article_id_1) |>
#   tidyr::separate(refugia_term, into = c('w','x', 'y', 'z', 'a', 'b'), sep = '\\, ') |>
#   tidyr::pivot_longer( c('w','x', 'y', 'z', 'a', 'b'))|> 
#   na.omit()|>
#   mutate(value = str_to_lower(value)) |>
#   dplyr::select(-name, refugia_term=value, article_id_1) |>
#   dplyr::mutate(refugia_term = case_when(
#     refugia_term == 'microrefugia' ~ 'micro\n(climate)\nrefugia',
#     refugia_term == 'climate refugia' ~ 'climate\n(change)\nrefugia',
#     refugia_term == 'climate change refugia' ~ 'climate\n(change)\nrefugia',
#     refugia_term == 'microclimate refugia' ~ 'micro\n(climate)\nrefugia',
#     refugia_term == 'microclimatic refugia' ~ 'micro\n(climate)\nrefugia',
#     refugia_term == 'thermal refugia' ~ 'thermal refugia',
#     refugia_term == 'thermal refuge' ~ 'thermal refugia',
#     refugia_term == 'thermal microrefugia' ~ 'thermal refugia',
#     TRUE ~ 'Other'
#   ))
  
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


# 
# ssu <- d |>
#   dplyr::select(value = is_species_sensitivity_used_to_identify_refugia,
#                 article_id_1) |> 
#   mutate(value = str_to_lower(value),
#          value = ifelse(value == 'na', 'entered as NA', value)) |>
#   tidyr::replace_na(list(value = "NA (because it was left blank)"))  |> 
#   tidyr::separate(value, into = c('w','x', 'y', 'z', 'a', 'b'), sep = '\\, ') |>
#   tidyr::pivot_longer( c('w','x', 'y', 'z', 'a', 'b'))|> 
#   na.omit() |>
#   dplyr::select(-name, article_id_1, ssu = value) |>
#   dplyr::mutate(ssu = ifelse(str_detect(ssu, 'NA'), "NA", ssu),
#                 ssu = ifelse(ssu %in% c('not relevant', 'i dont know', 
#                                         "don't know", 'NA'),
#                              "irrelevant\ndon't know\nNA", ssu))

df <- term |>
  left_join(climate_driver) |>
  left_join(space) |>
  left_join(time) |>
  left_join(climate) |>
  left_join(biological) |>
  left_join(framework) |>
  group_by(climate_driver, term, space_indef, framework,
           time_indef, climate_indef, biological_indef) |>
  dplyr::summarise(n=n()) |>
  ungroup() |>
  dplyr::filter(term != 'not_applicable') |>
  mutate(space_indef = str_replace_all(space_indef, 'unclear', 'no') |>
           forcats::fct_relevel('yes', 'no', 'undefined'),
         time_indef = forcats::fct_relevel(time_indef, 'yes', 'no', 'undefined'),
         climate_indef = forcats::fct_relevel(climate_indef, 'yes', 'no', 'undefined'),
         biological_indef = forcats::fct_relevel(biological_indef, 'yes', 'no', 'undefined'),
         term = str_replace_all(term, '_', ' ') |> 
           str_remove_all(' ') |>
           str_remove_all('refugia')#,
         #framework = str_replace_all(framework, ' ', '\n')
         ) |>
  na.omit()
  

df |>
ggplot(aes(y=n,
           axis1 = term,
           axis2 = space_indef, 
           axis3 = time_indef, 
           axis4 = climate_indef, 
           axis5 = biological_indef,
           axis6 = climate_driver)) +
  geom_alluvium(aes(fill = framework)) +
  geom_stratum() +
  scale_x_discrete(limits = c('Concept',
                              "Space",
                              "Time",
                              "Climate",
                              "Biological",
                              'Climate\nDriver'),
                   expand = c(.2, .05)) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  theme_minimal() +
  ggtitle("Definitions and key concepts") +
  theme(legend.position = 'bottom')


ggsave('out/alluvial_definition_july.png', width = 16, height = 10, bg='white')
  
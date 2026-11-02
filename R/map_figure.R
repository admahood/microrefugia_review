# map and habitat
library(tidyverse);library(sf); library(geodata)


d_clean <- readr::read_csv('data/cleaned_and_merged_data.csv')

d_map <- readxl::read_xlsx('data/Microrefugia literature review table - data collection.xlsx', 
                           sheet = 'map_habitat', 
                           skip = 1) |>
  slice(2:999) |>
  janitor::clean_names() |>
  right_join(d_clean |> dplyr::select(article_id, framework, main_concept)) |>
  tidyr::separate(latlong, into = c('lat', 'lon'), sep = '\\, ') |>
  dplyr::filter(!is.na(lon), #!str_detect(framework, 'Conceptual'), 
                main_concept =='climate_refugia') |> 
  st_as_sf(crs = 4326, coords = c('lon', 'lat')) |>
  dplyr::mutate(habitat = case_when(str_detect(habitat, '\\,') ~ 'spans multiple habitats', 
                                    str_detect(habitat, 'forest') ~ 'forest',
                                    str_detect(habitat, 'desert') ~ 'drylands',
                                    str_detect(habitat, 'arid') ~ 'drylands',
                                    str_detect(habitat, 'coastal') ~ 'coastal ecosystems',
                                    habitat == 'woodland' ~ 'forest',
                                    habitat == 'grassy slopes' ~ 'grassland',
                                    habitat == 'riparian' ~ 'riparian, wetlansds, lakes',
                                    habitat == 'wetlands and lakes' ~ 'riparian, wetlansds, lakes',
                                    is.na(habitat) ~ 'spans multiple habitats',
                                    habitat == 'sagebrush-steppe' ~ 'drylands',
                                    habitat %in% c('alpine ecosystems', 
                                                   'mountain range',
                                                   'rock outcrop') ~ 'montane, alpine',
                                    habitat == 'NA' ~ 'spans multiple habitats',
                                    TRUE ~ habitat
                                    ) |> str_to_title()) 
  

world <- geodata::world() |> st_as_sf()

ggplot(world) +
  geom_sf(fill = 'grey97', linewidth = 0.1, color = 'grey60') +
  geom_sf(data = d_map, aes(color = habitat), alpha = 0.75) +
  coord_sf(crs = 54030, ylim=c(-6000000, 8500000)) +
  ggtitle('Studies with specific locations (Climate Refugia only)', 'Global or continental studies not shown') +
  colorspace::scale_color_discrete_qualitative() +
  theme_bw() +
  theme(legend.position = c(0,0),
        legend.background = element_rect(fill = 'white', color = 'black'),
        legend.justification = c(0,0))
ggsave(filename = 'out/study_locations_habitats_climate_refugia.png', width = 13, height = 7, bg = 'white')

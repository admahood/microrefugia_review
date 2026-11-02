# multipanel definition figure

# map and habitat
library(tidyverse);library(sf); library(geodata); library(ggpubr)

d_clean <- readr::read_csv('data/cleaned_and_merged_data___preliminary.csv')

d_map <- readxl::read_xlsx('data/Microrefugia literature review table - data collection.xlsx', 
                           sheet = 'map_habitat', 
                           skip = 1) |>
  slice(2:999) |>
  janitor::clean_names() |>
  right_join(d_clean |> dplyr::select(article_id, framework, main_concept)) |>
  tidyr::separate(latlong, into = c('lat', 'lon'), sep = '\\, ') |>
  dplyr::filter(!is.na(lon), #!str_detect(framework, 'Conceptual'), 
                main_concept =='microrefugia') |> 
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

# map ==========================================================================
p_map <- ggplot(world) +
  geom_sf(fill = 'grey90', linewidth = 0, color = 'grey90') +
  geom_sf(data = d_map, aes(color = habitat), alpha = 0.75) +
  coord_sf(crs = 54030, ylim=c(-6000000, 8500000)) +
  ggtitle('Microrefugia Studies with Specified Locations', 'Global or continental studies not shown') +
  colorspace::scale_color_discrete_qualitative() +
  theme_bw() +
  theme(legend.position = c(0,0),
        legend.background = element_rect(fill = 'white', color = 'black'),
        legend.justification = c(0,0))

d_cont <- d_clean |>
  filter(main_concept == 'microrefugia',
         !is.na(continent),
         !str_detect(framework, 'Concept')
         ) |>
  dplyr::mutate(continent = str_replace_all(continent, '_', ' ') |>
                  str_replace_all(' ', '\n') |>
                  str_to_title() |>
                  fct_relevel('Europe', "North\nAmerica", 'Australia', 'Asia', 'South\nAmerica', 'Oceania')) 

# barplot for continents =======================================================
p_cont <- d_cont |>
  ggplot(aes(x=continent)) +
  geom_bar(stat= 'count') +
  ylab("N Studies") +
  xlab("Continent") +
  theme_classic()

# habitats =====================================================================
p_habitat <- d_clean |>
  dplyr::filter(!str_detect(framework, 'Concept'), main_concept == 'microrefugia') |>
  dplyr::select(main_concept, habitat) |>
  tidyr::separate(habitat, into = c('w','x', 'y', 'z', 'a', 'b'), sep = '\\, ') |>
  tidyr::pivot_longer(cols = c('w','x', 'y', 'z', 'a', 'b')) |> 
  na.omit() |>
  dplyr::mutate(value = str_remove_all(value, "[^A-Za-z0-9]")) |>
  filter(value != 'river') |>
  dplyr::mutate(value = case_when(value %in% c('stream', 'wetlandsandlakes', 
                                               'lake', 'swamp', 'saltlakes',
                                               'floodplains', 'riparian',
                                               'lakeshorelines') ~
                                    'Rivers, Lakes, Wetlands,\nFloodplains & Riparian',
                                  value %in% c('grassland', 'savannah', 'pasture',
                                               'agrosteppe', 'heath',
                                               'grassyslopes', 'shrubland') ~
                                    'Grasslands,\nSavannas & Shrublands',
                                  value %in% c('desert', 'semiaridregions', 
                                               'sagebrushsteppe', "aridregions") ~
                                    'Drylands',
                                  value %in% c('mountainrange', 'alpinevegetation',
                                               'alpineboulderfield', 'tundra', 
                                               'tepui',
                                               'rockglacier', 'montanemeadows',
                                               'alpine', 'Alpine', 'alpineecosystems') ~
                                    'Alpine, Montane',
                                  value %in% c('rockoutcrops', 'Rupicolous') ~
                                    'Rock Outcrops etc',
                                  value %in% c('coastal', 'tidal', 'coastalestuarinesystem') ~
                                    'Coastal Ecosystems',
                                  value %in% c('forest', 'taiga', 'rainforest', 
                                               'mediterraneanwoodland', 'coastalforest',
                                               'subtropicalforest', 'monodominantstand',
                                               'woodland', 'subalpinedryforest',
                                               'mountainforests') ~ 'Forests',
                                  str_detect(value, 'karst') ~ "Karst (Sinkholes)",
                                  value %in% c('spansmultiplehabitats', 'valleys',
                                               'Groundwaterdependentecosystems') ~
                                    'Multiple Habitats',
                                  TRUE ~ str_to_title(value)),
                main_concept = str_replace_all(main_concept, "_", ' ') |>
                  str_to_title()) |>
  group_by(value) |>
  mutate(total_by_habitat = n()) |>
  ungroup() |>
  group_by(main_concept, value, total_by_habitat) |>
  summarise(n = n()) |>
  ungroup() |>
  ggplot(aes(x=n, y=fct_reorder(value, total_by_habitat))) +
  geom_bar(stat = 'identity', color = 'black') +
  xlab("N Studies") +
  ggtitle("Habitat (if given)") +
  scale_fill_brewer(palette = "Dark2", name = "Main\nRefugia\nConcept")+
  theme_classic() +
  theme(axis.title.y = element_blank(),
        legend.background = element_rect(),
        legend.position = c(0.65,0.25),
        legend.justfication = c(1,0))

# definition ===================================================================
p_def <- d_clean |>
  dplyr::select(main_concept, refugia_defintion_reference) |>
  dplyr::filter(main_concept != 'not_applicable') |>
  tidyr::separate(refugia_defintion_reference, into=letters, sep = '\\;') |>
  tidyr::pivot_longer(cols = letters) |>
  na.omit() |> 
  mutate(value = str_remove_all(value, "^ ") |> 
           str_remove_all('\\(') |>
           str_remove_all('\\)') |>
           str_remove_all(',') |>
           str_remove_all('\n') |>
           str_remove_all('\\.') |>
           str_remove_all('et al') |>
           str_replace_all('  ', ' ') |>
           str_replace_all('  ', ' ') |>
           str_replace_all('  ', ' ') |>
           str_replace_all('  ', ' ') |>
           str_replace_all('and', '&') |>
           str_replace_all('\\+', '&') |>
           str_remove_all("^ ") |>
           str_replace_all(' & al ', ' ')) |>
  dplyr::select(-name) |>
  dplyr::group_by(value) |>
  dplyr::mutate(total_n = n()) |>
  ungroup() |>
  dplyr::mutate(value = ifelse(total_n == 1, 'Other', value))|>
  dplyr::group_by(value) |>
  dplyr::mutate(total_n = n()) |>
  ungroup() |>
  dplyr::group_by(main_concept, total_n, value) |>
  dplyr::summarise(n=n()) |>
  ungroup() |>
  dplyr::mutate(total_n = ifelse(value == 'Other', 1, total_n)) |>
  ggplot(aes(x=n, y=fct_reorder(value, total_n), fill = main_concept)) +
  geom_bar(stat = 'identity', color = 'black') +
  xlab("N Studies") +
  ggtitle("References Used") +
  scale_fill_brewer(palette = "Dark2", name = "Main\nRefugia\nConcept")+
  theme_classic() +
  theme(axis.title.y = element_blank(),
        legend.background = element_rect(fill = NA),
        legend.position = c(0.8, 0.2),
        legend.justfication = c(1,0))

# venn =========================================================================
source('R/b_venn.R')

# all together =================================================================

bottom <- ggpubr::ggarrange(p_habitat, p_venn_s, p_def, nrow = 1, labels = c('c', 'd', 'e'))
top <- ggpubr::ggarrange(p_map, p_cont, nrow = 1, widths = c(4,1.5), labels = 'auto')
ggpubr::ggarrange(top, bottom, nrow = 2, ncol = 1)
ggsave('out/mutipanel_figure_def_map_habitat.png', bg='white', width = 15, height = 10)


# extra definition stuff =======================================================

d_def_strict |>
  dplyr::select(elements_met_strict_0_4, elements_met_inclpartial_0_4, full_microrefugium_definition) |>
  tidyr::pivot_longer(cols = c(elements_met_strict_0_4, 
                               elements_met_inclpartial_0_4, 
                               full_microrefugium_definition)) |>
  dplyr::mutate(value = str_remove_all(value, '.0'),
                name = str_remove_all(name, '_0_4') |>
                  str_replace_all('_', " ") |>
                  str_replace_all('inclpartial', '(Including Partial)') |>
                  str_to_title()) |>
  ggplot(aes(x=value)) +
  geom_bar(stat = 'count') +
  facet_wrap(~name, scales = 'free') +
  ggtitle('') +
  theme_bw() +
  theme(axis.title.x = element_blank())

ggsave('out/strict_definition_bars.png', width = 8, height = 4, bg = 'white')




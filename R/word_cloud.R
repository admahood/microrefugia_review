# word cloud for definition
# csc setup
.libPaths(c("/users/mahoodal/ondemand/r_pkgs", .libPaths()))
libpath <- .libPaths()[1]

# libs 
# install.packages('ggwordcloud')
library(tidyverse); library(ggwordcloud)

d <- 
  readxl::read_xlsx('data/Microrefugia literature review table - data collection.xlsx', 
                    sheet = 1,
                    skip = 1) |>
  janitor::clean_names() |>
  dplyr::slice(2:999) |>
  dplyr::select(-2) |>
  dplyr::select(refugia_definition, article_id = article_id_1) |>
  na.omit() |>
  unique() |>
  tidyr::separate(refugia_definition, into = c(letters, LETTERS), sep = " ", extra = 'merge') |>
  tidyr::pivot_longer(-article_id) |>
  na.omit() |>
  dplyr::select(-name) |>
  dplyr::mutate(value = str_remove_all(value, "[^A-Za-z0-9]") |>
                  str_remove_all('[0-9]') |> trimws() |>
                  str_to_lower())|>
  dplyr::filter(!value %in% c('that', 'with', 'where', 'of', 'to', 'in', 'by',
                              'from', 'are', 'and', 'a', 'as', 'for', 'na', 'et',
                              'al', 'or', 'through', 'but', 'their', 'which',
                              'within', 'the', 'can', 'th', '', 'be', 'have',
                              'is', 'than', 'at', 'ie', 'these', 'they', 'them',
                              'we', 'will', 'an', 'has', 'more', 'been', 'such',
                              'under', 'often', 'thus')) |>
  dplyr::mutate(value = case_when(value == 'climatic' ~ 'climate',
                                  value == 'buffered' ~ 'buffer',
                                  value == 'locally' ~ 'local',
                                  TRUE ~ value));d
  
fig <- d |>
  dplyr::group_by(value) |>
  dplyr::summarise(n = n()) |>
  ungroup() |>
  # mutate(angle = 45 * sample(-2:2, n(), replace = TRUE, 
  #                            prob = c(1, 1, 4, 1, 1))) |>
  mutate(angle = rnorm(n=n(), mean = 0, sd = 40)) |>
  filter(n>4) |>
  # arrange(desc(n)) |> print(n=222)
  ggplot(aes(label = value, size = n, angle = angle)) +
    geom_text_wordcloud_area(shape= 'square') +
  scale_size_area(max_size = 50, trans = power_trans(1/.7)) +  
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_minimal()

ggsave('out/wordcloud.png', width = 10, 
       height = 7, bg = 'white')

# venn diagram
.libPaths(c("/users/mahoodal/ondemand/r_pkgs", .libPaths()))
libpath <- .libPaths()[1]

remotes::install_github('gaospecial/ggVennDiagram')

library(tidyverse)
library(ggVennDiagram)
process_region_data(Venn(y))library(janitor)


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

x <- list(
  space = drd |>
    dplyr::filter(space_indef == 'yes') |>
    dplyr::pull(article_id),
  time =drd |>
    dplyr::filter(time_indef == 'yes') |>
    dplyr::pull(article_id),
  climate= drd |>
    dplyr::filter(climate_indef == 'yes') |>
    dplyr::pull(article_id),
  biological =drd |>
    dplyr::filter(biological_indef == "yes") |>
    dplyr::pull(article_id)
)

ggVennDiagram(x, label_color = 'white', label_font = 'bold') +
  scale_fill_gradient(low="grey90",high = "firebrick")

ggsave('out/definition_venn.png', width =8, height = 6, bg = 'white')

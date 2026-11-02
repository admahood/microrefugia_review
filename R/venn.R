# venn diagram
.libPaths(c("/scratch/project_2016646", .libPaths()))
libpath <- .libPaths()[1]

# remotes::install_github('gaospecial/ggVennDiagram')

library(tidyverse)
library(ggVennDiagram)
library(janitor)


d <- readr::read_csv('data/cleaned_and_merged_data___preliminary.csv')

n_mr <- d |> filter(main_concept == 'microrefugia', 
                    is_the_specified_refugia_defined == 'no') |>
  pull(article_id) |>
  unique() |>
  length()
t_mr <- d |> filter(main_concept == 'microrefugia') |>
  pull(article_id) |>
  unique() |>
  length()

n_cr <- d |> filter(main_concept == 'climate_refugia') |>
  pull(article_id) |>
  unique() |>
  length()

xm <- list(
  space = d |>
    dplyr::filter(space_indef == 'yes', main_concept == 'microrefugia') |>
    dplyr::pull(article_id),
  time = d |>
    dplyr::filter(time_indef == 'yes', main_concept == 'microrefugia') |>
    dplyr::pull(article_id),
  climate= d |>
    dplyr::filter(climate_indef == 'yes', main_concept == 'microrefugia') |>
    dplyr::pull(article_id),
  biology = d |>
    dplyr::filter(biological_indef == "yes", main_concept == 'microrefugia') |>
    dplyr::pull(article_id)
)

xc <- list(
  space = d |>
    dplyr::filter(space_indef == 'yes', main_concept == 'climate_refugia') |>
    dplyr::pull(article_id),
  time = d |>
    dplyr::filter(time_indef == 'yes', main_concept == 'climate_refugia') |>
    dplyr::pull(article_id),
  climate= d |>
    dplyr::filter(climate_indef == 'yes', main_concept == 'climate_refugia') |>
    dplyr::pull(article_id),
  biology = d |>
    dplyr::filter(biological_indef == "yes", main_concept == 'climate_refugia') |>
    dplyr::pull(article_id)
)

p_venn <- ggVennDiagram(xm, label_color = 'black', label_font = 'bold') +
  scale_fill_gradient(low="grey97",high = "springgreen", guide = 'none') +
  ggtitle(paste0("Microrefugia (n = ", t_mr, ', ',n_mr, ' Undefined)'))

# ggpubr::ggarrange(
#   ggVennDiagram(xc, label_color = 'black', label_font = 'bold') +
#     scale_fill_gradient(low="grey97",high = "skyblue", guide = 'none') +
#     ggtitle(paste0("Climate Refugia (n = ", n_cr, ')')),
#   ggVennDiagram(xm, label_color = 'black', label_font = 'bold') +
#     scale_fill_gradient(low="grey97",high = "springgreen", guide = 'none') +
#     ggtitle(paste0("Microrefugia (n = ", n_mr, ')'))
# )
# ggsave('out/definition_venn_prelim.png', width =14, height = 6, bg = 'white')

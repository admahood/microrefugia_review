library(dplyr)
library(ggmosaic)
library(colorspace)

dat = read.csv("data/cleaned_and_merged_data.csv")

dat = dat |> 
  dplyr::select(article_id, framework, species_group, spatial_resolution_modelling_rs_new, 
                climate_data_resolution_new)

# recategorize climate data resolution
dat <- dat %>%
  mutate(
    resolution_category = case_when(
      # Point measurement
      climate_data_resolution_new %in% c("point measurement", "point measurements",
                                         "25 m;point measurement", "point measurement;25 m", 
                                         "50 m; point measurement", "90 m; point measurement", 
                                         "30 m;point measurement", "point measurement; 30m",
                                         "1 m; point measurement", 
                                         "0.6 m; point measurement", "1 cm;point measurement",
                                         "1000 m;point measurement", "4000 m; point measurement") ~ "point measurement",
      
      # 0-10 m
      climate_data_resolution_new %in% c("5 m") ~ "0-10 m",
      
      # 10-100 m
      climate_data_resolution_new %in% c("14 m", "15 m", "22 m", "23 m", "25 m", "28 m", 
                                         "30 m", "50 m", "90 m", "100 m") ~ "10-100 m",
      
      # 100-500 m
      climate_data_resolution_new %in% c("250 m", "500 m") ~ "100-500 m",
      
      # 500 - 1000 m
      climate_data_resolution_new %in% c("800 m", "1000 m", "501-1000 m", 
                                        "1000 m;250 m", 
                                         "100 m;1000 m") ~ "500-1000 m",
      
      # 1000-5000 m
      climate_data_resolution_new %in% c("3000 m", "4000 m", "4500 m", "5000 m", 
                                          "1000 m; 5000 m") ~ "1000-5000 m",
      
      # > 5000 m
      climate_data_resolution_new %in% c("9000 m", "10 000 m", "12 000 m", "18 000 m", 
                                         "20 000 m", "25000 m", "27 000 m", "50 000 m", 
                                         "55 000 m", "5000 km") ~ ">5000 m",

      .default = NA
    )
  )

# recategorize taxa
dat = dat %>%
  separate_longer_delim(species_group, delim = ",") %>%
  mutate(species_group = str_trim(species_group),
         species_group = str_to_title(species_group),
         species_group = case_when(
           # Fungi (separate category)
           species_group %in% c("Basidiomycota", "Ascomycota", "Lecanoromycete", "Fungi") ~ "Fungi",
           # Bacteria (separate category)
           species_group %in% c("Bacteria", "Actinomycetes", "Thermoleophilia",
                                "Alphaproteobacteria", "Gammaproteobacteria",
                                "Flavobacteriia", "Bacilli", "Betaproteobacteria") ~ "Bacteria",
           grepl("Simulated", species_group) ~ "Simulated species",
           grepl("No Species", species_group) ~ "No species",
           .default = species_group
         )) |> 
  distinct()

dat = dat |> 
  mutate(species_group = case_when(species_group == "Na" ~ NA,
                                   species_group == "Birds" ~ "Aves",
                                   .default = species_group))

dat <- dat %>%
  mutate(group2 = case_when(
    # Endotherms (warm-blooded)
    species_group %in% c("Aves", "Mammalia") ~ "Endotherms",
    
    # Ectotherms (cold-blooded animals)
    species_group %in% c("Amphibia", "Reptilia", "Actinopterygii", "Arachnida",
                         "Insecta", "Malacostraca", "Mollusca", "Gastropoda",
                         "Bivalvia", "Cnidaria", "Collembola", "Rhabditophora") ~ "Ectotherms",
    
    # Phanerogams (seed plants)
    species_group %in% c("Magnoliopsida", "Liliopsida", "Pinopsida") ~ "Phanerogams",
    
    # Cryptogams (non-seed plants and algae only)
    species_group %in% c("Polypodiopsida", "Bryopsida", "Lycopodiopsida",
                         "Phaeophyceae") ~ "Cryptogams",
    
    .default = species_group
  ),
  group2 = factor(group2, levels = c("Endotherms", "Ectotherms", "Phanerogams",
                                     "Cryptogams", "Fungi", "Bacteria",
                                     "Simulated species", "No species", NA)))

# remove reviews
dat = dat |> 
  filter(framework != "Conceptual | Review") |> 
  drop_na(resolution_category, group2) |> 
  mutate(resolution_category = factor(resolution_category,
                                      levels = c("point measurement", "0-10 m",
                                                 "10-100 m", "100-500 m",
                                                 "500-1000 m", "1000-5000 m",
                                                 ">5000 m")))
  

ggplot(data = dat) +
  geom_mosaic(aes(x = product(resolution_category, group2),
                  fill = resolution_category, color = resolution_category)) +
  scale_fill_discrete_sequential(palette = "viridis") +
  scale_color_discrete_sequential(palette = "viridis") +
  scale_x_productlist(expand = c(0,0)) +
  scale_y_productlist(expand = c(0,0)) +
  theme_minimal() +
  labs(fill = "Climate data\nresolution", color = "Climate data\nresolution") +
  theme(axis.text.x = element_text(angle = 330, vjust = 0, hjust = 0, size = 7),
        axis.title = element_blank(),
        axis.text.y = element_text(size = 7),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7))
ggsave("out/climdat_resolution_taxagroup.png",
       width = 160, height = 90, units = "mm", dpi = 300)
  
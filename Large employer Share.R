library(tidyverse)
library(cansim)
library(sf)

set.seed(2112)

Ontario_csd <- read_sf("ON_CSD.gpkg")


Business_counts <- get_cansim("33-10-0766-01")

Ontario_Counts <- Business_counts |>
  filter((GeoUID >= 3500 & GeoUID < 3600))





Total_bus_on_large <- 	Ontario_Counts |>
  filter(`Hierarchy for North American Industry Classification System (NAICS)` == 1) |>
  filter(
    `Employment size` == "100 to 199 employees" |
      `Employment size` == "200 to 499 employees" |
      `Employment size` == "500 plus employees"
  ) |>
  group_by(GEO, GeoUID) |>
  summarise(Total_Large = sum(val_norm, na.rm = T))  


Total_bus_on_total <- 	Ontario_Counts |>
  filter(`Hierarchy for North American Industry Classification System (NAICS)` == 1) |>
  filter(`Employment size` == "Total, with employees") |>
  group_by(GEO, GeoUID) |>
  summarise(Total_firms = sum(val_norm, na.rm = T)) |>
  left_join(Total_bus_on_large) |>
  mutate(Total_Large = replace_na(Total_Large, 0)) |>
  as_tibble() |>
  as.data.frame() |>
  mutate(Share_Large = (Total_Large / Total_firms)*100 ) |>
  mutate(Share_Large = Share_Large + runif(n(), -1e-15, 1e-15)) |>
  mutate(`Share_Large Index` = cut_number(Share_Large, 15)) |>
  mutate(`Share_Large Index` = case_when(
    Share_Large < 1e-8 ~ "(0)",
    TRUE ~ `Share_Large Index`))
    
  


Ontario_csd_map <- Ontario_csd |>
        left_join(Total_bus_on_total, join_by(CSDUID == GeoUID))  |>
        mutate(`Share_Large Index` = replace_na(`Share_Large Index`, "(0)")) |>
  ggplot(aes(fill = `Share_Large Index`)) +
  geom_sf(linewidth = 0.01) +
  scale_fill_viridis_d() +
  labs(title = "Share of Employers with more than 100 employees",
       subtitle = "Data from Statcan Table '33-10-0766-01', December 2024",
       caption = paste0("Compiled on:", Sys.Date(), " By Dr. Martin Lefebvre")  ) + 
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)


Ontario_csd_map

ggsave("Ontario_csd_map.svg", width = 30, height = 30, units = "cm")
ggsave("Ontario_csd_map.png", width = 30, height = 30, units = "cm")


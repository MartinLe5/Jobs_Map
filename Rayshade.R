library(tidyverse)
library(sf)
library(abdiv)
library("ggspatial")
library(rayshader)
options(scipen = 999)



Jobs_and_HHI_map <- read_sf("Jobs_and_HHI_Map.gpkg")


Jobs_and_HHI_ggplot <- Jobs_and_HHI_map |>
  filter(DAUID >= 35480000) |>
  mutate(`Simpson Index` = simpsons_125) |>
  ggplot(aes(fill = `Simpson Index`)) +
  geom_sf(linewidth = 0.01) +
  scale_fill_viridis_c() +
  labs(title = "Simpson Diverstiy Index at the DA Level for Jobs Within 125 KM",
       subtitle = "Data from 2021 Census at the NAICS2 Level",
       caption = paste0("Compiled on:", Sys.Date(), " By Dr. Martin Lefebvre")  ) + 
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)



Ray_3d_Jobs_and_HHI_ggplot <- plot_gg(Jobs_and_HHI_ggplot, multicore = TRUE, raytrace = TRUE, width = 10, height = 10, 
        scale = 300, windowsize = c(2560 , 1369), zoom = 0.6, phi = 30, theta = 30)

render_snapshot("Northern Ontario Rayshade Simpson Index.png", clear = TRUE)



Jobs_and_HHI_ggplot_jobs <- Jobs_and_HHI_map |>
  mutate(`Jobs Count` = Total_jobs_125 ) |>
  ggplot(aes(fill = `Jobs Count`)) +
  geom_sf(linewidth = 0.01) +
  scale_fill_viridis_c() +
  labs(title = "Count of Employed people at the DA Level for Jobs Within 125 KM",
       subtitle = "Data from 2021 Census with jobs classifed by NAICS2",
       caption = paste0("Compiled on:", Sys.Date(), " By Dr. Martin Lefebvre")  ) + 
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)


Ray_3d_Jobs_and_HHI_ggplot_jobs_ggplot <- plot_gg(Jobs_and_HHI_ggplot_jobs, multicore = TRUE, raytrace = TRUE, width = 10, height = 10, 
                                      scale = 300, windowsize = c(2560 , 1369), zoom = 0.6, phi = 30, theta = 30)

render_snapshot("Northern Ontario Rayshade Jobs.png", clear = TRUE)

library(tidyverse)
library(sf)
library(abdiv)
library("ggspatial")

options(scipen = 999)


##### Data Prep #####
Jobs_data <- read_csv("98-401-X2021006_English_CSV_data_Ontario.csv")

Jobs_data <- Jobs_data |> 
        filter(CHARACTERISTIC_ID >= 2262 & CHARACTERISTIC_ID <= 2281) |>
        select(2,5,9,10,12)
          

Ontario_DA <- read_sf("Ontario_DA.gpkg")

list_of_geos <- Ontario_DA$DGUID

Centroid_matrix <- Ontario_DA |>
                    st_centroid() |>
                    st_distance() |>
                    as.data.frame()


colnames(Centroid_matrix) <- Ontario_DA$DGUID
Centroid_matrix$DGUID <- Ontario_DA$DGUID

Centroid_matrix <- Centroid_matrix |>
                            select(DGUID, everything()) #|>
                           # write_csv("DA_Distance_Matrix.csv") |>
                            

Jobs_and_HHI_Table <- data.frame(DGUID = list_of_geos,
                                 Total_jobs_75 = NA,
                                 HHI_75 = NA,
                                 simpsons_75 = NA,
                                 Total_jobs_100 = NA,
                                 HHI_100 = NA,
                                 simpsons_100 = NA,
                                 Total_jobs_125 = NA,
                                 HHI_125 = NA,
                                 simpsons_125 = NA,
                                 Total_jobs_150 = NA,
                                 HHI_150 = NA,
                                 simpsons_150 = NA
                                  )





for(i in 1:20465){
  
  col_extract <- Centroid_matrix[,c(1,i+1)] 
  
  colnames(col_extract)[2] <- "distance"
  
  col_extract <- col_extract |>
    mutate(distance = as.numeric(distance)) |>
    left_join(Jobs_data) 
  
  ### 75 ###
  

  
  col_extract_75 <- col_extract |>
        filter(distance <= 75000) |>
        group_by(CHARACTERISTIC_ID, CHARACTERISTIC_NAME) |>
        summarise(Jobs = sum(C1_COUNT_TOTAL, na.rm= TRUE)) |>
        as_tibble() |>
        as.data.frame() 
  
  Jobs_sum_75 <- sum(col_extract_75$Jobs)
  
  Simpsons_75 <- simpson(col_extract_75$Jobs)
  
  HHI_value_75 <- col_extract_75 |>
    mutate(Percentage = (Jobs / Jobs_sum_75)*100 ) |>
    mutate(HHI = Percentage^2) |>
    summarise(HHI = sum(HHI))
  
  Jobs_and_HHI_Table$Total_jobs_75[i] <- Jobs_sum_75
  Jobs_and_HHI_Table$HHI_75[i] <- HHI_value_75
  Jobs_and_HHI_Table$simpsons_75[i] <- Simpsons_75
  
  ### 100 ###
  
  col_extract_100 <- col_extract |>
    filter(distance <= 100000) |>
    group_by(CHARACTERISTIC_ID, CHARACTERISTIC_NAME) |>
    summarise(Jobs = sum(C1_COUNT_TOTAL, na.rm= TRUE)) |>
    as_tibble() |>
    as.data.frame() 
  
  Jobs_sum_100 <- sum(col_extract_100$Jobs)
  
  Simpsons_100 <- simpson(col_extract_100$Jobs)
  
  HHI_value_100 <- col_extract_100 |>
    mutate(Percentage = (Jobs / Jobs_sum_100)*100 ) |>
    mutate(HHI = Percentage^2) |>
    summarise(HHI = sum(HHI))
  
  Jobs_and_HHI_Table$Total_jobs_100[i] <- Jobs_sum_100
  Jobs_and_HHI_Table$HHI_100[i] <- HHI_value_100
  Jobs_and_HHI_Table$simpsons_100[i] <- Simpsons_100
  
  ### 125 ###
  
  col_extract_125 <- col_extract |>
    filter(distance <= 125000) |>
    group_by(CHARACTERISTIC_ID, CHARACTERISTIC_NAME) |>
    summarise(Jobs = sum(C1_COUNT_TOTAL, na.rm= TRUE)) |>
    as_tibble() |>
    as.data.frame() 
  
  Jobs_sum_125 <- sum(col_extract_125$Jobs)
  
  Simpsons_125 <- simpson(col_extract_125$Jobs)
  
  HHI_value_125 <- col_extract_125 |>
    mutate(Percentage = (Jobs / Jobs_sum_125)*100 ) |>
    mutate(HHI = Percentage^2) |>
    summarise(HHI = sum(HHI))
  
  Jobs_and_HHI_Table$Total_jobs_125[i] <- Jobs_sum_125
  Jobs_and_HHI_Table$HHI_125[i] <- HHI_value_125
  Jobs_and_HHI_Table$simpsons_125[i] <- Simpsons_125
  
  ### 150 ###
  
  col_extract_150 <- col_extract |>
    filter(distance <= 150000) |>
    group_by(CHARACTERISTIC_ID, CHARACTERISTIC_NAME) |>
    summarise(Jobs = sum(C1_COUNT_TOTAL, na.rm= TRUE)) |>
    as_tibble() |>
    as.data.frame() 
  
  Jobs_sum_150 <- sum(col_extract_150$Jobs)
  
  Simpsons_150 <- simpson(col_extract_150$Jobs)
  
  HHI_value_150 <- col_extract_150 |>
    mutate(Percentage = (Jobs / Jobs_sum_150)*100 ) |>
    mutate(HHI = Percentage^2) |>
    summarise(HHI = sum(HHI))
  
  Jobs_and_HHI_Table$Total_jobs_150[i] <- Jobs_sum_150
  Jobs_and_HHI_Table$HHI_150[i] <- HHI_value_150
  Jobs_and_HHI_Table$simpsons_150[i] <- Simpsons_150
  
  ### Counter ###
  print(paste(i, "of 20466"))
}

Jobs_and_HHI_Table$Total_jobs_75  <- as.numeric(Jobs_and_HHI_Table$Total_jobs_75)
Jobs_and_HHI_Table$HHI_75  <- as.numeric(Jobs_and_HHI_Table$HHI_75)
Jobs_and_HHI_Table$simpsons_75  <- as.numeric(Jobs_and_HHI_Table$simpsons_75)

Jobs_and_HHI_Table$Total_jobs_100  <- as.numeric(Jobs_and_HHI_Table$Total_jobs_100)
Jobs_and_HHI_Table$HHI_100  <- as.numeric(Jobs_and_HHI_Table$HHI_100)
Jobs_and_HHI_Table$simpsons_100  <- as.numeric(Jobs_and_HHI_Table$simpsons_100)

Jobs_and_HHI_Table$Total_jobs_125  <- as.numeric(Jobs_and_HHI_Table$Total_jobs_125)
Jobs_and_HHI_Table$HHI_125  <- as.numeric(Jobs_and_HHI_Table$HHI_125)
Jobs_and_HHI_Table$simpsons_125  <- as.numeric(Jobs_and_HHI_Table$simpsons_125)

Jobs_and_HHI_Table$Total_jobs_150  <- as.numeric(Jobs_and_HHI_Table$Total_jobs_150)
Jobs_and_HHI_Table$HHI_150  <- as.numeric(Jobs_and_HHI_Table$HHI_150)
Jobs_and_HHI_Table$simpsons_150  <- as.numeric(Jobs_and_HHI_Table$simpsons_150)

write_csv(Jobs_and_HHI_Table, "Jobs_and_HHI_Table.csv")


Jobs_and_HHI_map <- Ontario_DA[,c(-3,-4)] |>
                    left_join(Jobs_and_HHI_Table) |>
                    write_sf("Jobs_and_HHI_Map.gpkg")

########################################################################################################

Jobs_and_HHI_map <- read_sf("Jobs_and_HHI_Map.gpkg")


Jobs_and_HHI_ggplot <- Jobs_and_HHI_map |>
                        mutate(`Simpson Index` = cut_number(simpsons_125, 10)) |>
                        ggplot(aes(fill = `Simpson Index`)) +
                          geom_sf(linewidth = 0.01) +
                          scale_fill_viridis_d() +
                          labs(title = "Simpson Diverstiy Index at the DA Level for Jobs Within 125 KM",
                               subtitle = "Data from 2021 Census at the NAICS2 Level",
                               caption = paste0("Compiled on:", Sys.Date(), " By Dr. Martin Lefebvre")  ) + 
                          annotation_scale(location = "bl", width_hint = 0.5) +
                          annotation_north_arrow(location = "bl", which_north = "true", 
                                                 pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                                                 style = north_arrow_fancy_orienteering)


Jobs_and_HHI_ggplot

ggsave("Simpsons 125km range.svg", width = 30, height = 30, units = "cm")
ggsave("Simpsons 125km range.png", width = 30, height = 30, units = "cm")



### Northern Ontario ####

NO_Jobs_and_HHI_ggplot <- Jobs_and_HHI_map |>
  filter(DAUID >= 35480000) |>
  mutate(`Simpson Index` = cut_number(simpsons_125, 8)) |>
  ggplot(aes(fill = `Simpson Index`)) +
  geom_sf(linewidth = 0.01) +
  scale_fill_viridis_d() +
  labs(title = "Simpson Diverstiy Index at the DA Level for Jobs Within 125 KM",
       subtitle = "Data from 2021 Census at the NAICS2 Level",
       caption = paste0("Compiled on:", Sys.Date(), " By Dr. Martin Lefebvre")  ) + 
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)


NO_Jobs_and_HHI_ggplot

ggsave("NO_Simpsons 125km range.svg", width = 30, height = 30, units = "cm")
ggsave("NO_Simpsons 125km range.png", width = 30, height = 30, units = "cm")

######################

Jobs_and_HHI_ggplot_jobs <- Jobs_and_HHI_map |>
  mutate(`Jobs Count` = cut_number(Total_jobs_125, 10, dig.lab = 10)) |>
  ggplot(aes(fill = `Jobs Count`)) +
  geom_sf(linewidth = 0.01) +
  scale_fill_viridis_d() +
  labs(title = "Count of Employed people at the DA Level for Jobs Within 125 KM",
       subtitle = "Data from 2021 Census with jobs classifed by NAICS2",
       caption = paste0("Compiled on:", Sys.Date(), " By Dr. Martin Lefebvre")  ) + 
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)


Jobs_and_HHI_ggplot_jobs

ggsave("Drive 125km range.svg", width = 30, height = 30, units = "cm")
ggsave("Drive 125km range.png", width = 30, height = 30, units = "cm")

#### Northern Ontario ####

NO_Jobs_and_HHI_ggplot_jobs <- Jobs_and_HHI_map |>
  filter(DAUID >= 35480000) |>
  mutate(`Jobs Count` = cut_number(Total_jobs_125, 8, dig.lab = 10)) |>
  ggplot(aes(fill = `Jobs Count`)) +
  geom_sf(linewidth = 0.01) +
  scale_fill_viridis_d() +
  labs(title = "Count of Employed people at the DA Level for Jobs Within 125 KM",
       subtitle = "Data from 2021 Census with jobs classifed by NAICS2",
       caption = paste0("Compiled on:", Sys.Date(), " By Dr. Martin Lefebvre")  ) + 
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)


NO_Jobs_and_HHI_ggplot_jobs

ggsave("NO_Drive 125km range.svg", width = 30, height = 30, units = "cm")
ggsave("NO_Drive 125km range.png", width = 30, height = 30, units = "cm")

##########################



Jobs_and_HHI_ggplot_HHI <- Jobs_and_HHI_map |>
  mutate(`Herfindahl–Hirschman index` = cut_number(HHI_125, 10)) |>
  ggplot(aes(fill = `Herfindahl–Hirschman index`)) +
  geom_sf(linewidth = 0.01) +
  scale_fill_viridis_d() +
  labs(title = "Herfindahl–Hirschman Index of Job Diversity the DA Level for Jobs Within 125 KM",
       subtitle = "Data from 2021 Census with jobs classifed by NAICS2",
       caption = paste0("Compiled on:", Sys.Date(), " By Dr. Martin Lefebvre")  ) + 
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)


Jobs_and_HHI_ggplot_HHI

ggsave("HHI 125km range.svg", width = 30, height = 30, units = "cm")
ggsave("HHI 125km range.png", width = 30, height = 30, units = "cm")



#### Northern Ontario ####


NO_Jobs_and_HHI_ggplot_HHI <- Jobs_and_HHI_map |>
  filter(DAUID >= 35480000) |>
  mutate(`Herfindahl–Hirschman index` = cut_number(HHI_125, 8)) |>
  ggplot(aes(fill = `Herfindahl–Hirschman index`)) +
  geom_sf(linewidth = 0.01) +
  scale_fill_viridis_d() +
  labs(title = "Herfindahl–Hirschman Index of Job Diversity the DA Level for Jobs Within 125 KM",
       subtitle = "Data from 2021 Census with jobs classifed by NAICS2",
       caption = paste0("Compiled on:", Sys.Date(), " By Dr. Martin Lefebvre")  ) + 
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)


NO_Jobs_and_HHI_ggplot_HHI

ggsave("NO_HHI 125km range.svg", width = 30, height = 30, units = "cm")
ggsave("NO_HHI 125km range.png", width = 30, height = 30, units = "cm")

####################################################################


#######  Indexed #########

df_zscore <- Jobs_and_HHI_map %>%
  mutate(across(3:14, ~ (. - mean(.)) / sd(.))) |>
  mutate(KM_75 = Total_jobs_75 + HHI_75, 
         KM_100 = Total_jobs_100 + HHI_100, 
         KM_125 = Total_jobs_125 + HHI_125,
         KM_150 = Total_jobs_125 + HHI_125)







Brittleness_index_75 <- df_zscore |>
  mutate(`Brittleness Index` = cut_number(KM_75, 10)) |>
  ggplot(aes(fill = `Brittleness Index`)) +
  geom_sf(linewidth = 0.001) +
  scale_fill_viridis_d() +
  labs(title = "Brittleness Index of Job Quantity and Diversity the DA Level for Jobs Within 75 KM",
       subtitle = "Data from 2021 Census with jobs classifed by NAICS2",
       caption = paste0("Compiled on:", Sys.Date(), " By Dr. Martin Lefebvre")  ) + 
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)


Brittleness_index_75

ggsave("Brittleness_index_75.svg", width = 30, height = 30, units = "cm")
ggsave("Brittleness_index_75.png", width = 30, height = 30, units = "cm")


Brittleness_index_100 <- df_zscore |>
  mutate(`Brittleness Index` = cut_number(KM_100, 10)) |>
  ggplot(aes(fill = `Brittleness Index`)) +
  geom_sf(linewidth = 0.001) +
  scale_fill_viridis_d() +
  labs(title = "Brittleness Index of Job Quantity and Diversity the DA Level for Jobs Within 100 KM",
       subtitle = "Data from 2021 Census with jobs classifed by NAICS2",
       caption = paste0("Compiled on:", Sys.Date(), " By Dr. Martin Lefebvre")  ) + 
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)


Brittleness_index_100

ggsave("Brittleness_index_100.svg", width = 30, height = 30, units = "cm")
ggsave("Brittleness_index_100.png", width = 30, height = 30, units = "cm")



Brittleness_index_125 <- df_zscore |>
  mutate(`Brittleness Index` = cut_number(KM_125, 10)) |>
  ggplot(aes(fill = `Brittleness Index`)) +
  geom_sf(linewidth = 0.001) +
  scale_fill_viridis_d() +
  labs(title = "Brittleness Index of Job Quantity and Diversity the DA Level for Jobs Within 125 KM",
       subtitle = "Data from 2021 Census with jobs classifed by NAICS2",
       caption = paste0("Compiled on:", Sys.Date(), " By Dr. Martin Lefebvre")  ) + 
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)


Brittleness_index_125

ggsave("Brittleness_index_125.svg", width = 30, height = 30, units = "cm")
ggsave("Brittleness_index_125.png", width = 30, height = 30, units = "cm")





Brittleness_index_150 <- df_zscore |>
  mutate(`Brittleness Index` = cut_number(KM_150, 10)) |>
  ggplot(aes(fill = `Brittleness Index`)) +
  geom_sf(linewidth = 0.001) +
  scale_fill_viridis_d() +
  labs(title = "Brittleness Index of Job Quantity and Diversity the DA Level for Jobs Within 125 KM",
       subtitle = "Data from 2021 Census with jobs classifed by NAICS2",
       caption = paste0("Compiled on:", Sys.Date(), " By Dr. Martin Lefebvre")  ) + 
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)


Brittleness_index_150

ggsave("Brittleness_index_150.svg", width = 30, height = 30, units = "cm")
ggsave("Brittleness_index_150.png", width = 30, height = 30, units = "cm")


zscore_Index_DB <- df_zscore |>
                      st_drop_geometry() |>
                      write_csv("Z_Score_Brittleness_Index.csv")

Jobs_and_HHI_DF <- Jobs_and_HHI_map |>
                        st_drop_geometry() |>
                        write_csv("Jobs_and_HHI_DF.csv")

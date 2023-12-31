library(sf)
library(tidyverse)
library(tictoc)
source("src/spatial-utils.R")

tic()
## Check to ensure each directory has all the layers the first directory has
files <- list.files("raw_data/quadrats/quadrat34/", pattern = '.tif')
directories <- sprintf("raw_data/quadrats/quadrat%02d/", seq(34, 83, 1))

for(directory in directories ){
  
  to_check <- list.files(directory, pattern = ".tif")
  
  if(!length(setdiff(files, to_check)) == 0 || !length(setdiff(to_check, files)) == 0){
    print(directory)
    print(setdiff(files, to_check))
    print(setdiff(to_check, files))
  }
  
  for(file in list.files(directory, full.names = TRUE)){
    if(!file.size(file)){
      print(file)
    }
  }
}


directories <- tibble(directory = sprintf("raw_data/quadrats/quadrat%02d/", seq(34, 83, 1))) 

data <- directories |>
  mutate(image = map(directory, ~list.files(.x, pattern = '.tif'))) |>
  unnest(cols = image) |>
  mutate(image_path = paste0(directory, image)) |>
  mutate(polygon_path = paste0(directory, 'polygons.json')) |>
  mutate(include_polygon_data = case_when(
    image == 'rgb.tif' ~ TRUE,
    TRUE ~ FALSE
  )) 
  


args <- list(data$image_path, data$polygon_path, data$include_polygon_data)

data$pixels <- args |> 
  pmap(extract_polygon_pixels) 

pixels <-  data |>
  group_by(image)  |>
  summarise(feature = list(bind_rows(pixels)))
 
pixels <- bind_cols(pixels$feature) |>
  as_tibble() |>
  relocate(c(label, key, label, cell, area), .before = hsl_1) 

## After manually looking at the data set these changes need to be made
polygons_to_remove <- read_csv("clean_data/polygons_to_remove.csv", show_col_types = FALSE) |>
  mutate(key = paste0(quadrat, "_", poly_num)) |>
  select(key)

polygon_class_changes <- read_csv("clean_data/polygon_class_changes.csv", show_col_types = FALSE) |>
  mutate(key = paste0(quadrat, "_", poly_num)) |>
  select(c(key, class_to_change_to))

labeled_pixels  <- pixels |>
  filter(!(key %in% polygons_to_remove$key)) |>
  left_join(polygon_class_changes, by='key') |>
  mutate(label = case_when(
    !is.na(class_to_change_to) ~ class_to_change_to,
    TRUE ~ label
  )) |>
  select(-c(class_to_change_to)) |>
  dplyr::mutate(label = case_when(label == "live Marram grass" ~ "live vegetation", TRUE ~ label)) |>
  filter(is.element(label, c("live vegetation", "dead vegetation", "sand")))


# Save Data
saveRDS(labeled_pixels, 'clean_data/labeled_pixels.rds')
toc()


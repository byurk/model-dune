library(sf)
library(tidyverse)
library(tictoc)
source("src/extract-polygon-pixels.R")



## Check to ensure each directory has all the layers needed
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



directories <- tibble(
  directory = sprintf("raw_data/quadrats/quadrat%02d/", seq(34, 83, 1)),
  polygon_path = sprintf("raw_data/quadrats/quadrat%02d/polygons.json", seq(34, 83, 1))
)

data <- directories |>
  mutate(image = map(directory, ~list.files(.x, pattern = '.tif'))) |>
  unnest(cols = image) |>
  mutate(image_path = paste0(directory, image))

args <- list(data$image_path, data$polygon_path)

data$pixels <-  pmap(args, extract_polygon_pixels)

data
toc()


## Save Data
#raw_pixels <- data
#saveRDS(raw_pixels, 'clean_data/pixels.rds')


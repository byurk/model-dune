library(terra)
source('src/labelme-json-to-sf.R')

extract_polygon_pixels <- function(image_path, polygon_path, extent = c(0, 4032, 0, 3024)) {
  image <- terra::rast(image_path)
  
  # If there is just one layer we need to fix the names, eg contrast etc
  if (length(names(image)) == 1) {
    
    feature_split <- str_split(image_path, pattern = "/")[[1]]
    feature_file <-  tail(feature_split, 1)
    feature <- str_split(feature_file, ".tif")[[1]][1] 
    
    names(image) <- feature
  }
  
  
  polygon <- label_me_json_to_sf(polygon_path)
  
  poly_info <- polygon |>
    st_drop_geometry() |>
    mutate(ID = row_number())
  
  pixels <- terra::extract(image, polygon, cells = TRUE, extent = extent) |>
    left_join(poly_info, by = "ID") |>
    dplyr::select(-c(ID,area)) |>
    as_tibble()
  
  return(pixels)
}

# image_path <- "raw_data/quadrats/quadrat59/hsv_contrast_L3_W7.tif"
# polygon_path <- "raw_data/quadrats/quadrat59/polygons.json"
# pixels <- extract_pixels(image_path, polygon_path)


library(terra)
source('src/labelme-json-to-sf.R')

extract_polygon_pixels <- function(image_path, polygon_path, include_polygon_info = TRUE , extent = c(0, 4032, 0, 3024)) {
  image <- terra::rast(image_path)
  
  quadrat_number <- gsub("\\D", "", image_path)
  
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
    mutate(ID = row_number()) |>
    mutate(key = paste0(quadrat_number,"_", ID))
  
  pixels <- terra::extract(image, polygon, cells = TRUE, extent = extent) 
  
  if(include_polygon_info){
    
    pixels <- pixels |>
      left_join(poly_info, by = "ID") |>
      dplyr::select(-c(ID, imagePath )) |>
      as_tibble()
  } else {
    pixels <- pixels |>
      dplyr::select(-c(ID, cell))
  }
  
  return(pixels)
}

# image_path <- "raw_data/quadrats/quadrat35/rgb.tif"
# polygon_path <- "raw_data/quadrats/quadrat35/polygons.json"
# test_pixels <- extract_polygon_pixels(image_path, polygon_path, FALSE)
# test_pixels
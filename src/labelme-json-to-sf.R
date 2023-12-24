library(sf)
library(jsonlite)


#function that converts matrix to polygon
polygon_from_matrix <- function(point_matrix, image_height) {
  #transform y so origin is at bottom left corner of image
  point_matrix[, 2] <- image_height - point_matrix[, 2]
  
  #repeat first point at end
  point_matrix <- rbind(point_matrix, point_matrix[1,])
  point_matrix_list <- list(point_matrix)
  #create sf polygon
  poly <- st_polygon(point_matrix_list)
  return(poly)
}

label_me_json_to_sf <- function(json_path) {
  
  #read in json file from labelme
  labeled_polys_list <- read_json(json_path, simplifyVector = TRUE)
  labeled_polys_list$imageData <- NULL
  
  #get the shapes data frame with all the stuff we want
  labeled_polys_tib <-
    labeled_polys_list$shapes |>
    as_tibble() |>
    mutate(imagePath = labeled_polys_list$imagePath)
  
  image_width <- labeled_polys_list$imageWidth
  image_height <- labeled_polys_list$imageHeight
  
  # add polygons to the tibble
  labeled_polys_tib <- labeled_polys_tib |>
    mutate(polys = lapply(points, polygon_from_matrix, image_height = image_height)) |>
    dplyr::select(-points)
  
  # create the sf object and remove unneeded variables
  labeled_polys_sf <-
    labeled_polys_tib |> 
    st_as_sf(sf_column_name = "polys") |>
    dplyr::select(-c(flags, group_id, shape_type))
  
  labeled_polys_sf$area = st_area(labeled_polys_sf$polys)
  
  return(labeled_polys_sf)
}


library(glcm)
library(sf)

add_ndvi <- function(ortho, red_band = 3L, nir_band = 5L){
  ndvi <- (ortho[[nir_band]] - ortho[[red_band]]) / (ortho[[nir_band]] + ortho[[red_band]])
  names(ndvi) <- "ndvi"
  c(ortho, ndvi)
}

get_contrast <- function(ortho, layer = 7L, statistic = 'contrast', window = 11L){
  raster<- ortho[[layer]]
  layer_name <- names(ortho)[layer]
  
  texture <- glcm(
    raster,
    statistics = statistic,
    window = c(window, window),
    shift = list(c(0, 1), c(1, 1), c(1, 0), c(1, -1)),
  )
  
  names(texture) <- paste0(layer_name, '_', statistic, '_W', window)
  return(terra::rast(texture))
}

# quadrant (0-3) is a region within the quadrat
# See https://docs.google.com/document/d/1fZsO6068yEvLJnU3if5tpVF9bU1i7cqadtJvj_2KteU/edit

create_quadrant_polygon <- function(quadrant, c_coords, i_coords, ii_coords) {
  #print(c_coords)
  #print(i_coords)
  # quadrant <- 0
  # 
  # # Assign values to c_coords, i_coords, and ii_coords
  # c_coords <- first$corner_c[[1]]
  # i_coords <- first$corner_i[[1]]
  # ii_coords <- first$corner_ii[[1]] 
  
  # Check and flip corners if necessary using cross_product
  # if (!cross_product(ii_coords - c_coords, i_coords - c_coords)) {
  #   temp <- i_coords
  #   i_coords <- ii_coords
  #   ii_coords <- temp
  # }
  
  # Computing all coordinates needed for the four quadrants
  iii_coords <- i_coords + (ii_coords - c_coords)
  s1 <- (c_coords + i_coords) / 2
  s2 <- (i_coords + iii_coords) / 2
  s3 <- (ii_coords + iii_coords) / 2
  s4 <- (c_coords + ii_coords) / 2
  m <- (c_coords + iii_coords) / 2
  
  # Create a matrix with the coordinates of the specified quadrant
  coords <- switch(as.character(quadrant),
                   "0" = rbind(c_coords, s1, m, s4, c_coords),
                   "1" = rbind(s1, i_coords, s2, m, s1),
                   "2" = rbind(s4, m, s3, ii_coords, s4),
                   "3" = rbind(m, s2, iii_coords, s3, m),
                   "total" = rbind(c_coords, i_coords, iii_coords, ii_coords, c_coords)) 
  
  
  coords <-  lapply(coords, st_coordinates)
  #print(coords)
  
  polygon <- st_polygon(coords)
  #print(polygon)
  
  return(list(polygon))
}

cross_product <- function(x, y) {
  z <- cross(to_3d(x), to_3d(y))
  return(z[3] > 0)
}

to_3d <- function(t) {
  return(c(t[1], t[2], 0))
}


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

label_me_points_json_to_sf <- function(json_path){
  json <- read_json(json_path,simplifyVector=TRUE)
  json$imageData <- NULL
  
  shapes <- json$shapes
  imagePath <- json$imagePath
  im_wid <- json$imageWidth
  im_ht <- json$imageHeight
  
  #convert points to sf object
  labeled_points_sf <- shapes |>
    filter(shape_type == "point") |>
    mutate(
      x = sapply(points, function(p) p[[1]]),
      y = sapply(points, function(p) im_ht - p[[2]])
    ) |>
    mutate(corner = label) |>
    dplyr::select(-c(flags,group_id,shape_type, points, label)) |>
    rowwise() |>
    mutate(points = list(st_point(x = c(as.numeric(x), as.numeric(y)) ))) |>
    ungroup() |>
    st_as_sf()
  
  
  return(labeled_points_sf)
}





scale_polygon <- function(polygon, old_xmin = 0, old_xmax = 4032, old_ymin = 0, old_ymax = 3024, new_xmin = 0, new_xmax= 1, new_ymin = 0, new_ymax = 1)  {
  # Extract coordinates
  coords <- st_coordinates(polygon)[,-3][,-3]
  
  # Scale the x-coordinates
  coords[, 1] <- (coords[, 1] - old_xmin) / (old_xmax - old_xmin) * (new_xmax - new_xmin) + new_xmin
  
  # Scale the y-coordinates
  coords[, 2] <- (coords[, 2] - old_ymin) / (old_ymax - old_ymin) * (new_ymax - new_ymin) + new_ymin
  
  # Create a new polygon with scaled coordinates
  new_polygon <- st_polygon(list(coords))
  
  return(new_polygon)
}

sanitize_polygon <- function(polygon)  {
  # Extract coordinates
  coords <- st_coordinates(polygon)[,-3][,-3]
  
  # Create a new polygon with scaled coordinates
  new_polygon <- st_polygon(list(coords))
  
  return(new_polygon)
}

#polygon <- ground_quadrants[2,]$geometry[[1]]
#scaled <- scale_polygon(polygon)
#scaled

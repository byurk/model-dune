source('code/src/spatial-utils.R')
library(terra)
library(sf)
# Extract 200 quarters from 50 quadrants in both the classified images and the drone orthomasic
# The classified images serve as training data for the multinomial logistic regression model

processed_ortho_path <- 'clean_data/ortho/processed_ortho.tif'
processed_ortho <- terra::rast(processed_ortho_path)
ortho <- terra::rast('raw_data/ortho/ortho.tif')
ortho_crs <- crs(ortho)

add_contrast <- function(ortho){
  contrast <- get_contrast(raster::brick(ortho), layer=7L, window=5L)
  return(c(ortho,contrast))
}

# Extract drone data
corner_points_ortho <- st_read("raw_data/ortho/coord_layer.shp") |>
  rename(quadrat = id) |>
  mutate(quadrat = as.character(quadrat)) |>
  as_tibble()

corner_points_ortho

quadrants <- corner_points_ortho |>
  pivot_wider(names_from = corner,  values_from = geometry, names_prefix = "corner_") |>
  rowwise() |>
  mutate(
         zero = create_quadrant_polygon(0, corner_c, corner_i, corner_ii),
         one = create_quadrant_polygon(1,  corner_c, corner_i, corner_ii),
         two = create_quadrant_polygon(2, corner_c, corner_i, corner_ii),
         three = create_quadrant_polygon(3,  corner_c, corner_i, corner_ii),
         total = create_quadrant_polygon('total',  corner_c, corner_i, corner_ii)) |>
  dplyr::select(-c(corner_c, corner_i, corner_ii))|>
  mutate(
    total = list(sanitize_polygon(total)),
    zero = list(sanitize_polygon(zero)),
    one = list(sanitize_polygon(one)),
    two  = list(sanitize_polygon(two)),
    three = list(sanitize_polygon(three))
         ) |>
  ungroup() |>
  mutate(quad_crop = map(total,\(x)(terra::crop(ortho, x)))) |>
  mutate(quad_crop = map(quad_crop, add_ndvi)) |>
  mutate(quad_crop = map(quad_crop, .f=add_contrast)) |>
  mutate(quad_crop = map(quad_crop, .f = \(x){  
    crs(x) <- ortho_crs
    return(x)
  }))


quad <- quadrants |>
  dplyr::select(c(quadrat, zero, one, two, three, quad_crop)) |>
  pivot_longer(cols = c(zero, one, two, three), names_to = "quadrant", values_to = "geometry") |>
  rowwise() |>
  mutate(geometry = list(sanitize_polygon(geometry))) |>
  ungroup() |>
  rename(polys = geometry) |>
  mutate(quadrant_key = paste(quadrat, quadrant, sep = "_")) |>
  #rowwise() |>
  #mutate(is_valid = st_is_valid(polys)) |>
  #ungroup() |>
  dplyr::select(-c(quad_crop,))|>
  nest(polys =c(polys, quadrat)) |>
  mutate(polys = map(polys, st_as_sf)) |>
  mutate(quadrat = str_extract(quadrant_key, "\\d+")) |>
  left_join(quadrants, by ='quadrat') |>
  dplyr::select(-c(zero, one, two, three, total,))|>
  mutate(aggregates = map2(quad_crop, polys, \(x,y){
    aggregate_functions <- list(
      "sd" = "sd",
      "mean" = "mean",
      "max" = "max",
      "min" = "min"
    )
    results <- list()
    for (statistic_name in names(aggregate_functions)) {
      func <- aggregate_functions[[statistic_name]]
      result <- terra::extract(x, y, fun=func, na.rm=TRUE)
      names(result) <- paste0(names(result), "_", statistic_name)
      results[[statistic_name]] <- result[,-1]
    }
    
    return(bind_cols(unname(results)))
  }))

ortho_data <- quad |>
  select(c(quadrant_key,aggregates)) |>
  unnest(cols = aggregates)
  
# Extract quadrats orthomosaic data
corner_points <- tibble(json_path = sprintf("raw_data/quadrats/quadrat%02d/points.json", seq(34, 83, 1)))

model_name <- 'xgb_fit_model_final'
image_files <- tibble(classified = list.files(glue("clean_data/classified/{model_name}"), pattern = ".tif", full.names = TRUE))
num_image_files <-length(image_files$classified)

data <- bind_cols(corner_points[1:num_image_files,], image_files) |>
  mutate(quadrat = gsub("\\D", "", classified)) |>
  rowwise() |>
  mutate(polygon = list(label_me_points_json_to_sf(json_path))) |>
  ungroup() |>
  unnest(cols = polygon)

geometry <- data |>
  dplyr::select(quadrat, corner, points) |>
  pivot_wider(names_from = corner,  values_from = points, names_prefix = "corner_")

image_key <- dplyr::select(data, c(quadrat, classified)) |>
  group_by(quadrat) |>
  slice(1) |>
  ungroup()

ground_quadrants <- geometry |>
  rowwise() |>
  mutate(zero = create_quadrant_polygon(0, corner_c, corner_i, corner_ii),
         one = create_quadrant_polygon(1,  corner_c, corner_i, corner_ii),
         two = create_quadrant_polygon(2, corner_c, corner_i, corner_ii),
         three = create_quadrant_polygon(3,  corner_c, corner_i, corner_ii)) |>
  ungroup() |>
  dplyr::select(c(quadrat, zero, one, two, three)) |>
  pivot_longer(cols = c(zero, one, two, three), names_to = "quadrant", values_to = "geometry") |>
  mutate(quadrant_key = paste(quadrat, quadrant, sep = "_")) |>
  left_join(image_key, by = 'quadrat') |>
  mutate(polys = geometry) |>
  rowwise() |>
  mutate(polys = list(scale_polygon(geometry))) |>
  mutate(is_valid = st_is_valid(polys)) |>
  ungroup() |>
  dplyr::select(-geometry) |>
  nest(polys= c(quadrant, polys, quadrant_key)) |>
  rowwise() |>
  mutate(polys = list(st_as_sf(polys))) |>
  ungroup()


ground_data_nested <- ground_quadrants |>
  rowwise() |>
  mutate(aggregates = list(extract_polygon_pixels(classified, polygon = polys, extent = c(0,1,0,1), include_polygon_info = TRUE))) |>
  ungroup()


ground_data <- bind_rows(ground_data_nested$aggregates) |>
  rename(class = lyr1) |>
  dplyr::select(c(class, quadrant_key)) |>
  group_by(quadrant_key, class) |>
  summarise(n = n()) |>
  pivot_wider(
    names_from = class,
    values_from = n,
    names_prefix = "class_"
    ) |>
  ungroup()

names(ground_data) <- c('quadrant_key', 'dead', 'grass', 'sand')


training <- ground_data |>
  left_join(ortho_data, by ='quadrant_key') |>
  mutate(across(everything(), \(x)(replace_na(x, 0))))

# Save the data
saveRDS(training, 'clean_data/training-multinomial.rds')


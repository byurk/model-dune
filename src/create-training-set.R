source('src/spatial-utils.R')
library(terra)
library(sf)
# Extract 200 quarters from 50 quadrants in both the classified images and the drone orthomasic
# The classified images serve as training data for the multinomial logistic regression model

processed_ortho_path <- 'clean_data/processed_ortho.tif'
processed_ortho <- terra::rast(processed_ortho_path)

# Extract drone data
corner_points_ortho <- st_read("~/projects/archive/quadClassify/raw_data/drone_sitched/coord_layer.shp") |>
  rename(quadrat = id) |>
  mutate(quadrat = as.character(quadrat)) |>
  as_tibble()

corner_points_ortho

quadrants <- corner_points_ortho |>
  pivot_wider(names_from = corner,  values_from = geometry, names_prefix = "corner_") |>
  rowwise() |>
  mutate(zero = create_quadrant_polygon(0, corner_c, corner_i, corner_ii),
         one = create_quadrant_polygon(1,  corner_c, corner_i, corner_ii),
         two = create_quadrant_polygon(2, corner_c, corner_i, corner_ii),
         three = create_quadrant_polygon(3,  corner_c, corner_i, corner_ii)) |>
  ungroup() |>
  dplyr::select(c(quadrat, zero, one, two, three)) |>
  pivot_longer(cols = c(zero, one, two, three), names_to = "quadrant", values_to = "geometry") |>
  rowwise() |>
  mutate(geometry = list(sanitize_polygon(geometry))) |>
  ungroup() |>
  rename(polys = geometry) |>
  mutate(quadrant_key = paste(quadrat, quadrant, sep = "_")) |>
  rowwise() |>
  mutate(is_valid = st_is_valid(polys)) |>
  ungroup() |>
  nest(polys = c(quadrant, polys)) |>
  rowwise() |>
  mutate(polys = list(st_as_sf((polys))))


ortho_data<- quadrants |>
  mutate(pixels = list(terra::extract(x = processed_ortho, y = polys, fun = mean , exact = TRUE))) |>
  unnest(pixels) |>
  dplyr::select(-c(quadrat,is_valid, polys, ID))

# Extract quadrats orthomosaic data
corner_points <- tibble(json_path = sprintf("raw_data/quadrats/quadrat%02d/points.json", seq(34, 83, 1)))

model_name <- 'xgb_fit'
image_files <-tibble(classified = list.files(glue('clean_data/classified/{model_name}')[[1]], pattern ='.tif', full.names = TRUE))
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

pixels_path <- 'clean_data/labeled_pixels.rds'
pixels <- readRDS(pixels_path) |>
  mutate(label = as.factor(label))

levels <- levels(pixels$label) |>
  map(\(x)(str_replace(x," ", "_"))) |> 
  unlist()

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

names(ground_data) <- c('quadrant_key', levels)


training <- ground_data |>
  left_join(ortho_data, by ='quadrant_key')

# Save the data
saveRDS(training, 'clean_data/training.rds')


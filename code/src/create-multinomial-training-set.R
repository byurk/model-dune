source('code/src/spatial-utils.R')
library(terra)
library(sf)
# Extract 200 quarters from 50 quadrants in both the classified images and the drone orthomasic
# The classified images serve as training data for the multinomial logistic regression model

processed_ortho_path <- 'clean_data/ortho/processed_ortho.tif'
processed_ortho <- terra::rast(processed_ortho_path)
ortho <- terra::rast('raw_data/drone_sitched/ortho.tif')
ortho_crs <- crs(ortho)

add_contrast <- function(ortho) {
  contrast <- get_contrast(raster::brick(ortho), layer = 7L, window = 5L)
  return(c(ortho, contrast))
}

# Extract drone data
corner_points_ortho <- st_read("raw_data/drone_sitched/coord_layer.shp") |>
  rename(quadrat = id) |>
  mutate(quadrat = as.character(quadrat)) |>
  as_tibble()

corner_points_ortho

quadrants <- corner_points_ortho |>
  pivot_wider(
    names_from = corner,
    values_from = geometry,
    names_prefix = "corner_"
  ) |>
  rowwise() |>
  mutate(
    zero = create_quadrant_polygon(0, corner_c, corner_i, corner_ii),
    one = create_quadrant_polygon(1, corner_c, corner_i, corner_ii),
    two = create_quadrant_polygon(2, corner_c, corner_i, corner_ii),
    three = create_quadrant_polygon(3, corner_c, corner_i, corner_ii),
    total = create_quadrant_polygon('total', corner_c, corner_i, corner_ii)
  ) |>
  dplyr::select(-c(corner_c, corner_i, corner_ii)) |>
  mutate(
    total = list(sanitize_polygon(total)),
    zero = list(sanitize_polygon(zero)),
    one = list(sanitize_polygon(one)),
    two = list(sanitize_polygon(two)),
    three = list(sanitize_polygon(three))
  ) |>
  ungroup() |>
  mutate(
    quad_crop = map(total, .f = \(x) {
      terra::crop(ortho, vect(x), mask = TRUE) # mask to make sure we don't pixels outside of quadrat
    })
  ) |>
  #mutate(quad_crop = map(total,\(x)(terra::mask(quad_crop, x)))) |>
  mutate(quad_crop = map(quad_crop, add_hsv)) |>
  mutate(quad_crop = map(quad_crop, add_ndvi)) |>
  #mutate(quad_crop = map(quad_crop, .f=add_contrast)) |> # does not work well with quadrats that are not aligned with cardinal directions
  mutate(
    quad_crop = map(quad_crop, .f = \(x) {
      crs(x) <- ortho_crs
      return(x)
    })
  )


# all_pix <- quadrants |>
#   select(-c(zero, one, two, three, total)) %>%
#   mutate(pix_vals = map(quad_crop, \(x)(values(x, dataframe=TRUE)))) %>%
#   unnest(pix_vals) %>%
#   select(-quad_crop)
#
# all_pix %>%
#   mutate(ndvi_sqrt = sqrt(if_else(ndvi>0,ndvi,0))) %>%
#   mutate(hue_sqrt = sqrt(hue)) %>%
#   mutate(hue_log = log(hue+0.05)) %>%
#   ggplot(aes(ndvi)) +
#   geom_histogram() +
#   theme_minimal()
#
# all_pix %>%
#   mutate(ndvi_sqrt = sqrt(if_else(ndvi>0,ndvi,0))) %>%
#   mutate(hue_sqrt = sqrt(hue)) %>%
#   mutate(hue_log = log(hue+0.05)) %>%
#   ggplot(aes(hue_log, ndvi)) +
#   geom_hex(bins = 75) +
#   scale_fill_continuous(type = "viridis") +
#   theme_minimal()

quad <- quadrants |>
  dplyr::select(c(quadrat, zero, one, two, three, quad_crop)) |>
  pivot_longer(
    cols = c(zero, one, two, three),
    names_to = "quadrant",
    values_to = "geometry"
  ) |>
  rowwise() |>
  mutate(geometry = list(sanitize_polygon(geometry))) |>
  ungroup() |>
  rename(polys = geometry) |>
  mutate(quadrant_key = paste(quadrat, quadrant, sep = "_")) |>
  #rowwise() |>
  #mutate(is_valid = st_is_valid(polys)) |>
  #ungroup() |>
  dplyr::select(-c(quad_crop, )) |>
  nest(polys = c(polys, quadrat)) |>
  mutate(polys = map(polys, st_as_sf)) |>
  mutate(quadrat = str_extract(quadrant_key, "\\d+")) |>
  left_join(quadrants, by = 'quadrat') |>
  dplyr::select(-c(zero, one, two, three, total, )) |>
  mutate(
    vals = map2(quad_crop, polys, .f = \(x, y) {
      terra::crop(x, vect(y), mask = TRUE, touches = FALSE, snap = "in") %>% # with near will get some cells that show up in multiple quandrants, in will avoid this but some cells may be missed entirely, out will have a lot of overlap
        terra::values(dataframe = TRUE, na.rm = TRUE)
    })
  ) #|>
# mutate(aggregates = map2(quad_crop, polys, \(x,y){
#   aggregate_functions <- list(
#     "sd" = "sd",
#     "mean" = "mean",
#     "max" = "max",
#     "min" = "min"
#   )
#   results <- list()
#   for (statistic_name in names(aggregate_functions)) {
#     func <- aggregate_functions[[statistic_name]]
#     result <- terra::extract(x, y, fun=func, na.rm=TRUE)
#     names(result) <- paste0(names(result), "_", statistic_name)
#     results[[statistic_name]] <- result[,-1]
#   }
#
#   return(bind_cols(unname(results)))
# }))

quad %>%
  select(quadrat, quadrant, quadrant_key, vals) %>%
  unnest(vals) %>%
  filter(quadrat == "83") %>%
  ggplot(aes(x = saturation, color = quadrant)) +
  geom_freqpoly(bins = 10) +
  theme_minimal()

# should us training set only here
qs <- quad %>%
  select(vals) %>%
  unnest(vals) %>%
  pivot_longer(everything(), names_to = "band", values_to = "val") %>%
  group_by(band) %>%
  summarize(
    mean = mean(val, na.rm = TRUE),
    sd = sd(val, na.rm = TRUE),
    min = min(val, na.rm = TRUE),
    max = max(val, na.rm = TRUE),
    n = sum(!is.na(val)),
    median = median(val, na.rm = TRUE),
    q1 = quantile(val, 0.25, na.rm = TRUE),
    q3 = quantile(val, 0.75, na.rm = TRUE)
  )

quad_stats <- quad %>%
  select(quadrat, quadrant, quadrant_key, vals) %>%
  unnest(vals) %>%
  group_by(quadrant_key) %>%
  summarize(
    across(
      !c(quadrat, quadrant),
      list(
        mean = ~ mean(.x, na.rm = TRUE),
        sd = ~ sd(.x, na.rm = TRUE),
        min = ~ min(.x, na.rm = TRUE),
        max = ~ max(.x, na.rm = TRUE),
        n = ~ sum(!is.na(.x)),
        median = ~ median(.x, na.rm = TRUE),
        q1 = ~ quantile(.x, 0.25, na.rm = TRUE),
        q3 = ~ quantile(.x, 0.75, na.rm = TRUE),
        p1 = ~ {
          q1 <- filter(qs, band == cur_column()) %>% pull(q1)
          q2 <- filter(qs, band == cur_column()) %>% pull(median)
          q3 <- filter(qs, band == cur_column()) %>% pull(q3)
          nn <- sum(!is.na(.x))
          p <- sum(.x < q1, na.rm = TRUE) / nn
          return(p)
        },
        p2 = ~ {
          q1 <- filter(qs, band == cur_column()) %>% pull(q1)
          q2 <- filter(qs, band == cur_column()) %>% pull(median)
          q3 <- filter(qs, band == cur_column()) %>% pull(q3)
          nn <- sum(!is.na(.x))
          p <- sum(.x >= q1 & .x < q2, na.rm = TRUE) / nn
          return(p)
        },
        p3 = ~ {
          q1 <- filter(qs, band == cur_column()) %>% pull(q1)
          q2 <- filter(qs, band == cur_column()) %>% pull(median)
          q3 <- filter(qs, band == cur_column()) %>% pull(q3)
          nn <- sum(!is.na(.x))
          p <- sum(.x >= q2 & .x < q3, na.rm = TRUE) / nn
          return(p)
        },
        p4 = ~ {
          q1 <- filter(qs, band == cur_column()) %>% pull(q1)
          q2 <- filter(qs, band == cur_column()) %>% pull(median)
          q3 <- filter(qs, band == cur_column()) %>% pull(q3)
          nn <- sum(!is.na(.x))
          p <- sum(.x >= q3, na.rm = TRUE) / nn
          return(p)
        }
      )
    )
  )


#ortho_data <- quad |>
#  select(c(quadrant_key,aggregates)) |>
#  unnest(cols = aggregates)

ortho_data <- quad_stats

# Extract quadrats orthomosaic data
corner_points <- tibble(
  quadrat = seq(34, 83, 1) %>% as.character(),
  json_path = sprintf(
    "raw_data/quadrats/quadrat%02d/points.json",
    seq(34, 83, 1)
  )
)

model_name <- 'xgb_model_final'

image_files <- tibble(
  quadrat = list.files(
    glue("clean_data/classified/{model_name}"),
    pattern = ".tif",
    full.names = FALSE
  ) %>%
    str_remove(".tif"),
  classified = list.files(
    glue("clean_data/classified/{model_name}"),
    pattern = ".tif",
    full.names = TRUE
  )
)

#num_image_files <-length(image_files$classified)

#data <- bind_cols(corner_points[1:num_image_files,], image_files) |>
data <- corner_points %>%
  right_join(image_files, by = "quadrat") |>
  #mutate(quadrat = gsub("\\D", "", classified)) |>
  rowwise() |>
  mutate(polygon = list(label_me_points_json_to_sf(json_path))) |>
  ungroup() |>
  unnest(cols = polygon)

geometry <- data |>
  dplyr::select(quadrat, corner, points) |>
  pivot_wider(
    names_from = corner,
    values_from = points,
    names_prefix = "corner_"
  )

image_key <- dplyr::select(data, c(quadrat, classified)) |>
  group_by(quadrat) |>
  slice(1) |>
  ungroup()

ground_quadrants <- geometry |>
  rowwise() |>
  mutate(
    zero = create_quadrant_polygon(0, corner_c, corner_i, corner_ii),
    one = create_quadrant_polygon(1, corner_c, corner_i, corner_ii),
    two = create_quadrant_polygon(2, corner_c, corner_i, corner_ii),
    three = create_quadrant_polygon(3, corner_c, corner_i, corner_ii)
  ) |>
  ungroup() |>
  dplyr::select(c(quadrat, zero, one, two, three)) |>
  pivot_longer(
    cols = c(zero, one, two, three),
    names_to = "quadrant",
    values_to = "geometry"
  ) |>
  mutate(quadrant_key = paste(quadrat, quadrant, sep = "_")) |>
  left_join(image_key, by = 'quadrat') |>
  mutate(polys = geometry) |>
  rowwise() |>
  mutate(polys = list(scale_polygon(geometry))) |>
  mutate(is_valid = st_is_valid(polys)) |>
  ungroup() |>
  dplyr::select(-geometry) |>
  nest(polys = c(quadrant, polys, quadrant_key)) |>
  rowwise() |>
  mutate(polys = list(st_as_sf(polys))) |>
  ungroup()


ground_data <- ground_quadrants %>%
  mutate(
    tables = map2(classified, polys, .f = \(x, y) {
      terra::extract(terra::rast(x), vect(y), fun = table)
    })
  ) %>%
  mutate(
    tables = map2(polys, tables, .f = \(x, y) {
      bind_cols(
        x %>% select(quadrant_key) %>% st_drop_geometry(),
        as_tibble(y) %>% select(-ID)
      )
    })
  ) %>%
  unnest(tables) %>%
  rename(dead = `1`, grass = `2`, sand = `3`) %>%
  dplyr::select(c(quadrant_key, dead, grass, sand))


# ground_data_nested <- ground_quadrants |>
#   rowwise() |>
#   mutate(aggregates = list(extract_polygon_pixels(classified, polygon = polys, extent = c(0,1,0,1), include_polygon_info = TRUE))) |>
#   ungroup()
#
#
# ground_data <- bind_rows(ground_data_nested$aggregates) |>
#   rename(class = lyr1) |>
#   dplyr::select(c(class, quadrant_key)) |>
#   group_by(quadrant_key, class) |>
#   summarise(n = n()) |>
#   pivot_wider(
#     names_from = class,
#     values_from = n,
#     names_prefix = "class_"
#     ) |>
#   ungroup()

#names(ground_data) <- c('quadrant_key', 'dead', 'grass', 'sand')

all_data <- ground_data |>
  left_join(ortho_data, by = 'quadrant_key')


#training_multinomial <- ground_data |>
#  left_join(ortho_data, by ='quadrant_key') |>
#  mutate(across(everything(), \(x)(replace_na(x, 0))))

# Save the data
saveRDS(all_data, glue('clean_data/quad-data-{model_name}.rds'))

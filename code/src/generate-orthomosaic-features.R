library(raster)
library(tictoc)
library(sf)
library(terra)
source('code/src/spatial-utils.R')

tic()

ortho_path <- 'raw_data/drone_sitched/ortho.tif'
ortho <- terra::rast(ortho_path)

features <- ortho %>% 
  add_ndvi() %>%
  add_hsv()

aggregate_functions <- list(
  "sd" = "sd",
  "mean" = "mean",
  "max" = "max",
  "min" = "min",
  "median" = "median",
  "q1" = \(x,...) quantile(x, 0.25,...),
  "q3" = \(x,...) quantile(x, 0.75,...)
)

aggregate_features <- lapply(names(aggregate_functions), function(stat_name) {
  result <- aggregate(features, fact = 5, fun = aggregate_functions[[stat_name]], na.rm = TRUE)
  names(result) <- paste0(names(result), "_", stat_name)
  result
})

names(aggregate_features) <- names(aggregate_functions)

aggregated_ortho <- terra::rast(unname(aggregate_features))

terra::writeRaster(aggregated_ortho, filename = 'clean_data/aggregated_ortho.tif', overwrite=TRUE)
toc()

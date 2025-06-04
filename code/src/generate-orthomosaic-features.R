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

aggregate_features <- list()

for (statistic_name in names(aggregate_functions)) {
  statistic_name <- 'q3'
  func <- aggregate_functions[[statistic_name]]
  result <- aggregate(features, fact = 6, fun = func, na.rm = TRUE)
  names(result) <- paste0(names(result), "_", statistic_name)
  results[[statistic_name]] <- result
}

aggregated_ortho <- terra::rast(unname(aggregate_features))

terra::writeRaster(aggregated_ortho, filename = 'clean_data/aggregated_ortho.tif', overwrite=TRUE)
toc()

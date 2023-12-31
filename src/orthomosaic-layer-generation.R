library(raster)
library(tictoc)
library(sf)
library(terra)
source('src/spatial-utils.R')

tic()

# Generate features of orthomosaic 
ortho_path <- 'raw_data/ortho.tif'
ortho <- terra::rast(ortho_path)

ortho_with_ndvi <- ortho |> 
  add_ndvi()

contrast <- ortho_with_ndvi |>
  raster::brick() |>
  get_contrast()

o <- c(ortho_with_ndvi, contrast)

## Save raster for later
ortho_ndvi_contrast <- o
terra::writeRaster(ortho_ndvi_contrast, 'clean_data/ortho_ndvi_contrast.tif')

## Aggregate everything using the c++ calls (much faster)
# sd <- aggregate(o, fact = 6, fun = "sd", na.rm = TRUE)
# sd_original_names <- names(sd)
# names(sd) <- paste0(sd_original_names, "_", 'sd')
# 
# mean <- aggregate(o, fact = 6, fun = "mean", na.rm = TRUE)
# mean_original_names <- names(mean)
# names(mean) <- paste0(mean_original_names, "_", 'mean')
# 
# max <- aggregate(o, fact = 6, fun = "max", na.rm = TRUE)
# max_original_names <- names(max)
# names(max) <- paste0(max_original_names, "_", 'max')
# 
# min <- aggregate(o, fact = 6, fun = "min", na.rm = TRUE)
# min_original_names <- names(min)
# names(min) <- paste0(min_original_names, "_", 'min')
# processed_ortho <- c(mean, sd, max, min)

aggregate_functions <- list(
  "sd" = "sd",
  "mean" = "mean",
  "max" = "max",
  "min" = "min"
)

results <- list()

# Loop over the aggregate functions
for (statistic_name in names(aggregate_functions)) {
  func <- aggregate_functions[[statistic_name]]
  result <- aggregate(o, fact = 6, fun = func, na.rm = TRUE)
  names(result) <- paste0(names(result), "_", statistic_name)
  results[[statistic_name]] <- result
}

processed_ortho <- terra::rast(unname(results))

terra::writeRaster(processed_ortho, filename = 'clean_data/processed_ortho.tif', overwrite=TRUE)
toc()

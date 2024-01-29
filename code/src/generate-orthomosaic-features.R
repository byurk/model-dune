library(raster)
library(tictoc)
library(sf)
library(terra)
source('src/spatial-utils.R')

tic()

ortho_path <- 'raw_data/ortho.tif'
ortho <- terra::rast(ortho_path)

ortho_with_ndvi <- ortho |> 
  add_ndvi()

contrast <- ortho_with_ndvi |>
  raster::brick() |>
  get_contrast(layer = 7L, window = 5L)

o <- c(ortho_with_ndvi, contrast)

## Save raster for later
ortho_ndvi_contrast <- o
terra::writeRaster(ortho_ndvi_contrast, 'clean_data/ortho_ndvi_contrast.tif')


aggregate_functions <- list(
  "sd" = "sd",
  "mean" = "mean",
  "max" = "max",
  "min" = "min"
)

results <- list()

for (statistic_name in names(aggregate_functions)) {
  func <- aggregate_functions[[statistic_name]]
  result <- aggregate(o, fact = 6, fun = func, na.rm = TRUE)
  names(result) <- paste0(names(result), "_", statistic_name)
  results[[statistic_name]] <- result
}

processed_ortho <- terra::rast(unname(results))

terra::writeRaster(processed_ortho, filename = 'clean_data/processed_ortho.tif', overwrite=TRUE)
toc()

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

ortho_with_contrast <- ortho_with_ndvi |>
  raster::brick() |>
  add_contrast()

o <- ortho_with_contrast

## Aggregate everything
sd <- aggregate(o, fact = 6, fun = "sd", na.rm = TRUE)
mean <- aggregate(o, fact = 6, fun = "mean", na.rm = TRUE)
max <- aggregate(o, fact = 6, fun = "max", na.rm = TRUE)
min <- aggregate(o, fact = 6, fun = "min", na.rm = TRUE)

combined_raster <- c(mean, sd, max, min)

# Rename layers appropriately
original_names <- names(o)

num_bands <- nlyr(o)
stats <- c("sd", "mean", "max", "min")
new_names <- paste0(rep(original_names, times = length(stats)), '_', rep(stats, each = num_bands))

names(combined_raster) <- new_names

terra::writeRaster(combined_raster, filename = 'clean_data/processed_ortho.tif')
toc()
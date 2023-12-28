library(sf)
library(tictoc)
library(glue)
library(furr)
library(tidymodels)

# Load in the model and classify all images
directories <- sprintf("raw_data/quadrats/quadrat%02d/", seq(34, 83, 1)) 
model_path <- 'xgb_fit.rds'

out_directory <- glue('clean_data/{model_name}/')
dir.create(out_directory)


classify_image <- function(feature_paths, model, out_path){
  
  # We will save the classified images in a folder named after the model name
  model_path <- 'xgb_fit.rds'
  model_name  <- str_split(model_path, '.rds')[[1]][1]
  
  
  model <- readRDS(glue('clean_data/{model_path}'))
  
  quadrat_34 <- directories[1]
  
  quadrat_number <- gsub("\\D", "", quadrat_34)
  
  out_path <- glue('clean_data/{model_name}/{quadrat_number}.tif')
  
  features <- paste0(quadrat_34,list.files(quadrat_34, pattern = '.tif'))
  
  raster <- features |>
    lapply(function(x){
      image <- terra::rast(x)
      ext(image) <- c(0,1,0,1)
      return(image)
    })
  
  raster |>
    terra::predict(model, fun = function(x){ pull(x, .pred_class) |> as.numeric() }, filename = out_path, overwrite = TRUE)
  
}






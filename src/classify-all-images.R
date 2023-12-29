library(sf)
library(tictoc)
library(glue)
library(furrr)
library(stringr)
library(tidymodels)
library(terra)

#define function to classify all images
classify_image <- function(feature_paths, model_name, model, out_path = NULL){
  feature_paths <- directories[1]
  
  if(is.null(out_path)){
    quadrat_number <- gsub("\\D", "", feature_paths)
    out_path <- glue('clean_data/classified/{model_name}/{quadrat_number}.tif')
  }
  
  features <- list.files(feature_paths, pattern = '.tif', full.names = TRUE)
  
  layers <- features |>
    lapply(\(x){
      image <- terra::rast(x)
      ext(image) <- c(0,1,0,1)
      return(image)
    }) |> 
    terra::rast()
  
  layers |>
    terra::predict(model, fun = \(...){ pull(predict(...), .pred_class) |> as.numeric() }, filename = out_path, overwrite = TRUE)
  
}

# Load in the model and classify all images
directories <- sprintf("raw_data/quadrats/quadrat%02d", seq(34, 83, 1)) 

model_name <- 'xgb_fit'
model_path <- glue('clean_data/{model_name}.rds')
model <- readRDS(model_path)

out_directory <- glue('clean_data/classified/{model_name}/')

if(!dir.exists(out_directory)){
  dir.create(out_directory)
}

#dir <- directories[1]
#classify_image(dir, model_name, model)

plan(multicore, workers = 2)

 directories |>
   future_map(\(x){classify_image(x, model_name, model)})

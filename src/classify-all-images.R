library(sf)
library(tictoc)
library(glue)
library(tidymodels)

# Load in the model and classify all images
directories <- sprintf("raw_data/quadrats/quadrat%02d/", seq(34, 83, 1)) 

# We will save the classified images in a folder named after the model name
model_path <- 'xgb_fit.rds'
model_name  <- str_split(model_path, '.rds')[[1]][1]

out_directory <- glue('clean_data/{model_name}/')

model <- readRDS(glue('clean_data/{model_path}'))

quadrat_34 <- directories[1]

features <- paste0(quadrat_34,list.files(quadrat_34, pattern = '.tif'))


raster <- features |>
  lapply(FUN = function(x){
    image <- terra::rast(x)
    ext(image) <- c(0,1,0,1)
    return(image)
  })

quad_pred <- function(q_dir){
  
  out_name <- q_dir |>
    str_remove("raw_data/quadrats") |>
    paste0("clean_data/quadrats_new", . , "/pred_", model_name, ".tif")
  
  q_files <- q_dir |>
    list.files(full.names = TRUE, pattern = "*.tif")
  
  q_rast <- q_files |>
    lapply(rast_read_set_ext) |>
    terra::rast()
  
  names(q_rast) <- str_replace(q_rast |> names, "\\.(?=\\d$)", "_")
  
  directory <- out_name |> 
    dirname()
  
  if (!dir.exists(directory)) {
    dir.create(directory)
  }
  
  q_rast |> 
    terra::predict(xgb_fit, fun = pred_fun, filename = out_name, overwrite = TRUE)
  
  return("done")
}


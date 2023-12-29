library(tictoc)
library(furrr)
source('src/classify-image.R')


directories <- sprintf("raw_data/quadrats/quadrat%02d", seq(34, 83, 1)) 

model_name <- 'xgb_fit'
model_path <- glue('clean_data/{model_name}.rds')
model <- readRDS(model_path)

out_directory <- glue('clean_data/classified/{model_name}/')

if(!dir.exists(out_directory)){
  dir.create(out_directory)
}

dir <- directories[1]
classify_image(dir, model_name, model)

# plan(multicore, workers = 2)
# 
#  directories |>
#    future_map(\(x){classify_image(x, model_name, model)})

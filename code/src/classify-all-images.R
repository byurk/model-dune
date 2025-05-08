library(tictoc)
source('code/src/spatial-utils.R')

directories <- sprintf("raw_data/quadrats/quadrat%02d", seq(34, 34, 1)) 

model_name <- 'xgb_model_final'
model_path <- glue('outputs/{model_name}.rds')
model <- readRDS(model_path)

out_directory <- glue('clean_data/classified/{model_name}/')

if(!dir.exists(out_directory)){
  dir.create(out_directory)
}

# dir <- directories[1]
# classify_image(dir, model_name, model)

 directories |>
   map(\(x){classify_image(x, model_name, model, overwrite = TRUE)})

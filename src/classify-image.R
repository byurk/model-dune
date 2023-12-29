library(sf)
library(glue)
library(stringr)
library(tidymodels)
library(terra)

#' Classify Images Using a Machine Learning Model
#'
#' This function classifies images located in specified directories using a 
#' provided machine learning model. It outputs the classified images to a specified path.
#'
#' @param feature_path_directory A character string representing the directories where the image files are located.
#' @param model_name A character string representing the name of the model.
#'                   Used in naming the output path.
#' @param model The classification model to be applied to the images. 
#'              This should be a pre-trained machine learning model object.
#' @param out_path Optional: A character string specifying the path where the 
#'                 classified images will be saved. If `NULL`, a default path is 
#'                 constructed based on `model_name` and a quadrat number 
#'                 extracted from `feature_paths`.
#' @param overwrite Optional: A boolean value if the classified image should be overwritten if it already exists. Default to FALSE
#' 
#' @return Outpath of the resulting classified image
#' @export
#'
#' @examples
#' # Example usage (assuming appropriate model and file paths are provided)
#' classify_image(feature_path_directory = "path/to/images", 
#'                model_name = "my_model", 
#'                model = my_pretrained_model)
classify_image <- function(feature_path_directory, model_name, model, out_path = NULL, overwrite = FALSE){

  if(is.null(out_path)){
    quadrat_number <- gsub("\\D", "", feature_path_directory)
    out_path <- glue('clean_data/classified/{model_name}/{quadrat_number}.tif')
  }

  features <- list.files(feature_path_directory, pattern = '.tif', full.names = TRUE)

  layers <- features |>
    lapply(\(x){
      image <- terra::rast(x)
      ext(image) <- c(0,1,0,1)
      return(image)
    }) |>
    terra::rast()

  layers |>
    terra::predict(model, fun = \(...){ pull(predict(...), .pred_class) |> as.numeric() }, filename = out_path, overwrite = overwrite)
  return(out_path)
}


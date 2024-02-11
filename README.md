# Modeling surface composition of sand dunes

## Getting started 

1. Clone the github repository to your local machine.

```bash
git clone git@github.com:byurk/model-dune.git
```

2. Open up the project by navigating inside the directory and click ```model-dune.Rproj```.

3. Install the ```renv``` package to manage the project dependencies.

```R
install.packages("renv")
library(renv)
```
4. Run the command ```renv::restore()``` to install the project dependencies.

Done! Now you should be able to run the code in the project.


## Project structure

The project is organized into the following directories:

1. code - Contains the modeling code in R scripts and quarto documents.

2. raw_data - Contains the features/images for the ground-based model and orthomosaic data for the drone-based model.

3. clean_data - Contains the classified images, the extracted pixels for training data, features for the orthomosaic.

4. outputs - Vegetation density map of the dune complex.


You should either create the raw_data, clean_data, and outputs directories and/or create symlinks to these directories (probably  somewhere on Google Drive).



## Workflow

1. The training data for the ground-based model was extracted using [labelme](https://github.com/labelmeai/labelme)
    - In the 50 RGB photographs (the 50 quadrats) polygons were drawn of the different surface compositions (sand, grass, dead vegetation, etc.) Each quadrat directory has a ```polygons.json``` file with the associated geometries and class labels.
    - The script ```code/src/sample-all-ground-pixels.R``` extracts the pixels from the polygons and saves them as a ```.rds``` file in the ```clean_data``` directory.


2. The ground-based model performs pixel based classification in the ground based imagery as grass, dead vegetation, and sand. The quarto document ```code/ground-based-model.qmd``` contains the code for this model.

 - The model chosen is an xGBoost model, but any model can be used.
 - Save the workflow and model (using tidymodel conventions) in ```clean_data```.
 - Run the script ```code/src/classify-all-images.R``` to classify all the images in the ```raw_data``` directory and save the classified images in the ```clean_data``` directory. This will save the classified quadrats inside a folder named after the model.
 

3. Once a model is selected and the quadrats are classified, the drone-based model can be trained. The drone-based model uses the classified quadrats to predict the surface composition of the entire orthomosaic map. The quarto document ```code/drone-based-model.qmd``` contains the code for this model.

 - The model chosen is a multinomial regression with regularization, but any model can be used.
 - First run ```code/src/generate-orthomsaic-features.R``` to extract the features from the orthomosaic and save them in the ```clean_data``` directory.
 - Fit the multinomial regression model using the features.
 - Save the workflow and model (using tidymodel conventions) in ```clean_data```.
 - Run the script ```code/src/predict-orthomosaic.R``` to predict the surface composition of the entire orthomosaic map and save the map in the ```outputs``` directory.


## Main entry points:

### 1. exploration.qmd

Explore the training/testing data, follow single photograph (quadrat) through the ground-based model and the drone-based model.

### 2. ground-based-model.qmd

Pixel based classification of \~50 close up photographs of Saugatuck Harbor Natural Area (SHNA) sand dune.

### 3. drone-based-model.qmd

Multinomial regression with regularization predicting surface composition estimates from the ground-based model using multispectral drone imagery of SHNA.

### 4. map-creation.qmd

Predicting surface composition across the entire stitched orthomosiac map of SHNA.

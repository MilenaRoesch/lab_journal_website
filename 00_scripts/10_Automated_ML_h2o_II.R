library(h2o)
library(tidyverse)
library(readxl)
library(rsample)
library(recipes)
library(PerformanceAnalytics)

# Load data
product_backorders_tbl          <- read_csv("./00_data/10_automated_ML_h2o_II/product_backorders.csv")

# Split data into test and train data
set.seed(seed = 1113)
split_obj                       <- rsample::initial_split(product_backorders_tbl, prop = 0.85)
train_readable_tbl              <- training(split_obj)
test_readable_tbl               <- testing(split_obj)

# Recipe
recipe_obj <- recipe(went_on_backorder ~., data = train_readable_tbl) %>% 
  step_zv(all_predictors()) %>% 
  prep()

# Bake Recipe
train_tbl <- bake(recipe_obj, new_data = train_readable_tbl)
test_tbl  <- bake(recipe_obj, new_data = test_readable_tbl)

# Modeling
h2o.init()

# Split data into a training and a validation data frame
# Setting the seed is just for reproducability
split_h2o <- h2o.splitFrame(as.h2o(train_tbl), ratios = c(0.85), seed = 1234)
train_h2o <- split_h2o[[1]]
valid_h2o <- split_h2o[[2]]
test_h2o  <- as.h2o(test_tbl)

# Set the target and predictors
y <- "went_on_backorder"
x <- setdiff(names(train_h2o), y)

automl_models_h2o <- h2o.automl(
  x = x,
  y = y,
  training_frame    = train_h2o,
  validation_frame  = valid_h2o,
  leaderboard_frame = test_h2o,
  max_runtime_secs  = 30,
  nfolds            = 5 
)

typeof(automl_models_h2o)

slotNames(automl_models_h2o)

automl_models_h2o@leaderboard

# Depending on the algorithm, the output will be different
h2o.getModel("StackedEnsemble_AllModels_AutoML_20210129_132920")

# Extracts and H2O model name by a position so can more easily use h2o.getModel()
extract_h2o_model_name_by_position <- function(h2o_leaderboard, n = 1, verbose = T) {
  
  model_name <- h2o_leaderboard %>%
    as_tibble() %>%
    slice(n) %>%
    pull(model_id)
  
  if (verbose) message(model_name)
  
  return(model_name)
  
}

automl_models_h2o@leaderboard %>% 
  extract_h2o_model_name_by_position(6) %>% 
  h2o.getModel()

#Save model
h2o.getModel("StackedEnsemble_AllModels_AutoML_20210129_132920") %>%
  h2o.saveModel(path = "04_Model/h2o_models/")

h2o.loadModel("04_Model/h2o_models/StackedEnsemble_AllModels_AutoML_20210129_132920")
stacked_ensemble_h2o <- h2o.loadModel("04_Model/h2o_models/StackedEnsemble_AllModels_AutoML_20210129_132920")
stacked_ensemble_h2o

predictions <- h2o.predict(stacked_ensemble_h2o, newdata = as.h2o(test_tbl))

typeof(predictions)

predictions_tbl <- predictions %>% as_tibble()

deep_learning_h2o <- h2o.loadModel("04_Model/h2o_models/StackedEnsemble_AllModels_AutoML_20210129_132920")
deep_learning_h2o@allparameters



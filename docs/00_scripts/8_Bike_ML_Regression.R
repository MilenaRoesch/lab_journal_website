# Standard
library(tidyverse)
library(dplyr)

# Modeling
library(parsnip)

# Preprocessing & Sampling
library(recipes)
library(rsample)

# Workflows
library(workflows)

# Modeling Error Metrics
library(yardstick)

# Plotting Decision Trees
library(rpart.plot)

# Modeling ----------------------------------------------------------------
bike_orderlines_tbl <- readRDS("00_data/01_bike_sales/02_wrangled_data/bike_orderlines.rds")

model_sales_tbl <- bike_orderlines_tbl %>%
  select(total_price, model, category_2, frame_material) %>%

  group_by(model, category_2, frame_material) %>%
  summarise(total_sales = sum(total_price)) %>%
  ungroup() %>%

  arrange(desc(total_sales))

model_sales_tbl %>%
  mutate(category_2 = as_factor(category_2) %>%
           fct_reorder(total_sales, .fun = max) %>%
           fct_rev()) %>%
  ggplot(aes(frame_material, total_sales)) +
  geom_violin() +
  geom_jitter(width = 0.1, alpha = 0.5, color = "#2c3e50") +
  #coord_flip() +
  facet_wrap(~ category_2) +
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M", accuracy = 0.1)) +
  tidyquant::theme_tq() +
  labs(
    title = "Total Sales for Each Model",
    x = "Frame Material", y = "Revenue"
  )

bike_features_tbl <- readRDS("00_data/01_bike_sales/02_wrangled_data/bike_features_tbl.rds")
names(bike_features_tbl) <- str_replace_all(names(bike_features_tbl), c(" " = "_" , "," = "" ))


# Set Shift Lever as alternative input if Rear Derailleur is na
bike_features_tbl$Rear_Derailleur[is.na(bike_features_tbl$Rear_Derailleur)] <- bike_features_tbl$Shift_Lever[is.na(bike_features_tbl$Rear_Derailleur)]
bike_features_tbl <- bike_features_tbl %>% 
  rename(Rear_Derailleur_Shift_Lever = Rear_Derailleur) %>%
  drop_na(Rear_Derailleur_Shift_Lever) %>%
  mutate(id = row_number()) 


# Splitting Data ----
set.seed(seed = 1113)
split_obj <- rsample::initial_split(bike_features_tbl, prop   = 0.80, 
                                    strata = "category_2")

# Check if testing contains all category_2 values
split_obj %>% training() %>% distinct(category_2)
split_obj %>% testing() %>% distinct(category_2)

# Assign training and test data
train_tbl <- training(split_obj)
test_tbl  <- testing(split_obj)

# Recipe
bike_features_rec <- 
  recipe( ~ ., data = train_tbl) %>%
  step_rm(all_predictors(), - Rear_Derailleur_Shift_Lever) %>% 
  step_dummy(all_nominal(), - all_outcomes()) %>%
  prep()

bike_transformed_tbl <- bake(bike_features_rec, new_data = NULL)

# Put in a workflow (example)

bike_features_wflow <-
  workflow() %>%
  add_model(lr_mod) %>%
  add_recipe(bike_features_rec)

bike_features_fit <- 
  bike_features_wflow %>% 
  fit(data = train_data)

bike_features_fit %>% 
  pull_workflow_fit() %>% 
  tidy()


# # 5.0 TESTING THE ALGORITHMS OUT ----
# g1 <- bike_features_tbl %>%  
#   mutate(category_2 = as.factor(category_2) %>% 
#            fct_reorder(price_euro)) %>% 
#   ggplot(aes(category_2, price_euro)) +
#   geom_violin() +
#   geom_jitter(width = 0.1, alpha = 0.5, color = "#2dc6d6") +
#   coord_flip() +
#   facet_wrap(~ frame_material) +
#   scale_y_continuous(labels = scales::dollar_format()) +
#   labs(
#     title = "Unit price_euro for Each Model",
#     y = "", x = "Category 2"
#   )
# 
# 
# # 5.1 NEW MODEL Cyclo-Cross bike with an aluminium frame and a Shimano XT groupset----
# 
# new_cross_country <- tibble(
#   model = "Exceed AL SL new",
#   category_2 = "Cross-Country",
#   frame_material = "aluminium",
#   shimano_dura_ace = 0,
#   shimano_ultegra = 0,
#   shimano_105 = 0,
#   shimano_tiagra = 0,
#   Shimano_sora = 0,
#   shimano_deore = 0,
#   shimano_slx = 0,
#   shimano_grx = 0,
#   Shimano_xt = 1,
#   Shimano_xtr = 0,
#   Shimano_saint = 0,
#   SRAM_red = 0,
#   SRAM_force = 0,
#   SRAM_rival = 0,
#   SRAM_apex = 0,
#   SRAM_xx1 = 0,
#   SRAM_x01 = 0,
#   SRAM_gx = 0,
#   SRAM_nx = 0,
#   SRAM_sx = 0,
#   Campagnolo_potenza = 0,
#   Campagnolo_super_record = 0,
#   shimano_nexus = 0,
#   shimano_alfine = 0
# ) 
# 
# models_tbl <- tibble(
#   model_id = str_c("Model 0", 1:7),
#   model = list(
#     model_01_linear_lm_simple,
#     model_02_linear_lm_complex,
#     model_03_linear_glmnet,
#     model_04_tree_decision_tree,
#     model_05_rand_forest_ranger,
#     model_06_rand_forest_randomForest,
#     model_07_boost_tree_xgboost
#   )
# )
# 
# # Add Predictions
# 
# predictions_new_cross_country_tbl <- models_tbl %>%
#   mutate(predictions = map(model, predict, new_data = new_cross_country)) %>%
#   unnest(predictions) %>%
#   mutate(category_2 = "Cross-Country") %>%
#   left_join(new_cross_country, by = "category_2")
# 
# # Update plot
# 
# g2 <- g1 +
#   geom_point(aes(y = .pred), color = "red", alpha = 0.5,
#              data = predictions_new_cross_country_tbl) +
#   ggrepel::geom_text_repel(aes(label = model_id, y = .pred),
#                            size = 3,
#                            data = predictions_new_cross_country_tbl)
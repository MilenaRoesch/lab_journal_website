library(h2o)
library(recipes)
library(readxl)
library(tidyverse)
library(tidyquant)
library(lime)
library(rsample)
library(dplyr)
library(stringr)

process_hr_data_readable <- function(data, definitions_tbl) {
  
  definitions_list <- definitions_tbl %>%
    fill(...1, .direction = "down") %>%
    filter(!is.na(...2)) %>%
    separate(...2, into = c("key", "value"), sep = " '", remove = TRUE) %>%
    rename(column_name = ...1) %>%
    mutate(key = as.numeric(key)) %>%
    mutate(value = value %>% str_replace(pattern = "'", replacement = "")) %>%
    split(.$column_name) %>%
    map(~ select(., -column_name)) %>%
    map(~ mutate(., value = as_factor(value))) 
  
  for (i in seq_along(definitions_list)) {
    list_name <- names(definitions_list)[i]
    colnames(definitions_list[[i]]) <- c(list_name, paste0(list_name, "_value"))
  }
  
  data_merged_tbl <- list(HR_Data = data) %>%
    append(definitions_list, after = 1) %>%
    reduce(left_join) %>%
    select(-one_of(names(definitions_list))) %>%
    set_names(str_replace_all(names(.), pattern = "_value", 
                              replacement = "")) %>%
    select(sort(names(.))) %>%
    mutate_if(is.character, as.factor) %>%
    mutate(
      BusinessTravel = BusinessTravel %>% fct_relevel("Non-Travel", 
                                                      "Travel_Rarely", 
                                                      "Travel_Frequently"),
      MaritalStatus  = MaritalStatus %>% fct_relevel("Single", 
                                                     "Married", 
                                                     "Divorced")
    )
  
  return(data_merged_tbl)
  
}

# Load Data
employee_attrition_tbl <- read_csv("./00_data/09_automated_ML_h2o/datasets-1067-1925-WA_Fn-UseC_-HR-Employee-Attrition.csv")
definitions_raw_tbl    <- read_excel("./00_data/09_automated_ML_h2o/data_definitions.xlsx", sheet = 1, col_names = FALSE)

employee_attrition_readable_tbl <- process_hr_data_readable(employee_attrition_tbl, definitions_raw_tbl)

# Split into test and train
set.seed(seed = 1113)
split_obj <- rsample::initial_split(employee_attrition_readable_tbl, prop = 0.85)

# Assign training and test data
train_readable_tbl <- training(split_obj)
test_readable_tbl  <- testing(split_obj)

# ML Preprocessing Recipe 
recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>%
  step_zv(all_predictors()) %>%
  step_mutate_at(c("JobLevel", "StockOptionLevel"), fn = as.factor) %>% 
  prep()

train_tbl <- bake(recipe_obj, new_data = train_readable_tbl)
test_tbl  <- bake(recipe_obj, new_data = test_readable_tbl)


# 2. Models ----

h2o.init()

automl_leader <- h2o.loadModel("./04_Model/h2o_models/Business_Case_Employee_Attrition/GLM_1_AutoML_20210130_000114")

# 3. LIME ----

# 3.1 Making Predictions ----

predictions_tbl <- automl_leader %>% 
  h2o.predict(newdata = as.h2o(test_tbl)) %>%
  as.tibble() %>%
  bind_cols(
    test_tbl %>%
      select(Attrition, EmployeeNumber)
  )

test_tbl %>%
  slice(1)

# Multiple Explanations ----

explainer <- train_tbl %>%
  select(-Attrition) %>%
  lime(
    model           = automl_leader,
    bin_continuous  = TRUE,
    n_bins          = 4,
    quantile_bins   = TRUE
  )


explanation <- test_tbl %>%
  slice(1:20) %>%
  select(-Attrition) %>%
  lime::explain(
    explainer = explainer,
    n_labels   = 1,
    n_features = 8,
    n_permutations = 5000,
    kernel_width   = 0.5
  )

explanation <- explanation %>%  
  mutate(case = as.numeric(case)) %>%
  mutate(feature_desc = as.character(feature_desc)) %>%
  mutate(feature_desc2 = gsub("[^a-zA-Z]", "", feature_desc))%>% 
  mutate(feature_desc3 = gsub("[^0-9]", "", feature_desc))%>% 
  select(feature_desc,feature_desc2,feature_desc3,case, everything()) %>% 
  arrange(desc(feature_desc2)) %>% 
  group_by(feature_desc2,feature_desc3,case)%>%
  as.tibble()

ggplot(explanation, aes(x = case, y = fct_inorder(feature_desc), fill =feature_weight)) +
  facet_wrap(~ label) +
  geom_tile() + 
  scale_fill_gradientn(colours = c("#A42A2C", "white", "#2A6AA4"))+
  scale_x_continuous(breaks = seq(0, 20, 1), lim = c(0, 20)) +
  labs(
    title    = "",
    subtitle = "",
    x = "Case", 
    y = "Feature",
    fill = "Feature weight"
  )
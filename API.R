## reading in data
library(tidyverse)
temp_data <- read_csv("diabetes_binary_health_indicators_BRFSS2015.csv", col_names = TRUE)

## subsetting the data
dia_data <- temp_data |>
  select(Diabetes_binary, BMI, Smoker, PhysActivity, Fruits, Veggies, HvyAlcoholConsump) |>
  drop_na() |>
  # turning binary vars into factors
  mutate(Diabetes_binary = factor(Diabetes_binary, levels = c(0, 1), labels = c("no diabetes", "diabetes"))) |>
  mutate(Smoker = factor(Smoker, levels = c(0, 1), labels = c("has not smoked 5 packs", "has smoked 5 packs"))) |>
  mutate(PhysActivity = factor(PhysActivity, levels = c(0, 1), labels = c("no exercise last 30 days", "has exercised last 30 days"))) |>
  mutate(Fruits = factor(Fruits, levels = c(0, 1), labels = c("does not eat fruit daily", "eats fruit daily"))) |>
  mutate(Veggies = factor(Veggies, levels = c(0, 1), labels = c("does not eat veggies daily", "eats veggies daily"))) |>
  mutate(HvyAlcoholConsump = factor(HvyAlcoholConsump, levels = c(0, 1), labels = c("not a heavy drinker", "heavy drinker")))

names_vec <- c("dia_binary","bmi", "smoker", "exercise", "fruits", "veggies", "alcohol")
names(dia_data) <- names_vec

## splitting the data
library(tidymodels)
set.seed(123)
dia_split <- initial_split(dia_data, 0.7)
dia_train <- training(dia_split)
dia_test <- testing(dia_split)

## set up for cross validation
dia_5_fold <- vfold_cv(dia_train, 5)

library(ranger)

## creating the rf recipe
rf_rec <-
  recipe(dia_binary ~ ., data = dia_data) |>
  step_dummy(smoker, exercise, fruits, veggies, alcohol) |>
  step_normalize(all_numeric(), -all_outcomes())

## defining the rf model
rf_spec <- rand_forest(mtry = tune()) |>
  set_engine("ranger", importance = "impurity") |>
  set_mode("classification")

## creating the rf workflow
rf_wkf <- workflow() |>
  add_recipe(rf_rec) |>
  add_model(rf_spec)

## fitting the rf model
rf_fits <- rf_wkf |> 
  tune_grid(resamples = dia_5_fold,
            grid = 5,
            metrics = metric_set(mn_log_loss))

## selecting the best model
rf_best_params <- rf_fits |>
  select_best(metric = "mn_log_loss")

## fitting best model to entire data set
best_model <- rf_wkf |>
  finalize_workflow(rf_best_params) |>
  fit(dia_data)


## creating API endpoints
library(plumber)

## setting default values for pred endpoint
default_vals <- list(pred1 = mean(dia_data$bmi),
                     pred2 = names(which.max(table(dia_data$smoker))),
                     pred3 = names(which.max(table(dia_data$exercise))),
                     pred4 = names(which.max(table(dia_data$fruits))),
                     pred5 = names(which.max(table(dia_data$veggies))),
                     pred6 = names(which.max(table(dia_data$alcohol))))

## pred endpoint
#* @param pred1
#* @param pred2
#* @param pred3
#* @param pred4
#* @param pred5
#* @param pred6
#* @get /pred
function(pred1 = default_vals$pred1,
         pred2 = default_vals$pred2,
         pred3 = default_vals$pred3,
         pred4 = default_vals$pred4,
         pred5 = default_vals$pred5,
         pred6 = default_vals$pred6) {
  
  input_data <- tibble(
    bmi = as.numeric(pred1),
    smoker = factor(pred2, levels = c("has not smoked 5 packs", "has smoked 5 packs")),
    exercise = factor(pred3, levels = c("no exercise last 30 days", "has exercised last 30 days")),
    fruits = factor(pred4, levels = c("does not eat fruit daily", "eats fruit daily")),
    veggies = factor(pred5, levels = c("does not eat veggies daily", "eats veggies daily")),
    alcohol = factor(pred6, levels = c("not a heavy drinker", "heavy drinker"))
  )
  
  predict(best_model, input_data, type = "prob")

}
# query with http://127.0.0.1:6810/pred?pred1=20
# query with http://127.0.0.1:6810/pred?pred1=50&pred2=has%20smoked%205%20packs&pred3=no%20exercise%20last%2030%20days&pred4=does%20not%20eat%20fruit%20daily&pred5=does%20not%20eat%20veggies%20daily&pred6=heavy%20drinker
# query with http://127.0.0.1:6810/pred?pred2=has%20smoked%205%20packs&pred3=no%20exercise%20last%2030%20days



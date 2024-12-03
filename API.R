## reading in data
library(tidyverse)
temp_data <- read_csv("diabetes_binary_health_indicators_BRFSS2015.csv", col_names = TRUE)

## subsetting the data
dia_data <- temp_data |>
  select(Diabetes_binary, BMI, Smoker, PhysActivity, Fruits, Veggies, HvyAlcoholConsump, MentHlth, PhysHlth) |>
  drop_na() |>
  # turning binary vars into factors
  mutate(Diabetes_binary = factor(Diabetes_binary, levels = c(0, 1), labels = c("no diabetes", "diabetes"))) |>
  mutate(Smoker = factor(Smoker, levels = c(0, 1), labels = c("has not smoked 5 packs", "has smoked 5 packs"))) |>
  mutate(PhysActivity = factor(PhysActivity, levels = c(0, 1), labels = c("no exercise last 30 days", "has exercised last 30 days"))) |>
  mutate(Fruits = factor(Fruits, levels = c(0, 1), labels = c("does not eat fruit daily", "eats fruit daily"))) |>
  mutate(Veggies = factor(Veggies, levels = c(0, 1), labels = c("does not eat veggies daily", "eats veggies daily"))) |>
  mutate(HvyAlcoholConsump = factor(HvyAlcoholConsump, levels = c(0, 1), labels = c("not a heavy drinker", "heavy drinker")))

names_vec <- c("dia_binary","bmi", "smoker", "exercise", "fruits", "veggies", "alcohol", "ment_hlth", "phys_hlth")
names(dia_data) <- names_vec

library(tidymodels)

## creating the tree recipe
tree_rec <-
  recipe(dia_binary ~ ., data = dia_data) |>
  step_dummy(smoker, exercise, fruits, veggies, alcohol) |>
  step_normalize(all_numeric(), -all_outcomes())

## defining the tree model
tree_mod <- decision_tree(tree_depth = 11,
                          min_n = 20,
                          cost_complexity = 1e-10) |>
  set_engine("rpart") |>
  set_mode("classification")

## creating the tree workflow
tree_wkf <- workflow() |>
  add_recipe(tree_rec) |>
  add_model(tree_mod)

## fitting best model to entire data set
best_model <- tree_wkf |>
  fit(dia_data)


## creating API endpoints
library(plumber)

## setting default values for pred endpoint
default_vals <- list(pred1 = mean(dia_data$bmi),
                     pred2 = names(which.max(table(dia_data$smoker))),
                     pred3 = names(which.max(table(dia_data$exercise))),
                     pred4 = names(which.max(table(dia_data$fruits))),
                     pred5 = names(which.max(table(dia_data$veggies))),
                     pred6 = names(which.max(table(dia_data$alcohol))),
                     pred7 = mean(dia_data$ment_hlth),
                     pred8 = mean(dia_data$phys_hlth))

## pred endpoint
#* @param pred1
#* @param pred2
#* @param pred3
#* @param pred4
#* @param pred5
#* @param pred6
#* @param pred7
#* @param pred8
#* @get /pred
function(pred1 = default_vals$pred1,
         pred2 = default_vals$pred2,
         pred3 = default_vals$pred3,
         pred4 = default_vals$pred4,
         pred5 = default_vals$pred5,
         pred6 = default_vals$pred6,
         pred7 = default_vals$pred7,
         pred8 = default_vals$pred8) {
  
  input_data <- tibble(
    bmi = as.numeric(pred1),
    smoker = factor(pred2, levels = c("has not smoked 5 packs", "has smoked 5 packs")),
    exercise = factor(pred3, levels = c("no exercise last 30 days", "has exercised last 30 days")),
    fruits = factor(pred4, levels = c("does not eat fruit daily", "eats fruit daily")),
    veggies = factor(pred5, levels = c("does not eat veggies daily", "eats veggies daily")),
    alcohol = factor(pred6, levels = c("not a heavy drinker", "heavy drinker")),
    ment_hlth = as.numeric(pred7),
    phys_hlth = as.numeric(pred8)
  )
  
  predict(best_model, input_data, type = "class")
  predict(best_model, input_data, type = "prob")

}
# query with http://127.0.0.1:4884/pred?pred1=20
# query with http://127.0.0.1:4884/pred?pred1=50&pred2=has%20smoked%205%20packs&pred3=has%20exercised%20last%2030%20days&pred4=eats%20fruit%20daily&pred5=eats%20veggies%20daily&pred6=not%20a%20heavy%20drinker&pred7=30&pred8=30
# query with http://127.0.0.1:4884/pred?pred2=has%20smoked%205%20packs&pred3=no%20exercise%20last%2030%20days

## info endpoint
#* @get /info
function() {
  "My name is Jack Wetzel. My Github pages site can be found at https://jackwetz7.github.io/st558-finalprj/EDA.html"
}
# query with http://127.0.0.1:4884/info

## confusion endpoint
#* @get /confusion
function() {
  
  final_pred <- predict(best_model, dia_data, type = "class")
  
  cm_df <- tibble(dia_binary = dia_data$dia_binary,
                  pred_class = as.factor(final_pred$.pred_class))
  
  cm <- conf_mat(data = cm_df,
                 truth = dia_binary,
                 estimate = pred_class)
  
  a <- autoplot(cm, type = "mosaic")
  
  print(a)
}
# query with http://127.0.0.1:4884/confusion
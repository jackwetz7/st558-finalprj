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



library(GGally)
library(leaflet)

#Send a message
#* @get /readme
function(){
  "This is our basic API"
}

#http://localhost:PORT/readme


#* Find natural log of a number
#* @param num Number to find ln of
#* @get /ln
function(num){
  log(as.numeric(num))
}

#query with http://localhost:PORT/ln?num=1

#* Find multiple of two numbers
#* @param num1 1st number
#* @param num2 2nd number
#* @get /mult
function(num1, num2){
  as.numeric(num1)*as.numeric(num2)
}

#query with http://localhost:PORT/mult?num1=10&num2=20

#* Plot of iris data
#* @serializer png
#* @param type base or ggally
#* @param color TRUE or FALSE (only for ggally)
#* @get /plotiris
function(type = "base", color = FALSE){
  if(tolower(type) == "ggally"){
    if(color){
      a <- GGally::ggpairs(iris, aes(color = Species))
      print(a)
    } else {
      a <- GGally::ggpairs(iris)
      print(a)
    }
  } else {
    pairs(iris)
  }
}
#http://localhost:PORT/plotiris?type=ggally


#* Plotting widget
#* @serializer htmlwidget
#* @param lat latitude
#* @param lng longitude
#* @get /map
function(lng = 174.768, lat = -36.852){
  m <- leaflet::leaflet() |>
    addTiles() |>  # Add default OpenStreetMap map tiles
    addMarkers(as.numeric(lng), as.numeric(lat))
  m  # Print the map
}

#query with http://localhost:PORT/map?lng=174&lat=-36


# Choose a predictor
#* @param predictor
#* @get /pred
function(predictor) {
  data <- iris
  if (is.numeric(data[[predictor]])) {
    value <- mean(data[[predictor]])
    message <- paste("The mean of", predictor, "is", value)
    return(message)
  } else if (predictor == "Species") {
    table <- table(data[[predictor]])
    return(paste0(names(table), ": ", table))
  } else {
    stop("Invalid predictor.")
  }
}
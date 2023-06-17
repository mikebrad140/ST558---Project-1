# ST558 Project 1
# Programmer: Michael Bradshaw
# Date: 6/15/23

#packages to load:
library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)
library(ggplot2)


# Function to query the Spoonacular API and find a set recipes that we want to explore:
getRecipes <- function(apiKey, query, cuisine, diet, includeIngredients, excludeIngredients, maxReadyTime, number) {
  base_url <- "https://api.spoonacular.com/recipes/complexSearch"
  
  # Create a list of query parameters
  query_params <- list(
    apiKey = apiKey,
    query = query,
    cuisine = cuisine,
    diet = diet,
    includeIngredients = includeIngredients,
    excludeIngredients = excludeIngredients,
    maxReadyTime = maxReadyTime,
    number = number
  )

  # Send the GET request to Spoonacular
  response <- GET(url = base_url, query = query_params)

  # Extract the response content and parse it into a data frame
  content <- content(response, "text")
  parsed_data <- fromJSON(content)
  
  # Return the parsed data frame
  return(parsed_data$results)
}

# From our set of recipes, pull key information for further analysis.
getRecipeById <- function(recipe_id) {
  # API endpoint URL
  url <- paste0("https://api.spoonacular.com/recipes/", recipe_id, "/information")
  
  # API request
  response <- GET(url, query = list(apiKey = apiKey))
  
  # Parse the response data
  data <- content(response, as = "text")
  parsed_data <- fromJSON(data)
  
  # Return the parsed recipe data
  return(parsed_data)
}

# pull the taste scores for each recipe
getRecipeTasteById <- function(recipe_id) {
  # API endpoint URL
  url <- paste0("https://api.spoonacular.com/recipes/", recipe_id, "/tasteWidget.json")
  
  # API request to taste 
  response <- GET(url, query = list(apiKey = apiKey))
  
  # Parse the response data
  data <- content(response, as = "text")
  parsed_data <- fromJSON(data)
  
  # Return the parsed taste data
  return(parsed_data)
}

#Get Nutrition Information
getRecipeNutritionById <- function(recipe_id) {
  # API endpoint URL
  url <- paste0("https://api.spoonacular.com/recipes/", recipe_id, "/nutritionWidget.json")
  
  # API request to get nutrition info
  response <- GET(url, query = list(apiKey = apiKey))
  
  # Parse the response data
  data <- content(response, as = "text")
  parsed_data <- fromJSON(data)
  
  # Return the parsed nutrition data
  return(parsed_data)
}

# Pulling example set of recipes to test functionality
apiKey <- "6495f41712b644cebe48728831b9d5b1"
query <- "pasta"
cuisine <- "Mediterranean"
diet <- "vegetarian"
includeIngredients <- "tomato,cheese"
excludeIngredients <- "eggs"
maxReadyTime <- 45
number <- 50

# set diet to Null 
recipes <- getRecipes(apiKey, query, cuisine, NULL, includeIngredients, excludeIngredients, maxReadyTime, number)

# Nest step: - create data frame from our recipes of interest

# pull together the data
recipe_df1 <- data.frame()

# Iterate over each recipe ID and pull information from different endpoints to create a new dataset
for (recipe_id in recipes$id) {
  # Retrieve recipe information for the current recipe ID
  recipe_data <- getRecipeById(recipe_id)
  
  # Retrieve taste information for the current recipe ID
  taste_info <- getRecipeTasteById(recipe_id)
  
  # Retrieve nutrition information for the current recipe ID
  nutrition_info <- getRecipeNutritionById(recipe_id)
  # Extract non-list objects from the recipe information and add them to the data frame
  recipe_info <- list(
    id = recipe_data$id,
    title = recipe_data$title,
    vegetarian = recipe_data$vegetarian,
    vegan = recipe_data$vegan,
    glutenFree = recipe_data$glutenFree,
    dairyFree = recipe_data$dairyFree,
    veryHealthy = recipe_data$veryHealthy,
    cheap = recipe_data$cheap,
    veryPopular = recipe_data$veryPopular,
    sustainable = recipe_data$sustainable,
    preparationMinutes = recipe_data$preparationMinutes,
    cookingMinutes = recipe_data$cookingMinutes,
    healthScore = recipe_data$healthScore,
    pricePerServing = recipe_data$pricePerServing,
    servings = recipe_data$servings,
    readyInMinutes = recipe_data$readyInMinutes,
    sourceUrl = recipe_data$sourceUrl,
    sweetness = taste_info$sweetness,
    saltiness = taste_info$saltiness,
    sourness = taste_info$sourness,
    bitterness = taste_info$bitterness,
    savoriness = taste_info$savoriness,
    fattiness = taste_info$fattiness,
    spiciness = taste_info$spiciness,
    calories = nutrition_info$calories,
    carbs = nutrition_info$carbs,
    fat = nutrition_info$fat,
    protein = nutrition_info$protein,
    query = query,
    cuisine = cuisine,
    diet = diet,
    maxReadyTime = maxReadyTime
  )
  
  # Append the recipe information to the data frame
  recipe_df1 <- rbind(recipe_df1, recipe_info)
}

# Print the resulting data frame
glimpse(recipe_df1)

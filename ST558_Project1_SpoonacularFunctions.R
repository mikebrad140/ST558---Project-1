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




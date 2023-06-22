ST558: Project 1
================
Michael Bradshaw
2023-06-22

### Spoonacular API - To examine the relationships between ingredients, recipes, and nutrition

Let’s begin by communicating with the Spoonacular API (Application
Programming Interface) for food recipes. The URL for accessing the API
is below and functions for requesting different parts of recipe
information are provided.

To access more recipe data from the API, I wrote functions to pull from
different endpoints to capture recipe information, taste information,
and nutrition information. To determine how to request query the
information, you need to read the
(<https://spoonacular.com/food-api/docs>). My functions allows users to
pull recipes by query (keyword), cuisine (i.e. italian, french, etc.),
diet (i.e. vegan, paleo, etc.), the ability to include or exclude
certain ingredients, the maximum time it takes to make the recipe, and
the maximum number of recipes to return (1-100). If you do not want to
specify a certain parameter, you can set it to NULL.

``` r
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
  
  # API request to pull recipe information
  response <- GET(url, query = list(apiKey = apiKey))
  
  # Parse the response data
  data <- content(response, as = "text")
  parsed_data <- fromJSON(data)
  
  # Return the parsed recipe data
  return(parsed_data)
}

# pull the taste scores for each recipe from our query
getRecipeTasteById <- function(recipe_id) {
  # API endpoint URL
  url <- paste0("https://api.spoonacular.com/recipes/", recipe_id, "/tasteWidget.json")
  
  # API request to taste scores
  response <- GET(url, query = list(apiKey = apiKey))
  
  # Parse the response data
  data <- content(response, as = "text")
  parsed_data <- fromJSON(data)
  
  # Return the parsed taste data
  return(parsed_data)
}

#Get Nutrition Information for each recipe from our query
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
```

### This code pulls the recipe ID’s for the specified search parameters. Here we are pulling up to 100 pasta recipes that have cheese as an ingredient and are ready in 45 minutes!

``` r
# Pulling 100 pasta recipes that have cheese that are ready in 45 minutes, 
query <- "pasta"
cuisine <- NULL
diet <- NULL
includeIngredients <- NULL
excludeIngredients <- NULL
maxReadyTime <- 90
number <- 100

# Pulling the relevant recipes based on our search: here we set query, includeIngredients, maxReadyTime, and number - all others to NULL
recipes <- getRecipes(apiKey, query, cuisine, diet, includeIngredients, excludeIngredients, maxReadyTime, number)
```

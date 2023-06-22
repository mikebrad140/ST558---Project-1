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

# Pulling pasta recipes that are ready in 90 minutes
apiKey <- "6495f41712b644cebe48728831b9d5b1"
query <- "pasta"
cuisine <- NULL
diet <- NULL
includeIngredients <- NULL
excludeIngredients <- NULL
maxReadyTime <- 90
number <- 200

# set diet to Null, set include ingredient to null
recipes <- getRecipes(apiKey, query, cuisine, diet, includeIngredients, excludeIngredients, maxReadyTime, number)

# Nest step: - create data frame from our recipes of interest

# pull together the data
recipe_df1 <- data.frame()

# Iterate over each recipe ID and pull information from different endpoints 
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
      # record each cuisine associated with a recipe
      cuisine = paste(unlist(recipe_data$cuisines), collapse = ", "), 
      # record each diet associated with a recipe
      diet = paste(unlist(recipe_data$diets), collapse = ", "), 
      maxReadyTime = maxReadyTime
    )
    # Append the recipe information to the data frame
    recipe_df1 <- rbind(recipe_df1, recipe_info)
}

# Take a look at the resulting data frame
head(recipe_df1, n=5)

# Now lets create some new variables 
recipe_df1 <- recipe_df1 %>% mutate(expensive = ifelse(pricePerServing > quantile(pricePerServing, 0.75), 
                                                       "Yes", "No"),
                                     highHealthScore = ifelse(healthScore > quantile(healthScore, 0.75), 
                                                              "Yes", "No"),
                                     yummyScore = (sweetness + savoriness - bitterness),
                                     yummyCategory = cut(yummyScore, breaks = 
                                                        quantile(yummyScore, probs = c(0, 0.25, 0.5, 0.75, 1)),
                                     labels = c("Yuck", "Not Yummy", "Yummy", "Super Yummy"),
                                     include.lowest = TRUE))

# Create some contingency tables:

#1st table: Expensive vs. Vegetarian
contingency_table1 <- table(recipe_df1$expensive, recipe_df1$vegetarian)
colnames(contingency_table1) <- c("Not Vegetarian", "Vegetarian")
rownames(contingency_table1) <- c("Not Expensive", "Expensive")
contingency_table1

#2nd table: Yummy Score Category by high Health Score
contingency_table2 <- table(recipe_df1$yummyCategory, recipe_df1$highHealthScore)
colnames(contingency_table2) <- c("Not Healthy", "Healthy")
contingency_table2

# 3rd table: gluten Free by very Healthy
contingency_table3 <- table(recipe_df1$glutenFree, recipe_df1$veryHealthy)
colnames(contingency_table3) <- c("Not Very Healthy", "Very Healthy")
rownames(contingency_table3) <- c("Not Gluten Free", "Gluten Free")
contingency_table3

table(recipe_df1$glutenFree, recipe_df1$veryHealthy)

# create numerical summaries for some quantitative variables at each setting of some of your categorical variables

# Summary 1:  pricePerServing by expensive categories:
recipe_df1 %>%
  group_by(expensive) %>%
  summarize(n_recipes = n(),
            mean_price = mean(pricePerServing),
            median_price = median(pricePerServing),
            min_price = min(pricePerServing),
            max_price = max(pricePerServing))

# Summary 2:  Number of calories by Yummy Categories
recipe_df1 %>% 
  mutate(calories = as.numeric(calories)) %>%
  group_by(yummyCategory) %>%
  summarize(n_recipes = n(),
            mean_calories = mean(calories),
            median_calories = median(calories),
            min_calories = min(calories),
            max_calories = max(calories))

# Summary 3: Health score by Dairy Free
recipe_df1 %>%
  group_by(dairyFree) %>%
  summarize(n_recipes = n(),
            mean_healthScore = mean(healthScore),
            median_healthScore = median(healthScore),
            min_healthScore = min(healthScore),
            max_healthScore = max(healthScore))

# Summary 4: Ready in Minutes by Yummy Category
recipe_df1 %>%
  group_by(yummyCategory) %>%
  summarize(n_recipes = n(),
            mean_readyInMinutes = mean(readyInMinutes),
            median_readyInMinutes = median(readyInMinutes),
            min_readyInMinutes = min(readyInMinutes),
            max_readyInMinutes = max(readyInMinutes))


---
title: "House Price Prediction Model - R"
author: "NJUGUNA J.M"
date: "`r Sys.Date()`"
output: html_document
df_print: paged
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

Introduction:
This project builds a predictive model to estimate house prices using R. It demonstrates the use of machine learning techniques, data visualization, and evaluation methods.
The process includes:

  - Generating a synthetic dataset.
  - Data cleaning.
  - Exploratory data analysis (EDA).
  - Feature engineering.
  - Model training and evaluation.
  - Making predictions on new data.

```{r setup, include = FALSE}
## Load Required Libraries
library(tidyverse)
library(caret)
library(corrplot)
library(scales)

# Set seed for reproducibility
set.seed(42)
```

Generating a synthetic dataset:

```{r setup, include = FALSE}
# Generate sample dataset
n_samples <- 1000

# Create data frame
data <- data.frame(
  square_feet = rnorm(n_samples, 2000, 500),
  bedrooms = sample(1:5, n_samples, replace = TRUE),
  bathrooms = sample(1:3, n_samples, replace = TRUE),
  age = sample(0:49, n_samples, replace = TRUE),
  garage = sample(0:2, n_samples, replace = TRUE)
)

# Generate price
data$price <- 150000 + 
  data$square_feet * 100 +
  data$bedrooms * 15000 +
  data$bathrooms * 20000 -
  data$age * 1000 +
  data$garage * 10000 +
  rnorm(n_samples, 0, 20000)
```

Data cleaning:

```{r setup, include = FALSE}
# Data Cleaning
clean_data <- function(data) {
  # Remove outliers using IQR method
  Q1 <- quantile(data$price, 0.25)
  Q3 <- quantile(data$price, 0.75)
  IQR <- Q3 - Q1
  data <- data[!(data$price < (Q1 - 1.5 * IQR) | data$price > (Q3 + 1.5 * IQR)), ]
  return(data)
}

data <- clean_data(data)
```

Exploratory data analysis (EDA):

```{r setup, include = FALSE}
# Exploratory Data Analysis
perform_eda <- function(data) {
  # Correlation matrix
  cor_matrix <- cor(data)
  corrplot(cor_matrix, method = "color", type = "upper", 
           addCoef.col = "black", number.cex = 0.7)
  
  # Distribution of price
  ggplot(data, aes(x = price)) +
    geom_histogram(bins = 30, fill = "skyblue", color = "black") +
    labs(title = "Distribution of House Prices",
         x = "Price", y = "Count") +
    scale_x_continuous(labels = dollar_format())
}
```

Feature engineering:

``` {r setup, include = FALSE}
# Feature Engineering
engineer_features <- function(data) {
  data$price_per_sqft <- data$price / data$square_feet
  data$total_rooms <- data$bedrooms + data$bathrooms
  data$age_squared <- data$age ^ 2
  return(data)
}

data <- engineer_features(data)
```

Model training and evaluation:

``` {r setup, include = FALSE}

# Model Training
train_model <- function(data) {
  # Prepare features
  features <- c("square_feet", "bedrooms", "bathrooms", "age", "garage",
                "price_per_sqft", "total_rooms", "age_squared")
  
  # Create training control
  train_control <- trainControl(method = "cv", number = 5)
  
  # Split data
  set.seed(42)
  training_index <- createDataPartition(data$price, p = 0.8, list = FALSE)
  train_data <- data[training_index, ]
  test_data <- data[-training_index, ]
  
  # Train model
  model <- train(
    price ~ .,
    data = train_data[, c(features, "price")],
    method = "lm",
    trControl = train_control
  )
  
  # Make predictions
  predictions <- predict(model, test_data)
  
  # Calculate metrics
  rmse <- sqrt(mean((test_data$price - predictions)^2))
  r2 <- cor(test_data$price, predictions)^2
  
  # Print results
  cat("RMSE: $", format(rmse, big.mark = ",", scientific = FALSE), "\n")
  cat("R² Score:", round(r2, 4), "\n\n")
  
  # Feature importance
  importance <- varImp(model)
  print(importance)
  
  return(list(model = model, train_data = train_data))
}

# Train the model
model_results <- train_model(data)
```

Making predictions on new data:

``` {r setup, include = FALSE}
# Function to make new predictions
predict_price <- function(model, new_data) {
  predicted_price <- predict(model, new_data)
  return(predicted_price)
}

# Example prediction
new_house <- data.frame(
  square_feet = 2500,
  bedrooms = 3,
  bathrooms = 2,
  age = 10,
  garage = 2,
  price_per_sqft = 200,
  total_rooms = 5,
  age_squared = 100
)

predicted_price <- predict_price(model_results$model, new_house)
cat("\nPredicted price for new house: $", 
    format(predicted_price, big.mark = ",", scientific = FALSE))
```








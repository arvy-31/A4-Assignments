# Load necessary libraries
library(tidyverse)
library(lmtest)
library(car)

# Load the data
pizza_data <- read.csv("D:/SCMA632__FIRE632/Stats/Assignment/A4/pizza_data.csv")

# Convert relevant columns to factor type
pizza_data$brand <- as.factor(pizza_data$brand)
pizza_data$price <- as.factor(pizza_data$price)

# Check the conversion
str(pizza_data)

# Define a simplified set of attributes and levels
attributes <- list(
  brand = levels(pizza_data$brand),
  price = levels(pizza_data$price)
)

# Create the design matrix
design <- expand.grid(attributes)

# Assuming 'ranking' column represents respondent evaluations
ratings <- pizza_data$ranking

# Combine the design matrix with ratings
conjoint_data <- cbind(design, ratings)

# Convert factors to dummy variables
conjoint_data <- model.matrix(~ . - 1, data = conjoint_data)

# Prepare the data for the linear model
X <- conjoint_data[, -ncol(conjoint_data)]
y <- conjoint_data[, ncol(conjoint_data)]

# Add a constant to the model (intercept)
X <- cbind(Intercept = 1, X)

# Run the linear model
ca_model <- lm(y ~ X - 1)

# Print the results
summary(ca_model)

# Extract part-worth utilities (coefficients)
utilities <- coef(ca_model)

# Plot the part-worth utilities
utilities_df <- data.frame(Attributes = names(utilities), Utilities = utilities)

ggplot(utilities_df, aes(x = reorder(Attributes, Utilities), y = Utilities)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Part-Worth Utilities", x = "Attributes and Levels", y = "Utility Estimate") +
  theme_minimal()

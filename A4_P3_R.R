# Load necessary libraries
library(ggplot2)
library(scales)
library(MASS)
library(dplyr)

# Load the dataset
file_path <- 'D:/SCMA632__FIRE632/Stats/Assignment/A4/icecream.csv'
icecream_data <- read.csv(file_path)

# Prepare the data
numeric_data <- icecream_data[, -1]  # Assuming the first column is non-numeric (Brand names)
scaled_data <- scale(numeric_data)

# Compute MDS
mds <- isoMDS(dist(scaled_data), k = 2)

# Create a DataFrame for the MDS results
mds_df <- data.frame(Dim1 = mds$points[,1], Dim2 = mds$points[,2])
mds_df$Brand <- icecream_data[, 1]

# Plot the MDS results
ggplot(mds_df, aes(x = Dim1, y = Dim2, label = Brand)) +
  geom_point() +
  geom_text(vjust = -0.5, hjust = -0.5, size = 4) +
  theme_minimal() +
  labs(title = 'MDS of Ice Cream Brands', x = 'Dimension 1', y = 'Dimension 2') +
  xlim(min(mds_df$Dim1) - 0.5, max(mds_df$Dim1) + 0.5) +
  ylim(min(mds_df$Dim2) - 0.5, max(mds_df$Dim2) + 0.5)

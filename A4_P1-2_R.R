# Load necessary libraries
library(tidyverse)
library(cluster)
library(factoextra)
library(psych)
library(GGally)

# Set the file path and read the data
file_path <- "D:/SCMA632__FIRE632/Stats/Assignment/A4/Survey.csv"  # Adjust the path if necessary
data <- read.csv(file_path)

# View the structure of the data
str(data)

# Extract only the numeric columns
numeric_data <- data %>% select_if(is.numeric)

# Handle missing values if any (e.g., by removing rows with NA values or imputing them)
numeric_data <- na.omit(numeric_data)

# Standardize the data
data_scaled <- scale(numeric_data)

# Perform Principal Component Analysis (PCA)
pca_result <- prcomp(data_scaled, scale. = TRUE)
explained_variance <- summary(pca_result)$importance[2, ]
print(paste("Explained variance by component:", explained_variance))

# Plot PCA results
pca_df <- as.data.frame(pca_result$x)
pca_df$City <- data$City

ggplot(pca_df, aes(x = PC1, y = PC2, color = City)) +
  geom_point() +
  labs(title = "PCA Result - Individuals", x = "Principal Component 1", y = "Principal Component 2")

# Perform Factor Analysis
fa_result <- fa(data_scaled, nfactors = 2, rotate = "varimax")
loadings <- fa_result$loadings[, 1:2]
print(paste("Factor loadings:", loadings))

# Plot Factor Analysis results
fa_df <- as.data.frame(loadings)
fa_df$Variable <- rownames(loadings)

ggplot(fa_df, aes(x = MR1, y = MR2)) +
  geom_point() +
  geom_text(aes(label = Variable), hjust = 1.2, vjust = 1.2) +
  labs(title = "Factor Analysis Loadings", x = "Factor 1", y = "Factor 2") +
  theme_minimal()

# Determine the optimal number of clusters using the Elbow Method and Silhouette Method
wss <- map_dbl(2:10, function(k) { kmeans(data_scaled, k, nstart = 10)$tot.withinss })
silhouette_scores <- map_dbl(2:10, function(k) { silhouette(kmeans(data_scaled, k, nstart = 10)$cluster, dist(data_scaled))[, 3] %>% mean })

elbow_df <- tibble(Clusters = 2:10, WSS = wss, Silhouette = silhouette_scores)

ggplot(elbow_df, aes(x = Clusters)) +
  geom_line(aes(y = WSS, color = "WSS")) +
  geom_line(aes(y = Silhouette, color = "Silhouette Score")) +
  labs(title = "Elbow Method and Silhouette Score", y = NULL) +
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "Silhouette Score", labels = scales::percent))

# Perform K-means clustering
optimal_clusters <- 3  # Based on the Elbow Method and Silhouette Score
kmeans_result <- kmeans(data_scaled, centers = optimal_clusters, nstart = 10)
kmeans_labels <- kmeans_result$cluster

# Visualize the clustering result
pca_df$Cluster <- factor(kmeans_labels)

ggplot(pca_df, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point() +
  labs(title = "K-means Clustering Result", x = "Principal Component 1", y = "Principal Component 2") +
  theme_minimal()

# Additional Cluster Analysis - Hierarchical Clustering
dist_matrix <- dist(data_scaled)
hclust_result <- hclust(dist_matrix, method = "ward.D2")

fviz_dend(hclust_result, k = optimal_clusters, rect = TRUE, show_labels = FALSE) +
  labs(title = "Hierarchical Clustering Dendrogram")

# Assign clusters from hierarchical clustering
hierarchical_clusters <- cutree(hclust_result, k = optimal_clusters)
pca_df$Hierarchical_Cluster <- factor(hierarchical_clusters)

# Visualize the hierarchical clustering result
ggplot(pca_df, aes(x = PC1, y = PC2, color = Hierarchical_Cluster)) +
  geom_point() +
  labs(title = "Hierarchical Clustering Result", x = "Principal Component 1", y = "Principal Component 2") +
  theme_minimal()

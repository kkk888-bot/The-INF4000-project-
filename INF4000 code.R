#Data loading and preprocessing Load data and view basic information in the data frame.
library(ggplot2)
library(dplyr)
spotify_data<-read.csv("dataset.csv")
str(spotify_data)
summary(spotify_data)
spotify_data$artists <- strsplit(as.character(spotify_data$artists), ";")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("caret") 
library(ggplot2)
library(dplyr)
library(caret)

# Load and preprocess data
# Read data file ‘dataset.csv’
dataset <- read.csv("dataset.csv")
str(dataset)
summary(dataset)
dataset$track_genre <- as.factor(dataset$track_genre)
dataset_clean <- dataset %>%
  select(-track_id, -track_name, -album_name, -artists)
dataset_clean <- na.omit(dataset_clean)

#Split the training set and the test set
# Split the data set: 80% for the training set, 20% for the test set
set.seed(42)  
train_index <- createDataPartition(dataset_clean$track_genre, p = 0.8, list = FALSE)
train_data <- dataset_clean[train_index, ]
test_data <- dataset_clean[-train_index, ]

install.packages("randomForest")
install.packages("caret")
install.packages("ggplot2")
install.packages("dplyr")
library(randomForest)
library(caret)
library(ggplot2)
library(dplyr)
#4. Train a random forest model
set.seed(42)
rf_model <- randomForest(
  track_genre ~ .,  
  data = train_data,
  ntree = 100,    
  mtry = 3,         
  importance = TRUE 
)
print(rf_model)
plot(rf_model)


library(caret)
y_true <- test_data$track_genre
y_pred <- predict(rf_model, test_data)
conf_matrix <- confusionMatrix(y_pred, y_true)
accuracy <- conf_matrix$overall['Accuracy'] 
class_metrics <- conf_matrix$byClass 
precision <- mean(class_metrics[, "Precision"], na.rm = TRUE)
recall <- mean(class_metrics[, "Recall"], na.rm = TRUE)
f1_score <- mean(class_metrics[, "F1"], na.rm = TRUE)
performance_table <- data.frame(
  Metric = c("Accuracy", "Precision", "Recall", "F1-Score"),
  Value = c(accuracy, precision, recall, f1_score)
)
print(performance_table)


#6. Visual confusion matrix
conf_matrix_data <- as.data.frame.table(conf_matrix$table)
# Plot confusion matrix heatmap
ggplot(conf_matrix_data, aes(Prediction, Reference, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white") +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(
    title = "Confusion Matrix Heatmap",
    x = "Predicted Genre",
    y = "Actual Genre"
  ) +
  theme_minimal()
importance_values <- importance(rf_model)
importance_df <- data.frame(
  Feature = rownames(importance_values), 
  Importance = importance_values[, "MeanDecreaseGini"] 
)
importance_df$Importance <- importance_df$Importance / sum(importance_df$Importance)
print(importance_df)


#Visualisation 4
library(ggplot2)
library(dplyr)
library(tidyr)
data <- data.frame(
  Genre = c("Pop", "Rock", "Classical", "Hip-Hop"),
  Danceability = c(0.8, 0.6, 0.2, 0.9),
  Loudness = c(-5.3, -8.1, -20.0, -3.0),
  Tempo = c(120, 140, 80, 100)
)
data_normalized <- data %>%
  mutate(across(-Genre, ~ scales::rescale(., to = c(0, 1))))
data_long <- data_normalized %>%
  pivot_longer(-Genre, names_to = "Feature", values_to = "Value")
# Create radar chart
ggplot(data_long, aes(x = Feature, y = Value, group = Genre, color = Genre, fill = Genre)) +
  geom_polygon(alpha = 0.2, size = 1) +       
  geom_line(size = 1) +                       
  geom_point(size = 2) +                       
  coord_polar() +                             
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  labs(
    title = "Radar Chart of Music Features by Genre",
    x = "",
    y = "Normalized Value"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
    axis.text.x = element_text(size = 10),                
    panel.grid = element_line(color = "grey80")          
  )

#Visualisation 3
library(ggplot2)
library(reshape2)
data <- data.frame(
  Genre = c("Pop", "Rock", "Classical", "Hip-Hop"),
  Danceability = c(0.8, 0.6, 0.2, 0.9),
  Loudness = c(-5.3, -8.1, -20.0, -3.0),
  Tempo = c(120, 140, 80, 100)
)
data_melt <- melt(data, id.vars = "Genre", variable.name = "Feature", value.name = "Value")
# Draw a heat map
ggplot(data_melt, aes(x = Genre, y = Feature, fill = Value)) +
  geom_tile(color = "white") +  
  scale_fill_gradient(
    low = "lightblue", high = "darkblue", name = "Value"
  ) +  
  labs(
    title = "Heatmap of Music Features Across Genres",
    x = "Genre",
    y = "Features"
  ) +  
  theme_minimal(base_size = 12) +  
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
    axis.text.x = element_text(angle = 45, hjust = 1) 
  )

#Visualisation 2
library(ggplot2)
data <- data.frame(
  Genre = c("Pop", "Rock", "Classical", "Hip-Hop", "Pop", "Rock", "Classical", "Hip-Hop"),
  Danceability = c(0.8, 0.6, 0.2, 0.9, 0.7, 0.65, 0.25, 0.85),
  Loudness = c(-5.3, -8.1, -20.0, -3.0, -5.5, -7.9, -19.5, -2.8)
)
# Draw scatter plot
ggplot(data, aes(x = Danceability, y = Loudness, color = Genre)) +
  geom_point(size = 3, alpha = 0.8) +  
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +  
    title = "Relationship Between Danceability and Loudness",
    subtitle = "Analyzing Trends Across Different Music Genres",
    x = "Danceability (0-1)",
    y = "Loudness (dB)",
    color = "Music Genre"
  ) +
  coord_cartesian() +  
  theme_minimal(base_size = 12) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
    plot.subtitle = element_text(hjust = 0.5, size = 10),  
    legend.position = "bottom"  
  )

#Visualisation 1
library(ggplot2)
data <- data.frame(
  Genre = c("Pop", "Rock", "Classical", "Hip-Hop", "Pop", "Rock", "Classical", "Hip-Hop"),
  Danceability = c(0.8, 0.6, 0.2, 0.9, 0.7, 0.65, 0.25, 0.85)
)

# Draw a box plot
ggplot(data, aes(x = Genre, y = Danceability, fill = Genre)) +
  geom_boxplot(outlier.size = 2, outlier.shape = 16, alpha = 0.8) +  
  labs(
    title = "Danceability Distribution Across Genres",
    subtitle = "Comparing the Danceability of Different Music Genres",
    x = "Music Genre",
    y = "Danceability (0-1)"
  ) +
  scale_fill_brewer(palette = "Set2") +  
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),  
    plot.subtitle = element_text(hjust = 0.5, size = 12),  
    axis.text.x = element_text(angle = 45, hjust = 1),  
    legend.position = "none"  
  )






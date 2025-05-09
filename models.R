library(tidyverse)
library(tidymodels)

tracks_csv <- read_csv('./data/tracks.csv', show_col_types = FALSE)
charts <- read_csv('./data/charts.csv', show_col_types = FALSE)

set.seed(100)

# Filter charts data frame
charts <- charts |>
  filter(year(date) == 2021 | year(date) == 2020)

# Clean tracks data framen
tracks_csv <- tracks_csv |>
  select(-track_id, -...1, -popularity, -album_name) |>
  distinct(artists, track_name, .keep_all = TRUE) |>
  mutate(explicit = as.factor(explicit)) |>
  mutate(track_genre = as.factor(track_genre))



# Filter songs inside top 100
inside <- tracks_csv |>
  filter(track_name %in% charts$song) |>
  mutate(top_100 = 1)


outside <- tracks_csv |>
  filter(!track_name %in% charts$song) |>
  sample_n(2000) |>
  mutate(top_100 = 0) |>
  sample_n(2000) |>
  mutate(top_100 = 0)

# Combine data frames
tracks <- rbind(inside, outside)
print(tracks['top_100'])
tracks <- tracks |> arrange(track_name)
print(tracks)



numeric_cols <- tracks |> 
  select(where(is.numeric)) |> 
  select(-top_100)

cor_df <- cor(numeric_cols, tracks$top_100) |>
  as.data.frame() |>
  rename(correlation = V1) |>
  arrange(desc(abs(correlation)))


removed_features <- cor_df |> 
  filter(abs(correlation) <= 0.01)
removed_features <- rownames(removed_features)
tracks_filtered <- tracks |>
  select(-all_of(removed_features))


print(tracks_filtered)

library(tidymodels) 

tracks_filtered$top_100 <- as.factor(tracks_filtered$top_100)


data_split <- initial_split(tracks_filtered, prop = 0.8, strata = top_100)  # stratify if classification
df_trn <- training(data_split)
df_test <- testing(data_split)

#Try to get better trainign data
df_trn <- df_trn |> 
  group_by(top_100) |> 
  sample_n(min(n())) |> 
  ungroup()

library(dplyr)

test_small <- df_test



ggplot(tracks_filtered, aes(x = top_100, y = danceability, fill = top_100)) +
  geom_boxplot() +
  labs(title = "Danceability vs Top 100 status") +
  theme_minimal()

ggplot(tracks_filtered, aes(x = top_100, y = valence, fill = top_100)) +
  geom_boxplot() +
  labs(title = "valence vs Top 100 status") +
  theme_minimal()

ggplot(tracks_filtered, aes(x = top_100, y = duration_ms, fill = top_100)) +
  geom_boxplot() +
  labs(title = "duration_ms vs Top 100 status") +
  theme_minimal()

ggplot(tracks_filtered, aes(x = top_100, y = explicit, fill = top_100)) +
  geom_boxplot() +
  labs(title = "explicity vs Top 100 status") +
  theme_minimal()

ggplot(tracks_filtered, aes(x = top_100, y = energy, fill = top_100)) +
  geom_boxplot() +
  labs(title = "Energy vs Top 100 status") +
  theme_minimal()

ggplot(tracks_filtered, aes(x = top_100, y = loudness, fill = top_100)) +
  geom_boxplot() +
  labs(title = "Loudness vs Top 100 status") +
  theme_minimal()

ggplot(tracks_filtered, aes(x = top_100, y = speechiness, fill = top_100)) +
  geom_boxplot() +
  labs(title = "Speechiness vs Top 100 status") +
  theme_minimal()

ggplot(tracks_filtered, aes(x = top_100, y = acousticness, fill = top_100)) +
  geom_boxplot() +
  labs(title = "Acousticness vs Top 100 status") +
  theme_minimal()

ggplot(tracks_filtered, aes(x = top_100, y = liveness, fill = top_100)) +
  geom_boxplot() +
  labs(title = "Liveness vs Top 100 status") +
  theme_minimal()

ggplot(tracks_filtered, aes(x = top_100, y = tempo, fill = top_100)) +
  geom_boxplot() +
  labs(title = "Tempo vs Top 100 status") +
  theme_minimal()

ggplot(tracks_filtered, aes(x = top_100, y = time_signature, fill = top_100)) +
  geom_boxplot() +
  labs(title = "Time Signature vs Top 100 status") +
  theme_minimal()



knn_recipe <- recipe(top_100 ~ duration_ms + explicit + danceability + energy +
                       loudness + speechiness + acousticness + liveness +
                       valence + track_genre,
                     data = df_trn) |>
  step_dummy(all_nominal_predictors()) |>  # encode factors like track_genre
  step_normalize(all_numeric_predictors()) 

knn_mdl <- nearest_neighbor(mode = "classification", neighbors = 3)



knn_out <- workflow() |> 
  add_recipe(knn_recipe) |> 
  add_model(knn_mdl) |> 
  fit(data = df_trn)

preds <- predict(knn_out, new_data = test_small) |> 
  bind_cols(test_small)

library(yardstick)

accuracy(preds, truth = top_100, estimate = .pred_class)
cm <- conf_mat(preds, truth = top_100, estimate = .pred_class)


print(metrics(preds, truth = top_100, estimate = .pred_class))


library(yardstick)
library(ggplot2)

#confusion matrix
cm <- conf_mat(preds, truth = top_100, estimate = .pred_class)
cm_df <- as.data.frame(cm$table)

ggplot(cm_df, aes(x = Prediction, y = Truth, fill = Freq)) +
  geom_tile(color = "black", linewidth = 1.2) +
  geom_text(aes(label = Freq), size = 5) +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(
    title = "Confusion Matrix KNN",
    x = "Predicted Class",
    y = "Actual Class",
    fill = "Count"
  ) +
  theme_minimal(base_size = 16) +   theme(
    plot.background = element_rect(fill = "#f0f0f0", color = NA),     # Background outside plot
    panel.background = element_rect(fill = "#f7f7f7", color = NA),    # Background inside plot
    panel.grid.major = element_line(color = "#000050"),               # Gridline color
    panel.grid.minor = element_blank()
  )


probs <- predict(knn_out, new_data = test_small, type = "prob")

roc_data <- bind_cols(test_small, probs)

roc_df <- roc_curve(roc_data, truth = top_100, .pred_0)



ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_path(color = "blue", linewidth = 1.2) +
  geom_abline(linetype = "dashed", color = "gray") +
  coord_equal() +
  labs(
    title = "ROC Curve for KNN",
    x = "False Positive Rate",
    y = "True Positive Rate"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.background = element_rect(fill = "#f0f0f0", color = NA),     # Background outside plot
    panel.background = element_rect(fill = "#f7f7f7", color = NA),    # Background inside plot
    panel.grid.major = element_line(color = "#000050"),               # Gridline color
    panel.grid.minor = element_blank()
  )



library(tidyverse)
library(tidymodels)
<<<<<<< HEAD

=======
>>>>>>> 1118b13279b02f7ba4e64c1cbaddb6f834ef0fed
tracks_csv <- read_csv('./data/tracks.csv', show_col_types = FALSE)
charts <- read_csv('./data/charts.csv', show_col_types = FALSE)


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

# Filter songs outside top 100 & take a random sample
set.seed(100)
outside <- tracks_csv |>
  filter(!track_name %in% charts$song) |>
<<<<<<< HEAD
  sample_n(2000) |>
  mutate(top_100 = 0)


=======
  sample_n(8918) |>
  mutate(top_100 = 0)

>>>>>>> 1118b13279b02f7ba4e64c1cbaddb6f834ef0fed
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

tracks_filtered$duration_ms <- as.factor(tracks_filtered$duration_ms)
tracks_filtered$top_100 <- as.factor(tracks_filtered$top_100)

<<<<<<< HEAD
data_split <- initial_split(tracks_filtered, prop = 0.1, strata = top_100)  # stratify if classification
df_trn <- training(data_split)
df_test <- testing(data_split)

library(dplyr)

test_small <- sample_n(df_test, 1500)



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


metrics(preds, truth = top_100, estimate = .pred_class)
print(metrics)

library(yardstick)
library(ggplot2)

# Compute the confusion matrix
cm <- conf_mat(preds, truth = top_100, estimate = .pred_class)

# Convert to data frame
cm_df <- as.data.frame(cm$table)

print(colnames(cm_df))

ggplot(cm_df, aes(x = Prediction, y = Truth, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), size = 5) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(
    title = "Confusion Matrix",
    x = "Predicted Class",
    y = "Actual Class",
    fill = "Count"
  ) +
  theme_minimal()
=======

knn_recipe <- recipe(top_100 ~ duration_ms, data = tracks_filtered) |> 
  step_normalize(all_numeric_predictors())
knn_mdl <- nearest_neighbor(mode = "classification", neighbors = 3)

knn_out <- workflow() |> 
  add_recipe(knn_recipe) |> 
  add_model(knn_mdl) |> 
  fit(data = tracks_filtered)

>>>>>>> 1118b13279b02f7ba4e64c1cbaddb6f834ef0fed

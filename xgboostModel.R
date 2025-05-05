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
  sample_n(5000) |>
  mutate(top_100 = 0)

# Combine data frames
tracks <- rbind(inside, outside)
print(tracks['top_100'])
tracks <- tracks |> arrange(track_name)




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




library(tidymodels) 

tracks_filtered$top_100 <- as.factor(tracks_filtered$top_100)


data_split <- initial_split(tracks_filtered, prop = 0.8, strata = top_100)  # stratify if classification
df_trn <- training(data_split)
df_test <- testing(data_split)


library(dplyr)


xgb_mdl <- boost_tree(
  mode = "classification",
  trees = 1000,
  tree_depth = 6,
  learn_rate = 0.1,
  loss_reduction = 0.01) |> 
  set_engine("xgboost")

xgb_recipe <- recipe(top_100 ~ duration_ms + explicit + danceability + energy +
                       loudness + speechiness + acousticness + liveness +
                       valence + track_genre,
                     data = df_trn) |> 
  step_dummy(all_nominal_predictors()) |> 
  step_zv() |> 
  step_normalize(all_numeric_predictors())

xgb_wf <- workflow() |> 
  add_recipe(xgb_recipe) |> 
  add_model(xgb_mdl)

xgb_fit <- fit(xgb_wf, data = df_trn)

xgb_preds <- predict(xgb_fit, new_data = df_test, type = "class") |> 
  bind_cols(df_test)

print(xgb_preds)

acc <- accuracy(xgb_preds, truth = top_100, estimate = .pred_class)
print(acc)

cm <- conf_mat(xgb_preds, truth = top_100, estimate = .pred_class)
cm_df <- as.data.frame(cm$table)

ggplot(cm_df, aes(x = Prediction, y = Truth, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), size = 5) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(
    title = "Confusion Matrix XGBoost",
    x = "Predicted Class",
    y = "Actual Class",
    fill = "Count"
  ) +
  theme_minimal()



library(pROC)

xgb_probs <- predict(xgb_fit, new_data = df_test, type = "prob") |> 
  bind_cols(df_test)

# Generate ROC curve data
roc_df <- roc_curve(xgb_probs, truth = top_100, .pred_0)
# Plot ROC
ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_path(color = "blue", linewidth = 1.2) +
  geom_abline(linetype = "dashed", color = "gray") +
  coord_equal() +
  labs(
    title = "ROC Curve for XGBoost Model",
    x = "False Positive Rate",
    y = "True Positive Rate"
  ) +
  theme_minimal()

new_song <- tibble(
  duration_ms = 210000,
  explicit = factor("FALSE", levels = c("FALSE", "TRUE")),
  danceability = 0.75,
  energy = 0.85,
  loudness = -5.0,
  speechiness = 0.045,
  acousticness = 0.02,
  liveness = 0.1,
  valence = 0.6,
  track_genre = factor("pop", levels = levels(df_trn$track_genre))  # match training levels
)

print(predict(xgb_fit, new_data = new_song))

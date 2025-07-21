# --- Setup ---
rm(list=ls())
library(tidyverse)
library(data.table)
library(cluster)
library(factoextra)
library(recipes)
library(pheatmap)
library(randomForest)
library(scales)
library(ggplot2)
library(gridExtra)

# --- Load Data ---
df <- fread("../data/processed/scaled/wimbledon_subset_m_training.csv")

# --- Clean and prepare ---
df_clean <- df %>%
    filter(!is.na(ServeWidth), !is.na(ServeDepth), ServeWidth != "", ServeDepth != "") %>%
    filter(ServeNumber %in% c(1, 2)) %>%
    mutate(
        location_bin = paste0("W", ServeWidth, "_D", ServeDepth),
        ServerName   = tolower(if_else(ServeIndicator == 1, player1, player2)),
        is_ace       = if_else(ServeIndicator == 1, P1Ace, P2Ace)
    )

# --- Helper functions ---
compute_entropy <- function(x) {
    p <- prop.table(table(x))
    -sum(p * log2(p))
}
get_mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}

# --- Build per-player serve profiles ---
get_serve_profiles <- function(df, serve_number_label) {
    df %>%
        filter(ServeNumber == serve_number_label) %>%
        group_by(ServerName) %>%
        summarise(
            avg_speed        = mean(Speed_MPH, na.rm = TRUE),
            sd_speed         = sd(Speed_MPH, na.rm = TRUE),
            win_rate         = mean(PointWinner == ServeIndicator, na.rm = TRUE),
            ace_pct          = mean(is_ace, na.rm = TRUE),
            location_entropy = compute_entropy(location_bin),
            modal_location   = get_mode(location_bin),
            n_serves         = n(),
            .groups          = "drop"
        ) %>%
        filter(n_serves > 20)
}

serve_profiles1 <- get_serve_profiles(df_clean, 1)

# --- Prepare encoded and scaled data for modeling ---
model_data <- serve_profiles1 %>%
    select(-modal_location)

recipe_obj <- recipe(~ ., data = model_data) %>%
    update_role(ServerName, new_role = "id") %>%
    update_role(n_serves, new_role = "id") %>%
    step_center(all_numeric_predictors()) %>%
    step_scale(all_numeric_predictors()) %>%
    prep()

df_encoded <- bake(recipe_obj, new_data = NULL)
df_model <- df_encoded %>%
    column_to_rownames("ServerName") %>%
    select(-n_serves)

# --- Fit Random Forest ---
rf <- randomForest(
    win_rate ~ .,
    data = df_model,
    importance = TRUE
)

imp <- importance(rf, type = 1)
imp_sorted <- sort(imp[,1], decreasing = TRUE)
print(imp_sorted)

rf_weights <- imp_sorted / sum(imp_sorted)

# --- Predict serve score using RF model ---
serve_profiles1$serve_score_rf <- predict(rf, newdata = df_model)

# --- Rescale and compute weighted score ---
features <- names(rf_weights)
rescaled_for_score <- serve_profiles1 %>%
    mutate(across(all_of(features), ~ rescale(.x, to = c(0, 1)), .names = "rescaled_{.col}"))

server_quality <- rescaled_for_score %>%
    rowwise() %>%
    mutate(
        serve_score_rf_weighted = sum(c_across(starts_with("rescaled_")) * rf_weights[features])
    ) %>%
    ungroup() %>%
    arrange(desc(serve_score_rf_weighted)) %>%
    select(ServerName, serve_score_rf, serve_score_rf_weighted, all_of(features), starts_with("rescaled_"), n_serves)

write.csv(server_quality, "../data/results/server_quality_models/server_quality_wimbledon_m_first.csv", row.names = F)

# --- CLUSTERING for feature distribution visualization ---
set.seed(42)
kfit <- kmeans(df_model, centers = 3, nstart = 25)
clustered <- df_model %>%
    mutate(cluster = factor(kfit$cluster),
           ServerName = rownames(df_model)) %>%
    pivot_longer(cols = -c(cluster, ServerName), names_to = "Feature", values_to = "Value")

# --- Plot: Feature Distributions by Cluster ---
p_feat_dist <- ggplot(clustered, aes(x = cluster, y = Value, fill = cluster)) +
    geom_boxplot(outlier.size = 0.5) +
    facet_wrap(~ Feature, scales = "free", ncol = 3) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "none") +
    labs(title = "Feature Distributions by Cluster", x = "Cluster", y = "Standardized Value")

ggsave("../data/results/server_quality_models/feature_distributions_by_cluster.png", p_feat_dist, 
       width = 10, height = 8, bg = "white")

# --- Plot: Random Forest Variable Importance ---
imp_df <- data.frame(
    Variable = rownames(imp),
    Importance = imp[,1]
) %>%
    arrange(desc(Importance))

p_rf_imp <- ggplot(imp_df, aes(x = reorder(Variable, Importance), y = Importance)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    theme_minimal(base_size = 12) +
    labs(title = "Random Forest Variable Importance", x = "Feature", y = "Importance")

ggsave("../data/results/server_quality_models/rf_variable_importance.png", p_rf_imp, 
       width = 7, height = 5, bg = "white")


#--------------------------------------------
# importance metric based on over or underperforming from model (not from just weighted average of stats)
#--------------------------------------------

# --- Build model to predict win_rate using 4 key features ---
baseline_model <- lm(win_rate ~ ace_pct + avg_speed + sd_speed + location_entropy, data = serve_profiles1)
summary(baseline_model)

# --- Predict expected win rate from the model ---
serve_profiles1$expected_win_rate <- predict(baseline_model, newdata = serve_profiles1)

# --- Compute overperformance metric ---
serve_profiles1$overperformance <- serve_profiles1$win_rate - serve_profiles1$expected_win_rate

# --- View and export ---
overperf_summary <- serve_profiles1 %>%
    select(ServerName, win_rate, expected_win_rate, overperformance, ace_pct, avg_speed, sd_speed, location_entropy, n_serves) %>%
    arrange(desc(overperformance))

write.csv(overperf_summary, "../data/results/server_quality_models/server_overperformance.csv", row.names = FALSE)

# --- Optional: Plot overperformance ---
library(ggrepel)

p_overperf <- ggplot(overperf_summary, aes(x = expected_win_rate, y = win_rate)) +
    geom_point(aes(color = overperformance), size = 2.5) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
    geom_text_repel(data = subset(overperf_summary, abs(overperformance) > 0.05),
                    aes(label = ServerName), size = 3) +
    scale_color_gradient2(low = "red", high = "green", mid = "gray90", midpoint = 0) +
    theme_minimal(base_size = 12) +
    labs(
        title = "Serve Overperformance vs Expected Win Rate",
        x = "Expected Win Rate (Model-Based)",
        y = "Actual Win Rate",
        color = "Overperformance"
    )

ggsave("../data/results/server_quality_models/serve_overperformance_plot.png", p_overperf, width = 8, height = 6)

# Install necessary packages if you don't have them:
# install.packages(c("dplyr", "tidyr", "ggplot2", "factoextra", "lubridate", "cluster"))

library(dplyr)
library(tidyr)
library(ggplot2)
library(factoextra)
library(lubridate)
library(cluster)
library(bit64)

# ---------------------------------------------------------
# 1. Load and Inspect Data
# ---------------------------------------------------------
df <- read.csv("regime_data_2000_2025.csv")
df$Date <- as.Date(df$Date)

# ---------------------------------------------------------
# 2. Preprocessing & Feature Engineering
# ---------------------------------------------------------
# Drop the initial NA rows caused by 30-day rolling calculations
df_clean <- df %>% drop_na()

# Select stationary features indicative of 'regimes' (Avoid raw price levels)
features <- df_clean %>%
  select(vix_price, yield_spread, epu_index, gprd_index, 
         sp500_vol_30d, gold_oil_ratio, vix_epu_ratio)

# Scale the features (Z-score normalization)
features_scaled <- scale(features)

# ---------------------------------------------------------
# 3. Determine Optimal Number of Clusters
# ---------------------------------------------------------
# Set seed for reproducibility
set.seed(42)

# Elbow Method Plot
p_elbow <- fviz_nbclust(features_scaled, kmeans, method = "wss") +
  labs(title = "Elbow Method for Optimal k") +
  theme_minimal()
print(p_elbow)

# Silhouette Method Plot (Uncomment to run - can be computationally heavy)
# p_sil <- fviz_nbclust(features_scaled, kmeans, method = "silhouette")
# print(p_sil)

# ---------------------------------------------------------
# 4. K-Means Clustering
# ---------------------------------------------------------
# Assuming the elbow indicates k = 4 regimes
k_optimal <- 4
kmeans_result <- kmeans(features_scaled, centers = k_optimal, nstart = 25)

# Append cluster assignments back to the clean dataset
df_clean$Regime <- as.factor(kmeans_result$cluster)

# Summarize the average feature values for each regime to interpret them
regime_summary <- df_clean %>%
  group_by(Regime) %>%
  summarise(
    Avg_VIX = mean(vix_price),
    Avg_Yield_Spread = mean(yield_spread),
    Avg_EPU = mean(epu_index),
    Avg_GPRD = mean(gprd_index),
    Avg_Gold_Oil = mean(gold_oil_ratio),
    Count = n()
  )
print("Cluster Profiles:")
print(regime_summary)

# ---------------------------------------------------------
# 5. Visualization
# ---------------------------------------------------------

# Plot A: PCA representation of the clusters
p_pca <- fviz_cluster(kmeans_result, data = features_scaled, 
                      geom = "point", ellipse.type = "convex",
                      main = "Macro Regimes Clustered via PCA",
                      palette = "jco", ggtheme = theme_minimal())
print(p_pca)

# Plot B: Historical Timeline mapped to S&P 500 Prices
# This shows exactly WHEN each regime occurred in the real world
p_timeline <- ggplot(df_clean, aes(x = Date, y = sp500_price, color = Regime)) +
  geom_point(size = 1.5, alpha = 0.7) +
  scale_color_brewer(palette = "Set1") +
  theme_minimal() +
  labs(
    title = "S&P 500 Colored by Macroeconomic Regime (2000-2025)",
    subtitle = "Regimes identified via K-Means Clustering on Volatility, Yields, and Risk Indices",
    x = "Year",
    y = "S&P 500 Price",
    color = "Regime Cluster"
  ) +
  theme(legend.position = "bottom")

print(p_timeline)


# ---------------------------------------------------------
# 7. Additional Visualization: Vertical Bar Timeline (Barcode Plot)
# ---------------------------------------------------------
# Make sure ggplot2 and dplyr are loaded
library(ggplot2)
library(dplyr)

# We use geom_col with a constant y-value (1) to create vertical bars that stretch 
# from top to bottom, filling the color based on the Regime for that specific date.
p_barcode <- ggplot(df_clean, aes(x = Date, y = 1, fill = Regime, color = Regime)) +
  # width = 1 ensures the bars touch each other without gaps for daily data
  geom_col(width = 1) + 
  
  # We use both fill and color mapping to prevent tiny white rendering artifacts 
  # between the daily bars
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  
  theme_minimal() +
  labs(
    title = "Macroeconomic Regimes: Vertical Timeline (Barcode Plot)",
    subtitle = "Visualizing regime persistence, stickiness, and phase transitions (2000-2025)",
    x = "Year",
    y = NULL, # We don't need a Y-axis label since it's just a visual strip
    fill = "Regime Cluster"
  ) +
  
  # Clean up the Y-axis since the height (1) has no numerical meaning here
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "bottom",
    # Reduce the aspect ratio to make it look like a proper horizontal strip
    aspect.ratio = 0.2 
  ) +
  # Hide the color legend to avoid duplicating the fill legend
  guides(color = "none") 

print(p_barcode)



# 1. Extract the dates that belong to the Red Regime (Cluster 1)
# We use pull() to get a vector of dates
red_regime_dates <- df_clean %>%
  filter(Regime == 1) %>%
  pull(Date)

# 2. Load the combined_prices dataset
combined_prices <- read.csv("combined_prices.csv")

# Ensure the Date column is in the correct Date format for matching
combined_prices$Date <- as.Date(combined_prices$Date)

# 3. Create the new dataframe containing only Red Regime observations
# This selects all features from combined_prices for the crisis/stress dates
red_regime_df <- combined_prices %>%
  filter(Date %in% red_regime_dates) %>%
  arrange(Date) # Ensure chronological order for time-series splitting later

# 4. Inspect the resulting dataframe
print("Summary of Red Regime Dataframe:")
print(summary(red_regime_df))
head(red_regime_df)

##----------------------------------------------------------##
###Extracting regime cluster 1 (red region as it has largest current portion)
# Load necessary libraries
library(dplyr)
library(readr)

# 1. Load the datasets
regime_data <- read_csv("regime_data_2000_2025.csv")
combined_prices <- read_csv("combined_prices.csv")

# 2. Convert Date columns to Date objects to ensure a clean match
regime_data$Date <- as.Date(regime_data$Date)
combined_prices$Date <- as.Date(combined_prices$Date)

# 3. Handle the 'crude_oil_price' instruction
# We remove 'crude_oil_price' from the regime dataset so that 
# the version in 'combined_prices' is the one that remains after the merge.
regime_data_trimmed <- regime_data %>% 
  select(-crude_oil_price)

# 4. Merge the datasets
# We use an inner_join because combined_prices has fewer rows.
# This will result in a dataset containing only the dates present in both.
merged_data <- inner_join(combined_prices, regime_data_trimmed, by = "Date")

# 5. Review the result
head(merged_data)

# Optional: Save the merged dataset
write_csv(merged_data, "merged_market_data.csv")
View(merged_data)

red_regime_df <- merged_data %>%
  filter(Date %in% red_regime_dates) %>%
  arrange(Date) # Ensure chronological order for time-series splitting later

# 4. Inspect the resulting dataframe
print("Summary of Red Regime Dataframe:")
print(summary(red_regime_df))
head(red_regime_df)
write_csv(red_regime_df, "merged_red_regime_data.csv")
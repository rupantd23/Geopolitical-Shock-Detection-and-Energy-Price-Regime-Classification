# ================================================================================
#  MTH443 — HYDRA Pipeline
#  Script 1 of 3 : Exploratory Data Analysis
#  Run this FIRST before the main pipeline
# ================================================================================
#
#  Required packages (install once):
#  install.packages(c("ggplot2","dplyr","tidyr","corrplot","gridExtra","scales"))
# ================================================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(corrplot)
library(gridExtra)
library(scales)

# ── Paths ─────────────────────────────────────────────────────────────────────
MERGED_PATH    <- "merged_red_regime_data.csv"
RED_TRAIN_PATH <- "red_regime_train.csv"
RED_TEST_PATH  <- "red_regime_test_dec2024.csv"
OUTPUT_DIR     <- "outputs/"
dir.create(OUTPUT_DIR, showWarnings = FALSE)

# ════════════════════════════════════════════════════════════════════════════════
#  SECTION 1 — Load & Filter to Red Regime
# ════════════════════════════════════════════════════════════════════════════════
cat("Loading datasets...\n")

df       <- read.csv(MERGED_PATH,    stringsAsFactors = FALSE)
red_tr   <- read.csv(RED_TRAIN_PATH, stringsAsFactors = FALSE)
red_te   <- read.csv(RED_TEST_PATH,  stringsAsFactors = FALSE)

red_dates <- union(red_tr$Date, red_te$Date)
df        <- df[df$Date %in% red_dates, ]
df$Date   <- as.Date(df$Date)
df        <- df[order(df$Date), ]

# Drop duplicate columns
df$GPRD      <- NULL
df$VIX_price <- NULL

# Create lag features
df$crude_lag1 <- c(NA, head(df$crude_oil_price, -1))
df$crude_lag2 <- c(NA, NA, head(df$crude_oil_price, -2))
df$crude_lag3 <- c(NA, NA, NA, head(df$crude_oil_price, -3))
df$crude_lag5 <- c(rep(NA, 5), head(df$crude_oil_price, -5))
df            <- na.omit(df)

cat(sprintf("  Total rows after cleaning : %d\n", nrow(df)))
cat(sprintf("  Date range                : %s to %s\n",
            min(df$Date), max(df$Date)))

# Train / Test split
last_month <- format(max(df$Date), "%Y-%m")
train_df   <- df[format(df$Date, "%Y-%m") != last_month, ]
test_df    <- df[format(df$Date, "%Y-%m") == last_month, ]

cat(sprintf("  Train rows : %d\n", nrow(train_df)))
cat(sprintf("  Test  rows : %d  (December 2024)\n", nrow(test_df)))

# ════════════════════════════════════════════════════════════════════════════════
#  SECTION 2 — Summary Statistics
# ════════════════════════════════════════════════════════════════════════════════
cat("\n--- Summary Statistics (Training Set) ---\n")

KEY_COLS <- c("crude_oil_price","vix_price","gprd_index",
              "epu_index","sp500_price","gold_price")

summary_stats <- train_df[, KEY_COLS] %>%
  summarise(across(everything(), list(
    Mean   = ~round(mean(.),  2),
    SD     = ~round(sd(.),    2),
    Min    = ~round(min(.),   2),
    Median = ~round(median(.), 2),
    Max    = ~round(max(.),   2)
  ))) %>%
  pivot_longer(everything(),
               names_to  = c("Variable", ".value"),
               names_sep = "_(?=[^_]+$)")

print(summary_stats)
write.csv(summary_stats,
          file.path(OUTPUT_DIR, "eda_summary_stats.csv"),
          row.names = FALSE)

# ════════════════════════════════════════════════════════════════════════════════
#  SECTION 3 — Plot 1: Crude Oil Price Over Time
# ════════════════════════════════════════════════════════════════════════════════
p1 <- ggplot(df, aes(x = Date, y = crude_oil_price)) +
  geom_line(color = "#378ADD", linewidth = 0.6) +
  geom_vline(xintercept = as.numeric(as.Date("2024-12-01")),
             linetype = "dashed", color = "#E24B4A", linewidth = 0.8) +
  annotate("text", x = as.Date("2024-12-01"), y = max(df$crude_oil_price) * 0.95,
           label = "Test period", color = "#E24B4A", hjust = -0.1, size = 3.5) +
  labs(title = "Crude oil price — Red Regime (2001–2024)",
       x = NULL, y = "Price (USD)") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave(file.path(OUTPUT_DIR, "eda_01_crude_oil_timeseries.png"),
       p1, width = 10, height = 4, dpi = 150)
cat("  Saved: eda_01_crude_oil_timeseries.png\n")

# ════════════════════════════════════════════════════════════════════════════════
#  SECTION 4 — Plot 2: Distribution of Key Variables
# ════════════════════════════════════════════════════════════════════════════════
dist_data <- train_df[, KEY_COLS] %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Value")

p2 <- ggplot(dist_data, aes(x = Value)) +
  geom_histogram(fill = "#378ADD", color = "white",
                 bins = 40, alpha = 0.8) +
  facet_wrap(~Variable, scales = "free", ncol = 3) +
  labs(title = "Distribution of key variables (training set)",
       x = NULL, y = "Count") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"))

ggsave(file.path(OUTPUT_DIR, "eda_02_distributions.png"),
       p2, width = 12, height = 7, dpi = 150)
cat("  Saved: eda_02_distributions.png\n")

# ════════════════════════════════════════════════════════════════════════════════
#  SECTION 5 — Plot 3: Correlation Matrix
# ════════════════════════════════════════════════════════════════════════════════
CORR_COLS <- c("crude_oil_price","vix_price","gprd_index","epu_index",
               "sp500_price","gold_price","yield_spread",
               "crude_lag1","crude_lag2","crude_lag3")

cor_mat <- cor(train_df[, CORR_COLS], use = "complete.obs")

png(file.path(OUTPUT_DIR, "eda_03_correlation_matrix.png"),
    width = 900, height = 800, res = 120)
corrplot(cor_mat,
         method   = "color",
         type     = "upper",
         tl.cex   = 0.8,
         tl.col   = "black",
         addCoef.col = "black",
         number.cex  = 0.65,
         col      = colorRampPalette(c("#E24B4A","white","#378ADD"))(200),
         title    = "Correlation matrix — key features",
         mar      = c(0, 0, 2, 0))
dev.off()
cat("  Saved: eda_03_correlation_matrix.png\n")

# ════════════════════════════════════════════════════════════════════════════════
#  SECTION 6 — Plot 4: Stress Features Over Time (VIX, GPRD, EPU)
# ════════════════════════════════════════════════════════════════════════════════
stress_long <- train_df %>%
  select(Date, vix_price, gprd_index, epu_index) %>%
  pivot_longer(-Date, names_to = "Indicator", values_to = "Value") %>%
  mutate(Indicator = recode(Indicator,
    "vix_price"  = "VIX (Fear Index)",
    "gprd_index" = "GPRD (Geopolitical Risk)",
    "epu_index"  = "EPU (Policy Uncertainty)"
  ))

p4 <- ggplot(stress_long, aes(x = Date, y = Value, color = Indicator)) +
  geom_line(linewidth = 0.5, alpha = 0.85) +
  facet_wrap(~Indicator, scales = "free_y", ncol = 1) +
  scale_color_manual(values = c("#378ADD","#E24B4A","#1D9E75")) +
  labs(title = "Stress indicators over time",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none",
        plot.title      = element_text(face = "bold"),
        strip.text      = element_text(face = "bold"))

ggsave(file.path(OUTPUT_DIR, "eda_04_stress_indicators.png"),
       p4, width = 10, height = 8, dpi = 150)
cat("  Saved: eda_04_stress_indicators.png\n")

# ════════════════════════════════════════════════════════════════════════════════
#  SECTION 7 — Plot 5: Crude Oil vs Stress Features (Scatter)
# ════════════════════════════════════════════════════════════════════════════════
p5a <- ggplot(train_df, aes(x = vix_price, y = crude_oil_price)) +
  geom_point(alpha = 0.3, color = "#378ADD", size = 0.8) +
  geom_smooth(method = "loess", color = "#E24B4A", se = FALSE) +
  labs(title = "Crude oil vs VIX", x = "VIX", y = "Crude oil price") +
  theme_minimal(base_size = 11)

p5b <- ggplot(train_df, aes(x = gprd_index, y = crude_oil_price)) +
  geom_point(alpha = 0.3, color = "#1D9E75", size = 0.8) +
  geom_smooth(method = "loess", color = "#E24B4A", se = FALSE) +
  labs(title = "Crude oil vs GPRD", x = "GPRD", y = "Crude oil price") +
  theme_minimal(base_size = 11)

p5c <- ggplot(train_df, aes(x = epu_index, y = crude_oil_price)) +
  geom_point(alpha = 0.3, color = "#BA7517", size = 0.8) +
  geom_smooth(method = "loess", color = "#E24B4A", se = FALSE) +
  labs(title = "Crude oil vs EPU", x = "EPU", y = "Crude oil price") +
  theme_minimal(base_size = 11)

p5d <- ggplot(train_df, aes(x = sp500_price, y = crude_oil_price)) +
  geom_point(alpha = 0.3, color = "#7F77DD", size = 0.8) +
  geom_smooth(method = "loess", color = "#E24B4A", se = FALSE) +
  labs(title = "Crude oil vs S&P 500", x = "S&P 500", y = "Crude oil price") +
  theme_minimal(base_size = 11)

p5 <- grid.arrange(p5a, p5b, p5c, p5d, ncol = 2,
                   top = "Crude oil price vs key predictors")

ggsave(file.path(OUTPUT_DIR, "eda_05_scatter_predictors.png"),
       p5, width = 10, height = 8, dpi = 150)
cat("  Saved: eda_05_scatter_predictors.png\n")

# ════════════════════════════════════════════════════════════════════════════════
#  SECTION 8 — Missing Value Check
# ════════════════════════════════════════════════════════════════════════════════
missing_check <- data.frame(
  Column  = names(df),
  Missing = colSums(is.na(df))
) %>% filter(Missing > 0)

if (nrow(missing_check) == 0) {
  cat("\n  No missing values found after preprocessing.\n")
} else {
  cat("\n  Missing values:\n")
  print(missing_check)
}

cat("\n  EDA complete. All outputs saved to:", OUTPUT_DIR, "\n")

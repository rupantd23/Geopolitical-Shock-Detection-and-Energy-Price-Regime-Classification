library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)

OUTPUT_DIR <- "outputs/"
dir.create(OUTPUT_DIR, showWarnings = FALSE)

# Load saved objects from pipeline
obj        <- readRDS(file.path(OUTPUT_DIR, "hydra_objects.rds"))
y_te       <- obj$y_te
hp         <- obj$hydra_preds
all_preds  <- obj$all_preds
results_df <- obj$results_df
pred_df    <- obj$pred_df
train_df   <- obj$train_df
test_df    <- obj$test_df
regime_tr  <- obj$regime_tr
regime_te  <- obj$regime_te
label_map  <- obj$label_map
soft_tr    <- obj$soft_tr
pca_fit    <- obj$pca_fit
var_exp    <- obj$var_exp

pred_df$Date <- as.Date(pred_df$Date)


# ════════════════════════════════════════════════════════════════════════════════
#  PLOT 1 — Actual vs HYDRA Predicted (Line Chart)
# ════════════════════════════════════════════════════════════════════════════════
p1 <- ggplot(pred_df, aes(x = Date)) +
  geom_line(aes(y = Actual,     color = "Actual"),    linewidth = 1.0) +
  geom_line(aes(y = HYDRA_Pred, color = "HYDRA"),     linewidth = 0.9,
            linetype = "dashed") +
  geom_point(aes(y = Actual,     color = "Actual"),   size = 2.5) +
  geom_point(aes(y = HYDRA_Pred, color = "HYDRA"),    size = 2.5, shape = 17) +
  scale_color_manual(values = c("Actual" = "#378ADD", "HYDRA" = "#1D9E75"),
                     name   = NULL) +
  scale_x_date(date_labels = "%b %d", date_breaks = "3 days") +
  labs(title    = "Actual vs HYDRA predicted — crude oil price (December 2024)",
       subtitle = paste0("RMSE = ",
                         round(sqrt(mean((y_te - hp)^2)), 3),
                         "  |  MAE = ",
                         round(mean(abs(y_te - hp)), 3)),
       x = NULL, y = "Price (USD)") +
  theme_minimal(base_size = 12) +
  theme(legend.position   = "top",
        plot.title        = element_text(face = "bold"),
        axis.text.x       = element_text(angle = 45, hjust = 1),
        panel.grid.minor  = element_blank())

ggsave(file.path(OUTPUT_DIR, "results_01_actual_vs_predicted.png"),
       p1, width = 10, height = 5, dpi = 150)
cat("  Saved: results_01_actual_vs_predicted.png\n")


# ════════════════════════════════════════════════════════════════════════════════
#  PLOT 2 — Prediction Error Bar Chart
# ════════════════════════════════════════════════════════════════════════════════
pred_df <- pred_df %>%
  mutate(ErrorColor = case_when(
    abs(Error) <= 0.5 ~ "Within $0.50",
    abs(Error) <= 1.0 ~ "Within $1.00",
    TRUE              ~ "Over $1.00"
  ),
  ErrorColor = factor(ErrorColor,
                      levels = c("Within $0.50","Within $1.00","Over $1.00")))

p2 <- ggplot(pred_df, aes(x = Date, y = Error, fill = ErrorColor)) +
  geom_col(width = 0.6) +
  geom_hline(yintercept = 0, linewidth = 0.5, color = "grey40") +
  geom_hline(yintercept =  0.5, linetype = "dashed",
             linewidth = 0.4, color = "#1D9E75", alpha = 0.7) +
  geom_hline(yintercept = -0.5, linetype = "dashed",
             linewidth = 0.4, color = "#1D9E75", alpha = 0.7) +
  scale_fill_manual(values = c("Within $0.50" = "#1D9E75",
                               "Within $1.00" = "#BA7517",
                               "Over $1.00"   = "#E24B4A"),
                    name = "Error range") +
  scale_x_date(date_labels = "%b %d", date_breaks = "3 days") +
  labs(title = "HYDRA prediction errors — December 2024",
       x = NULL, y = "Error (predicted − actual, USD)") +
  theme_minimal(base_size = 12) +
  theme(plot.title       = element_text(face = "bold"),
        axis.text.x      = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank(),
        legend.position  = "top")

ggsave(file.path(OUTPUT_DIR, "results_02_error_bar.png"),
       p2, width = 10, height = 5, dpi = 150)
cat("  Saved: results_02_error_bar.png\n")


# ════════════════════════════════════════════════════════════════════════════════
#  PLOT 3 — Model Comparison (RMSE Bar Chart)
# ════════════════════════════════════════════════════════════════════════════════
results_df <- results_df %>%
  arrange(RMSE) %>%
  mutate(Model     = factor(Model, levels = Model),
         IsHYDRA   = Model == "HYDRA",
         FillColor = ifelse(IsHYDRA, "#1D9E75", "#378ADD"))

p3 <- ggplot(results_df, aes(x = reorder(Model, RMSE), y = RMSE,
                              fill = IsHYDRA)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = sprintf("%.3f", RMSE)),
            hjust = -0.15, size = 3.5,
            color = "grey30") +
  coord_flip() +
  scale_fill_manual(values = c("FALSE" = "#85B7EB", "TRUE" = "#1D9E75"),
                    guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Model comparison — RMSE on December 2024 test set",
       subtitle = "Lower is better",
       x = NULL, y = "RMSE (USD)") +
  theme_minimal(base_size = 12) +
  theme(plot.title  = element_text(face = "bold"),
        panel.grid.major.y = element_blank())

ggsave(file.path(OUTPUT_DIR, "results_03_model_comparison.png"),
       p3, width = 9, height = 5, dpi = 150)
cat("  Saved: results_03_model_comparison.png\n")


# ════════════════════════════════════════════════════════════════════════════════
#  PLOT 4 — PCA Biplot (PC1 vs PC2, coloured by regime)
# ════════════════════════════════════════════════════════════════════════════════
STRESS_FEATS <- c("vix_price", "gprd_index", "epu_index")
stress_center <- colMeans(train_df[, STRESS_FEATS])
stress_scale  <- apply(train_df[, STRESS_FEATS], 2, sd)
S_tr_sc <- scale(train_df[, STRESS_FEATS],
                  center = stress_center, scale = stress_scale)
PC_tr_plot <- as.data.frame(predict(pca_fit, S_tr_sc)[, 1:2])
colnames(PC_tr_plot) <- c("PC1", "PC2")
PC_tr_plot$Regime <- label_map[as.character(regime_tr)]
PC_tr_plot$Regime <- factor(PC_tr_plot$Regime,
                             levels = c("Low-Stress","Med-Stress","High-Stress"))

p4 <- ggplot(PC_tr_plot, aes(x = PC1, y = PC2, color = Regime)) +
  geom_point(alpha = 0.4, size = 0.8) +
  stat_ellipse(level = 0.90, linewidth = 1.0) +
  scale_color_manual(values = c("Low-Stress"  = "#1D9E75",
                                "Med-Stress"  = "#BA7517",
                                "High-Stress" = "#E24B4A")) +
  labs(title    = "PCA — micro-regime clusters in stress feature space",
       subtitle = sprintf("PC1: %.1f%%  |  PC2: %.1f%%  |  Total: %.1f%%",
                          var_exp[1]*100, var_exp[2]*100, sum(var_exp)*100),
       x = paste0("PC1 (", round(var_exp[1]*100,1), "%)"),
       y = paste0("PC2 (", round(var_exp[2]*100,1), "%)"),
       color = "Micro-regime") +
  theme_minimal(base_size = 12) +
  theme(plot.title    = element_text(face = "bold"),
        legend.position = "top")

ggsave(file.path(OUTPUT_DIR, "results_04_pca_biplot.png"),
       p4, width = 8, height = 6, dpi = 150)
cat("  Saved: results_04_pca_biplot.png\n")


# ════════════════════════════════════════════════════════════════════════════════
#  PLOT 5 — Regime Distribution Over Time
# ════════════════════════════════════════════════════════════════════════════════
train_df$Regime <- factor(
  label_map[as.character(regime_tr)],
  levels = c("Low-Stress","Med-Stress","High-Stress")
)

p5 <- ggplot(train_df, aes(x = Date, y = crude_oil_price, color = Regime)) +
  geom_point(size = 0.5, alpha = 0.6) +
  scale_color_manual(values = c("Low-Stress"  = "#1D9E75",
                                "Med-Stress"  = "#BA7517",
                                "High-Stress" = "#E24B4A")) +
  labs(title  = "Micro-regime assignment over time (training set)",
       x = NULL, y = "Crude oil price (USD)",
       color = "Micro-regime") +
  theme_minimal(base_size = 12) +
  theme(plot.title      = element_text(face = "bold"),
        legend.position = "top",
        panel.grid.minor = element_blank())

ggsave(file.path(OUTPUT_DIR, "results_05_regime_over_time.png"),
       p5, width = 10, height = 5, dpi = 150)
cat("  Saved: results_05_regime_over_time.png\n")


# ════════════════════════════════════════════════════════════════════════════════
#  PLOT 6 — All Models: Actual vs Predicted (Faceted)
# ════════════════════════════════════════════════════════════════════════════════
all_pred_long <- do.call(rbind, lapply(names(all_preds), function(nm) {
  data.frame(
    Date      = pred_df$Date,
    Actual    = y_te,
    Predicted = all_preds[[nm]],
    Model     = nm
  )
}))

p6 <- ggplot(all_pred_long, aes(x = Date)) +
  geom_line(aes(y = Actual),    color = "#378ADD",
            linewidth = 0.7, linetype = "solid") +
  geom_line(aes(y = Predicted), color = "#E24B4A",
            linewidth = 0.7, linetype = "dashed") +
  facet_wrap(~Model, ncol = 2) +
  scale_x_date(date_labels = "%d", date_breaks = "7 days") +
  labs(title    = "All models: actual (blue) vs predicted (red) — Dec 2024",
       x = "December 2024", y = "Crude oil price (USD)") +
  theme_minimal(base_size = 10) +
  theme(plot.title       = element_text(face = "bold"),
        strip.text       = element_text(face = "bold"),
        panel.grid.minor = element_blank())

ggsave(file.path(OUTPUT_DIR, "results_06_all_models_faceted.png"),
       p6, width = 12, height = 10, dpi = 150)
cat("  Saved: results_06_all_models_faceted.png\n")


# ════════════════════════════════════════════════════════════════════════════════
#  SUMMARY PRINTOUT
# ════════════════════════════════════════════════════════════════════════════════
cat("  All plots and CSVs saved to: outputs/\n")
cat("\n  Files generated:\n")
files <- list.files(OUTPUT_DIR, full.names = FALSE)
for (f in sort(files)) cat(sprintf("    %s\n", f))

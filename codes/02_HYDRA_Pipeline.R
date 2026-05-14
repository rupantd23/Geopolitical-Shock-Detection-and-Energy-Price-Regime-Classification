
library(dplyr)
library(tidyr)
library(MASS)        # lda()
library(e1071)       # svm(), naiveBayes()
library(nnet)        # nnet() — MLP
library(randomForest)
library(gbm)
library(caret)       # trainControl, cross-validation
library(ggplot2)
library(gridExtra)

set.seed(42)


MERGED_PATH    <- "merged_red_regime_data.csv"
RED_TRAIN_PATH <- "red_regime_train.csv"
RED_TEST_PATH  <- "red_regime_test_dec2024.csv"
OUTPUT_DIR     <- "outputs/"
dir.create(OUTPUT_DIR, showWarnings = FALSE)


# ════════════════════════════════════════════════════════════════════════════════
#  STAGE 1 — DATA LOADING & PREPROCESSING
# ════════════════════════════════════════════════════════════════════════════════

df       <- read.csv(MERGED_PATH,    stringsAsFactors = FALSE)
red_tr   <- read.csv(RED_TRAIN_PATH, stringsAsFactors = FALSE)
red_te   <- read.csv(RED_TEST_PATH,  stringsAsFactors = FALSE)

# Filter to Red Regime dates
red_dates <- union(red_tr$Date, red_te$Date)
df        <- df[df$Date %in% red_dates, ]
df$Date   <- as.Date(df$Date)
df        <- df[order(df$Date), ]

# Drop duplicate columns (GPRD == gprd_index, VIX_price == vix_price)
df$GPRD      <- NULL
df$VIX_price <- NULL

# Create lag features for crude_oil_price
df$crude_lag1 <- c(NA, head(df$crude_oil_price, -1))
df$crude_lag2 <- c(NA, NA, head(df$crude_oil_price, -2))
df$crude_lag3 <- c(NA, NA, NA, head(df$crude_oil_price, -3))
df$crude_lag5 <- c(rep(NA, 5), head(df$crude_oil_price, -5))
df            <- na.omit(df)
rownames(df)  <- NULL

# Train / Test split — last month (December 2024) as test
last_month <- format(max(df$Date), "%Y-%m")
train_df   <- df[format(df$Date, "%Y-%m") != last_month, ]
test_df    <- df[format(df$Date, "%Y-%m") == last_month, ]

# Feature & target definitions
STRESS_FEATS <- c("vix_price", "gprd_index", "epu_index")
REG_FEATS    <- c("vix_price", "gprd_index", "epu_index",
                  "sp500_price", "crude_lag1", "crude_lag2",
                  "crude_lag3", "crude_lag5")
TARGET       <- "crude_oil_price"

X_tr_raw <- as.matrix(train_df[, REG_FEATS])
y_tr     <- train_df[[TARGET]]
X_te_raw <- as.matrix(test_df[, REG_FEATS])
y_te     <- test_df[[TARGET]]

cat(sprintf("  Train : %d rows (%s → %s)\n",
            nrow(train_df), min(train_df$Date), max(train_df$Date)))
cat(sprintf("  Test  : %d rows (%s → %s)\n",
            nrow(test_df), min(test_df$Date), max(test_df$Date)))


# ════════════════════════════════════════════════════════════════════════════════
#  STAGE 2 — PCA (Dimensionality Reduction on Stress Features)
# ════════════════════════════════════════════════════════════════════════════════

# Scale stress features
S_tr_scaled <- scale(train_df[, STRESS_FEATS])
stress_center <- attr(S_tr_scaled, "scaled:center")
stress_scale  <- attr(S_tr_scaled, "scaled:scale")
S_te_scaled <- scale(test_df[, STRESS_FEATS],
                     center = stress_center, scale = stress_scale)

# Fit PCA on training stress features
pca_fit <- prcomp(S_tr_scaled, center = FALSE, scale. = FALSE)
PC_tr   <- pca_fit$x[, 1:2]
PC_te   <- predict(pca_fit, S_te_scaled)[, 1:2]

var_exp <- summary(pca_fit)$importance[2, 1:2]
cat(sprintf("  Variance explained — PC1: %.2f%%  PC2: %.2f%%  (Total: %.2f%%)\n",
            var_exp[1]*100, var_exp[2]*100, sum(var_exp)*100))


# ════════════════════════════════════════════════════════════════════════════════
#  STAGE 3 — KMeans Clustering (Micro-Regime Discovery)
# ════════════════════════════════════════════════════════════════════════════════
cat("\n================================================================\n")
cat("  STAGE 3 — KMeans Clustering (Micro-Regime Discovery)\n")
cat("================================================================\n")

km_fit    <- kmeans(S_tr_scaled, centers = 3, nstart = 20)
regime_tr <- km_fit$cluster            # 1, 2, or 3

# Assign test points to nearest centroid
assign_cluster <- function(X_scaled, centers) {
  dists <- apply(centers, 1, function(c) rowSums((X_scaled - c)^2))
  apply(dists, 1, which.min)
}
regime_te <- assign_cluster(S_te_scaled, km_fit$centers)

# Soft membership via softmax over negative distances
soft_membership <- function(X_scaled, centers) {
  neg_dists <- -apply(centers, 1, function(c) sqrt(rowSums((X_scaled - c)^2)))
  exp_d     <- exp(neg_dists - apply(neg_dists, 1, max))
  exp_d / rowSums(exp_d)
}
soft_tr <- soft_membership(S_tr_scaled, km_fit$centers)  # (n_train x 3)
soft_te <- soft_membership(S_te_scaled, km_fit$centers)  # (n_test  x 3)

# Label clusters by average VIX (higher VIX = more stress)
vix_by_cluster <- tapply(train_df$vix_price, regime_tr, mean)
cluster_order  <- order(vix_by_cluster)          # low → high VIX
label_map      <- setNames(
  c("Low-Stress", "Med-Stress", "High-Stress"),
  cluster_order
)

for (c in 1:3) {
  mask <- regime_tr == c
  cat(sprintf("  [%s] %d days | avg VIX=%.1f | avg crude=$%.1f\n",
              label_map[as.character(c)], sum(mask),
              mean(train_df$vix_price[mask]),
              mean(y_tr[mask])))
}


# ════════════════════════════════════════════════════════════════════════════════
#  STAGE 4 — Fisher LDA (Discriminant Features)
# ════════════════════════════════════════════════════════════════════════════════
cat("\n================================================================\n")
cat("  STAGE 4 — Fisher LDA (Discriminant Features)\n")
cat("================================================================\n")

# Scale raw features for LDA
lda_scale_center <- colMeans(X_tr_raw)
lda_scale_sd     <- apply(X_tr_raw, 2, sd)
X_lda_tr <- scale(X_tr_raw, center = lda_scale_center, scale = lda_scale_sd)
X_lda_te <- scale(X_te_raw, center = lda_scale_center, scale = lda_scale_sd)

lda_fit <- lda(X_lda_tr, grouping = as.factor(regime_tr))
LD_tr   <- as.matrix(predict(lda_fit, X_lda_tr)$x[, 1:2])
LD_te   <- as.matrix(predict(lda_fit, X_lda_te)$x[, 1:2])

lda_var <- lda_fit$svd^2 / sum(lda_fit$svd^2)
cat(sprintf("  LDA variance — LD1: %.2f%%  LD2: %.2f%%\n",
            lda_var[1]*100, lda_var[2]*100))


# ════════════════════════════════════════════════════════════════════════════════
#  STAGE 5 — Naive Bayes (Regime Membership Probabilities)
# ════════════════════════════════════════════════════════════════════════════════
cat("\n================================================================\n")
cat("  STAGE 5 — Naive Bayes (Regime Membership Probabilities)\n")
cat("================================================================\n")

nb_fit     <- naiveBayes(X_lda_tr, as.factor(regime_tr))
NB_prob_tr <- predict(nb_fit, X_lda_tr, type = "raw")
NB_prob_te <- predict(nb_fit, X_lda_te, type = "raw")

nb_preds   <- predict(nb_fit, X_lda_tr)
nb_acc     <- mean(nb_preds == as.factor(regime_tr))
cat(sprintf("  Naive Bayes train accuracy on regime labels: %.2f%%\n",
            nb_acc * 100))

# Build augmented feature matrices
X_tr <- cbind(X_tr_raw, PC_tr, LD_tr, NB_prob_tr)
X_te <- cbind(X_te_raw, PC_te, LD_te, NB_prob_te)
colnames(X_tr) <- NULL
colnames(X_te) <- NULL
cat(sprintf("  Final augmented feature matrix: %d features\n", ncol(X_tr)))


# ════════════════════════════════════════════════════════════════════════════════
#  STAGE 6 — REGIME-EXPERT REGRESSORS
# ════════════════════════════════════════════════════════════════════════════════
cat("\n================================================================\n")
cat("  STAGE 6 — Regime-Expert Regressors\n")
cat("================================================================\n")

# Helper: scale X using training subset stats
make_scaler <- function(X) {
  list(center = colMeans(X), scale = apply(X, 2, sd))
}
apply_scaler <- function(X, scaler) {
  scale(X, center = scaler$center, scale = scaler$scale)
}

expert_models  <- list()
expert_scalers <- list()

high_cluster <- cluster_order[3]   # Highest VIX cluster → SVR
med_cluster  <- cluster_order[2]   # Medium VIX cluster  → GBM
low_cluster  <- cluster_order[1]   # Lowest VIX cluster  → MLP (nnet)

# ── Expert 1: SVR (High-Stress) ───────────────────────────────────────────────
mask_high <- regime_tr == high_cluster
sc_high   <- make_scaler(X_tr[mask_high, ])
Xs_high   <- apply_scaler(X_tr[mask_high, ], sc_high)
expert_models[["high"]]  <- svm(Xs_high, y_tr[mask_high],
                                 type = "eps-regression",
                                 kernel = "radial",
                                 cost = 200, epsilon = 0.3)
expert_scalers[["high"]] <- sc_high
cat(sprintf("  Expert SVR  (High-Stress): trained on %d samples\n", sum(mask_high)))

# ── Expert 2: GBM (Med-Stress) ────────────────────────────────────────────────
mask_med <- regime_tr == med_cluster
df_med   <- as.data.frame(X_tr[mask_med, ])
df_med$y <- y_tr[mask_med]
expert_models[["med"]] <- gbm(y ~ ., data = df_med,
                               distribution    = "gaussian",
                               n.trees         = 150,
                               interaction.depth = 4,
                               shrinkage       = 0.08,
                               verbose         = FALSE)
cat(sprintf("  Expert GBM  (Med-Stress) : trained on %d samples\n", sum(mask_med)))

# ── Expert 3: MLP / nnet (Low-Stress) ─────────────────────────────────────────
mask_low <- regime_tr == low_cluster
sc_low   <- make_scaler(X_tr[mask_low, ])
Xs_low   <- apply_scaler(X_tr[mask_low, ], sc_low)
# nnet requires y scaled to [0,1]
y_low_min <- min(y_tr[mask_low]); y_low_max <- max(y_tr[mask_low])
y_low_sc  <- (y_tr[mask_low] - y_low_min) / (y_low_max - y_low_min)
expert_models[["low"]]  <- nnet(Xs_low, y_low_sc,
                                 size    = 8,
                                 linout  = TRUE,
                                 maxit   = 500,
                                 decay   = 0.001,
                                 trace   = FALSE)
expert_scalers[["low"]] <- sc_low
expert_y_scale          <- c(y_low_min, y_low_max)
cat(sprintf("  Expert MLP  (Low-Stress) : trained on %d samples\n", sum(mask_low)))

# ── Expert prediction function (soft-weighted blend) ─────────────────────────
expert_predict <- function(X, soft_weights) {
  n <- nrow(X)
  p <- matrix(0, nrow = n, ncol = 3)

  # High-Stress (SVR)
  Xs <- apply_scaler(X, expert_scalers[["high"]])
  p[, high_cluster] <- predict(expert_models[["high"]], Xs)

  # Med-Stress (GBM)
  df_pred <- as.data.frame(X)
  p[, med_cluster] <- predict(expert_models[["med"]], df_pred,
                               n.trees = 150, type = "response")

  # Low-Stress (MLP)
  Xs_l  <- apply_scaler(X, expert_scalers[["low"]])
  p_sc  <- predict(expert_models[["low"]], Xs_l)
  p[, low_cluster] <- p_sc * (expert_y_scale[2] - expert_y_scale[1]) +
                      expert_y_scale[1]

  rowSums(p * soft_weights)
}


# ════════════════════════════════════════════════════════════════════════════════
#  STAGE 7 — GLOBAL REGRESSORS
# ════════════════════════════════════════════════════════════════════════════════
cat("\n================================================================\n")
cat("  STAGE 7 — Global Regressors (5-fold TimeSeriesSplit CV)\n")
cat("================================================================\n")

# Time-series cross-validation (manual rolling-origin splits)
n_tr   <- nrow(X_tr)
n_folds <- 5
fold_size <- floor(n_tr / (n_folds + 1))

ts_cv_rmse <- function(pred_fn) {
  rmse_vals <- numeric(n_folds)
  for (k in 1:n_folds) {
    tr_end  <- k * fold_size
    val_idx <- (tr_end + 1):min(tr_end + fold_size, n_tr)
    p       <- pred_fn(1:tr_end, val_idx)
    rmse_vals[k] <- sqrt(mean((y_tr[val_idx] - p)^2))
  }
  mean(rmse_vals)
}

# Scalers for SVR and MLP global models
sc_global <- make_scaler(X_tr)
X_tr_sc   <- apply_scaler(X_tr, sc_global)
X_te_sc   <- apply_scaler(X_te, sc_global)

global_models  <- list()

# ── Global SVR ────────────────────────────────────────────────────────────────
cv_svr <- ts_cv_rmse(function(tr_idx, val_idx) {
  sc <- make_scaler(X_tr[tr_idx, ])
  m  <- svm(apply_scaler(X_tr[tr_idx,], sc), y_tr[tr_idx],
             type = "eps-regression", kernel = "radial",
             cost = 100, epsilon = 0.5)
  predict(m, apply_scaler(X_tr[val_idx,], sc))
})
global_models[["SVR"]] <- svm(X_tr_sc, y_tr, type = "eps-regression",
                               kernel = "radial", cost = 100, epsilon = 0.5)
cat(sprintf("  SVR   : CV RMSE = %.3f\n", cv_svr))

# ── Global MLP ────────────────────────────────────────────────────────────────
y_min_g <- min(y_tr); y_max_g <- max(y_tr)
y_tr_sc <- (y_tr - y_min_g) / (y_max_g - y_min_g)

cv_mlp <- ts_cv_rmse(function(tr_idx, val_idx) {
  sc    <- make_scaler(X_tr[tr_idx,])
  yn    <- (y_tr[tr_idx] - y_min_g) / (y_max_g - y_min_g)
  m     <- nnet(apply_scaler(X_tr[tr_idx,], sc), yn,
                size = 8, linout = TRUE, maxit = 500,
                decay = 0.001, trace = FALSE)
  p_sc  <- predict(m, apply_scaler(X_tr[val_idx,], sc))
  p_sc * (y_max_g - y_min_g) + y_min_g
})
global_models[["MLP"]] <- nnet(X_tr_sc, y_tr_sc, size = 8,
                                linout = TRUE, maxit = 500,
                                decay = 0.001, trace = FALSE)
cat(sprintf("  MLP   : CV RMSE = %.3f\n", cv_mlp))

# ── Global Random Forest ──────────────────────────────────────────────────────
X_tr_df <- as.data.frame(X_tr)
X_te_df <- as.data.frame(X_te)

cv_rf <- ts_cv_rmse(function(tr_idx, val_idx) {
  m <- randomForest(X_tr_df[tr_idx,], y_tr[tr_idx],
                    ntree = 200, mtry = floor(ncol(X_tr)/3))
  predict(m, X_tr_df[val_idx,])
})
global_models[["RF"]] <- randomForest(X_tr_df, y_tr, ntree = 200,
                                       mtry = floor(ncol(X_tr)/3))
cat(sprintf("  RF    : CV RMSE = %.3f\n", cv_rf))

# ── Global GBM ────────────────────────────────────────────────────────────────
X_tr_df$y <- y_tr
cv_gb <- ts_cv_rmse(function(tr_idx, val_idx) {
  m <- gbm(y ~ ., data = X_tr_df[tr_idx,],
            distribution = "gaussian", n.trees = 200,
            interaction.depth = 5, shrinkage = 0.05, verbose = FALSE)
  predict(m, X_tr_df[val_idx,], n.trees = 200, type = "response")
})
global_models[["GB"]] <- gbm(y ~ ., data = X_tr_df,
                              distribution = "gaussian", n.trees = 200,
                              interaction.depth = 5, shrinkage = 0.05,
                              verbose = FALSE)
X_tr_df$y <- NULL
cat(sprintf("  GB    : CV RMSE = %.3f\n", cv_gb))


# ════════════════════════════════════════════════════════════════════════════════
#  STAGE 8 — META-STACKER
# ════════════════════════════════════════════════════════════════════════════════
cat("\n================================================================\n")
cat("  STAGE 8 — Meta-Stacker\n")
cat("================================================================\n")

build_meta_features <- function(X, X_sc, X_df, soft_weights) {
  cbind(
    expert_blend = expert_predict(X, soft_weights),
    svr_pred     = predict(global_models[["SVR"]], X_sc),
    mlp_pred     = predict(global_models[["MLP"]], X_sc) *
                   (y_max_g - y_min_g) + y_min_g,
    rf_pred      = predict(global_models[["RF"]], X_df),
    gb_pred      = predict(global_models[["GB"]], X_df,
                           n.trees = 200, type = "response")
  )
}

M_tr_df    <- as.data.frame(build_meta_features(X_tr, X_tr_sc, X_tr_df, soft_tr))
M_tr_df$y  <- y_tr

X_te_df2   <- as.data.frame(X_te)
M_te_df    <- as.data.frame(build_meta_features(X_te, X_te_sc, X_te_df2, soft_te))

meta_model <- gbm(y ~ ., data = M_tr_df,
                  distribution = "gaussian", n.trees = 100,
                  interaction.depth = 3, shrinkage = 0.05, verbose = FALSE)

# Meta-stacker feature importances
meta_imp <- summary(meta_model, plotit = FALSE)
cat("  Meta-stacker feature importances:\n")
for (i in seq_len(nrow(meta_imp))) {
  bar <- paste(rep("\u2588", floor(meta_imp$rel.inf[i] / 2.5)), collapse = "")
  cat(sprintf("    %-15s %.4f  %s\n",
              meta_imp$var[i], meta_imp$rel.inf[i] / 100, bar))
}


# ════════════════════════════════════════════════════════════════════════════════
#  STAGE 9 — EVALUATION & RESULTS
# ════════════════════════════════════════════════════════════════════════════════
cat("\n================================================================\n")
cat("  STAGE 9 — Results (December 2024 Test Set)\n")
cat("================================================================\n")

all_preds <- list(
  "Expert Blend" = expert_predict(X_te, soft_te),
  "Global SVR"   = predict(global_models[["SVR"]], X_te_sc),
  "Global MLP"   = predict(global_models[["MLP"]], X_te_sc) *
                   (y_max_g - y_min_g) + y_min_g,
  "Global RF"    = predict(global_models[["RF"]], X_te_df2),
  "Global GB"    = predict(global_models[["GB"]], X_te_df2,
                           n.trees = 200, type = "response"),
  "HYDRA"        = predict(meta_model, M_te_df,
                           n.trees = 100, type = "response")
)

calc_metrics <- function(actual, pred) {
  rmse <- sqrt(mean((actual - pred)^2))
  mae  <- mean(abs(actual - pred))
  ss_res <- sum((actual - pred)^2)
  ss_tot <- sum((actual - mean(actual))^2)
  r2   <- 1 - ss_res / ss_tot
  c(RMSE = round(rmse, 4), MAE = round(mae, 4), R2 = round(r2, 4))
}

# Print comparison table
cat(sprintf("\n  %-22s  %6s  %6s  %7s\n", "Model", "RMSE", "MAE", "R2"))
cat("  ", paste(rep("-", 46), collapse = ""), "\n")

results_list <- list()
best_rmse    <- Inf
best_name    <- ""

for (nm in names(all_preds)) {
  m   <- calc_metrics(y_te, all_preds[[nm]])
  tag <- if (m["RMSE"] < best_rmse) { best_rmse <- m["RMSE"]; best_name <- nm; "" } else ""
  results_list[[nm]] <- m
}
for (nm in names(all_preds)) {
  m   <- results_list[[nm]]
  tag <- if (nm == best_name) "  <- BEST" else ""
  cat(sprintf("  %-22s  %6.3f  %6.3f  %7.4f%s\n",
              nm, m["RMSE"], m["MAE"], m["R2"], tag))
}

# Day-by-day prediction table
hydra_preds <- all_preds[["HYDRA"]]
regime_labels <- label_map[as.character(regime_te)]

cat(sprintf("\n  %-12s  %7s  %9s  %7s  Micro-Regime\n",
            "Date", "Actual", "Predicted", "Error"))
cat("  ", paste(rep("-", 56), collapse = ""), "\n")

for (i in seq_along(y_te)) {
  cat(sprintf("  %-12s  %7.2f  %9.2f  %+7.2f  %s\n",
              format(test_df$Date[i]), y_te[i], hydra_preds[i],
              hydra_preds[i] - y_te[i], regime_labels[i]))
}

# Save model comparison CSV
results_df <- do.call(rbind, lapply(names(results_list), function(nm) {
  data.frame(Model = nm, t(results_list[[nm]]))
}))
write.csv(results_df,
          file.path(OUTPUT_DIR, "hydra_model_comparison.csv"),
          row.names = FALSE)

# Save predictions CSV
pred_df <- data.frame(
  Date         = format(test_df$Date),
  Actual       = round(y_te, 2),
  HYDRA_Pred   = round(hydra_preds, 2),
  Error        = round(hydra_preds - y_te, 2),
  Micro_Regime = regime_labels
)
write.csv(pred_df,
          file.path(OUTPUT_DIR, "hydra_predictions_dec2024.csv"),
          row.names = FALSE)

cat(sprintf("\n  Outputs saved to: %s\n", OUTPUT_DIR))
cat("\n================================================================\n")
cat("  HYDRA pipeline complete.\n")
cat("================================================================\n")

# Store objects for results script
saveRDS(list(
  y_te         = y_te,
  hydra_preds  = hydra_preds,
  all_preds    = all_preds,
  results_df   = results_df,
  pred_df      = pred_df,
  train_df     = train_df,
  test_df      = test_df,
  regime_tr    = regime_tr,
  regime_te    = regime_te,
  label_map    = label_map,
  soft_tr      = soft_tr,
  pca_fit      = pca_fit,
  var_exp      = var_exp
), file.path(OUTPUT_DIR, "hydra_objects.rds"))
cat("  Model objects saved to: outputs/hydra_objects.rds\n")

dir.create("data", showWarnings = FALSE)
dir.create("outputs", showWarnings = FALSE)
dir.create("data/raw", showWarnings = FALSE)
dir.create("data/cleaned", showWarnings = FALSE)
dir.create("outputs/eda_figures", showWarnings = FALSE)
dir.create("outputs/models", showWarnings = FALSE)
dir.create("outputs/ml_plots", showWarnings = FALSE)
dir.create("scripts", showWarnings = FALSE)
dir.create("reports", showWarnings = FALSE)

pkgs <- c("dplyr","ggplot2","caret","pROC","randomForest")
miss <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(miss)) install.packages(miss)
invisible(lapply(pkgs, library, character.only = TRUE))

df <- read.csv(file.choose())
write.csv(df, "data/raw/heart_raw.csv", row.names = FALSE)

df <- na.omit(df)
df$condition <- factor(df$condition, levels = c(0,1), labels = c("NoDisease","Disease"))

cat_cols <- c("sex","cp","fbs","restecg","exang","slope","ca","thal")
cat_cols <- intersect(cat_cols, names(df))
df[cat_cols] <- lapply(df[cat_cols], factor)

write.csv(df, "data/cleaned/heart_clean.csv", row.names = FALSE)

set.seed(123)
idx <- createDataPartition(df$condition, p = 0.8, list = FALSE)
train <- df[idx, ]
test <- df[-idx, ]

cm_plot <- function(pred, truth, title, out_path){
  cm <- confusionMatrix(pred, truth, positive = "Disease")
  m <- as.data.frame(cm$table)
  colnames(m) <- c("Prediction","Reference","Freq")
  p <- ggplot(m, aes(x = Reference, y = Prediction, fill = Freq)) +
    geom_tile() +
    geom_text(aes(label = Freq), size = 6) +
    theme_minimal() +
    labs(title = title, x = "Actual", y = "Predicted")
  ggsave(out_path, p, width = 6, height = 5)
  cm
}

log_model <- glm(condition ~ ., data = train, family = binomial)
log_prob <- predict(log_model, newdata = test, type = "response")
log_pred <- factor(ifelse(log_prob >= 0.5, "Disease", "NoDisease"), levels = c("NoDisease","Disease"))

cm_log <- cm_plot(log_pred, test$condition, "Logistic Regression - Confusion Matrix", "outputs/ml_plots/cm_logistic.png")
cm_log

roc_log <- roc(response = test$condition, predictor = log_prob, levels = c("NoDisease","Disease"))
auc_log <- as.numeric(auc(roc_log))
auc_log

png("outputs/ml_plots/roc_logistic.png", width = 900, height = 650)
plot(roc_log, main = paste0("Logistic Regression ROC (AUC = ", round(auc_log, 3), ")"))
dev.off()

p_log_prob <- ggplot(data.frame(prob = log_prob, actual = test$condition), aes(x = prob, fill = actual)) +
  geom_histogram(bins = 25, position = "identity", alpha = 0.6) +
  theme_minimal() +
  labs(title = "Logistic Regression - Predicted Probability Distribution",
       x = "Predicted Probability of Disease", y = "Count")
ggsave("outputs/ml_plots/prob_logistic.png", p_log_prob, width = 7, height = 5)

set.seed(123)
rf_model <- randomForest(condition ~ ., data = train, ntree = 500, importance = TRUE)
rf_prob <- predict(rf_model, newdata = test, type = "prob")[, "Disease"]
rf_pred <- factor(ifelse(rf_prob >= 0.5, "Disease", "NoDisease"), levels = c("NoDisease","Disease"))

cm_rf <- cm_plot(rf_pred, test$condition, "Random Forest - Confusion Matrix", "outputs/ml_plots/cm_random_forest.png")
cm_rf

roc_rf <- roc(response = test$condition, predictor = rf_prob, levels = c("NoDisease","Disease"))
auc_rf <- as.numeric(auc(roc_rf))
auc_rf

png("outputs/ml_plots/roc_random_forest.png", width = 900, height = 650)
plot(roc_rf, main = paste0("Random Forest ROC (AUC = ", round(auc_rf, 3), ")"))
dev.off()

p_rf_prob <- ggplot(data.frame(prob = rf_prob, actual = test$condition), aes(x = prob, fill = actual)) +
  geom_histogram(bins = 25, position = "identity", alpha = 0.6) +
  theme_minimal() +
  labs(title = "Random Forest - Predicted Probability Distribution",
       x = "Predicted Probability of Disease", y = "Count")
ggsave("outputs/ml_plots/prob_random_forest.png", p_rf_prob, width = 7, height = 5)

imp <- importance(rf_model)
imp_df <- data.frame(feature = rownames(imp), MeanDecreaseGini = imp[, "MeanDecreaseGini"]) |>
  dplyr::arrange(dplyr::desc(MeanDecreaseGini)) |>
  dplyr::slice_head(n = 12)

p_imp <- ggplot(imp_df, aes(x = reorder(feature, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  labs(title = "Random Forest - Top Feature Importance (Gini)",
       x = "Feature", y = "Mean Decrease Gini")
ggsave("outputs/ml_plots/rf_feature_importance.png", p_imp, width = 7, height = 5)

png("outputs/ml_plots/roc_compare.png", width = 900, height = 650)
plot(roc_log, main = "ROC Comparison: Logistic vs Random Forest")
plot(roc_rf, add = TRUE)
legend("bottomright",
       legend = c(paste0("Logistic AUC=", round(auc_log,3)),
                  paste0("RF AUC=", round(auc_rf,3))),
       lwd = 2, bty = "n")
dev.off()

saveRDS(log_model, "outputs/models/logistic_model.rds")
saveRDS(rf_model, "outputs/models/random_forest_model.rds")

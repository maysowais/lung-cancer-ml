# ==============================
# Clear environment
# ==============================
rm(list = ls())

# ==============================
# Load Libraries
# ==============================
library(ggplot2)

# ==============================
# Create Output Folder
# ==============================
dir.create("outputs/Threshold pic", recursive = TRUE, showWarnings = FALSE)

# ==============================
# Import Raw Data
# ==============================
df_raw <- read.csv("data/raw/lung cancer.csv.csv", stringsAsFactors = FALSE)

# ==============================
# Data Cleaning
# ==============================
df_clean <- df_raw

df_clean$LUNG_CANCER <- ifelse(df_clean$LUNG_CANCER == "YES", 1, 0)
df_clean$LUNG_CANCER <- factor(df_clean$LUNG_CANCER, levels = c(0, 1))

df_clean$GENDER <- factor(df_clean$GENDER)

binary_cols <- c(
  "SMOKING","YELLOW_FINGERS","ANXIETY","PEER_PRESSURE",
  "CHRONIC_DISEASE","FATIGUE","ALLERGY","WHEEZING",
  "ALCOHOL_CONSUMING","COUGHING","SHORTNESS_OF_BREATH",
  "SWALLOWING_DIFFICULTY","CHEST_PAIN"
)

df_clean[binary_cols] <- lapply(
  df_clean[binary_cols],
  function(x) factor(ifelse(x == 2, 1, 0))
)

# ==============================
# Train / Test Split
# ==============================
set.seed(1)

index <- sample(1:nrow(df_clean), size = 0.7 * nrow(df_clean))
train_data <- df_clean[index, ]
test_data  <- df_clean[-index, ]

# ==============================
# Logistic Regression
# ==============================
model_logistic <- glm(
  LUNG_CANCER ~ .,
  data = train_data,
  family = binomial
)

# ==============================
# Prediction (Probabilities)
# ==============================
pred_prob <- predict(
  model_logistic,
  newdata = test_data,
  type = "response"
)

# ==============================
# BASELINE (Threshold = 0.5)
# ==============================
pred_class_05 <- ifelse(pred_prob >= 0.5, 1, 0)
pred_class_05 <- factor(pred_class_05, levels = c(0,1))

cm_05 <- table(Predicted = pred_class_05, Actual = test_data$LUNG_CANCER)

accuracy_05 <- sum(diag(cm_05)) / sum(cm_05)

# ==============================
# THRESHOLD TUNING FUNCTION
# ==============================
calc_metrics <- function(actual, prob, threshold) {
  pred <- ifelse(prob >= threshold, 1, 0)
  pred <- factor(pred, levels = c(0,1))
  actual <- factor(actual, levels = c(0,1))
  
  cm <- table(Predicted = pred, Actual = actual)
  
  TN <- cm["0","0"]
  FN <- cm["0","1"]
  FP <- cm["1","0"]
  TP <- cm["1","1"]
  
  accuracy    <- (TP + TN) / (TP + TN + FP + FN)
  sensitivity <- TP / (TP + FN)
  specificity <- TN / (TN + FP)
  
  data.frame(
    threshold = threshold,
    accuracy = accuracy,
    sensitivity = sensitivity,
    specificity = specificity
  )
}

# ==============================
# Evaluate Many Thresholds
# ==============================
thresholds <- seq(0.05, 0.95, by = 0.01)

results <- do.call(
  rbind,
  lapply(thresholds, function(t) {
    calc_metrics(test_data$LUNG_CANCER, pred_prob, t)
  })
)

# ==============================
# Compute Youden Index
# ==============================
results$youden <- results$sensitivity + results$specificity - 1

best_acc    <- results[which.max(results$accuracy), ]
best_sens   <- results[which.max(results$sensitivity), ]
best_youden <- results[which.max(results$youden), ]

final_threshold <- best_youden$threshold

# ==============================
# Final Model using Best Threshold
# ==============================
final_pred <- ifelse(pred_prob >= final_threshold, 1, 0)
final_pred <- factor(final_pred, levels = c(0,1))

final_cm <- table(
  Predicted = final_pred,
  Actual = test_data$LUNG_CANCER
)

final_accuracy <- sum(diag(final_cm)) / sum(final_cm)

# ==============================
# PRINT RESULTS TO CONSOLE
# ==============================
cat("\n===== BASELINE (Threshold = 0.5) =====\n")
print(cm_05)
cat("Accuracy @ 0.5 =", accuracy_05, "\n")

cat("\n===== BEST ACCURACY THRESHOLD =====\n")
print(best_acc)

cat("\n===== BEST SENSITIVITY THRESHOLD =====\n")
print(best_sens)

cat("\n===== BEST YOUDEN THRESHOLD (FINAL) =====\n")
print(best_youden)
cat("Final Threshold =", final_threshold, "\n")

cat("\n===== CONFUSION MATRIX (FINAL THRESHOLD) =====\n")
print(final_cm)

cat("Final Accuracy =", final_accuracy, "\n")

# ==============================
# 📊 PLOTS (Saved as Images)
# ==============================

# 1️⃣ Accuracy vs Threshold
p_acc <- ggplot(results, aes(x = threshold, y = accuracy)) +
  geom_line(color = "blue", linewidth = 1.2) +
  geom_vline(xintercept = best_acc$threshold, color = "red", linetype = "dashed") +
  labs(
    title = "Accuracy vs Threshold",
    x = "Threshold",
    y = "Accuracy"
  ) +
  theme_minimal()

ggsave("outputs/Threshold pic/accuracy_vs_threshold.png",
       p_acc, width = 7, height = 5, dpi = 300)

# 2️⃣ Sensitivity vs Specificity
p_tradeoff <- ggplot(results, aes(x = threshold)) +
  geom_line(aes(y = sensitivity, color = "Sensitivity"), linewidth = 1.2) +
  geom_line(aes(y = specificity, color = "Specificity"), linewidth = 1.2) +
  geom_vline(xintercept = best_youden$threshold, linetype = "dashed") +
  labs(
    title = "Sensitivity vs Specificity Across Thresholds",
    x = "Threshold",
    y = "Metric Value"
  ) +
  scale_color_manual(values = c("Sensitivity"="red","Specificity"="green")) +
  theme_minimal()

ggsave("outputs/Threshold pic/sensitivity_vs_specificity.png",
       p_tradeoff, width = 7, height = 5, dpi = 300)

# 3️⃣ Youden Index Plot
p_youden <- ggplot(results, aes(x = threshold, y = youden)) +
  geom_line(color = "purple", linewidth = 1.2) +
  geom_vline(xintercept = best_youden$threshold, color = "black", linetype = "dashed") +
  labs(
    title = "Youden Index vs Threshold",
    x = "Threshold",
    y = "Youden Index"
  ) +
  theme_minimal()

ggsave("outputs/Threshold pic/youden_index.png",
       p_youden, width = 7, height = 5, dpi = 300)


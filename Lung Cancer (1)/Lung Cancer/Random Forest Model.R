library(randomForest)

# Clear environment
rm(list = ls())

# ==============================
# Load Libraries
# ==============================
library(ggplot2)
library(randomForest)
library(pROC)

# ==============================
# Import Raw Data
# ==============================
df_raw <- read.csv("data/raw/lung cancer.csv.csv", stringsAsFactors = FALSE)

# ==============================
# Data Cleaning
# ==============================
df_clean <- df_raw

# Target variable: YES / NO → 1 / 0
df_clean$LUNG_CANCER <- ifelse(df_clean$LUNG_CANCER == "YES", 1, 0)
df_clean$LUNG_CANCER <- factor(df_clean$LUNG_CANCER, levels = c(0,1))

# Gender
df_clean$GENDER <- factor(df_clean$GENDER)

# Binary variables
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
set.seed(123)

index <- sample(1:nrow(df_clean), size = 0.7 * nrow(df_clean))
train_data <- df_clean[index, ]
test_data  <- df_clean[-index, ]

# ==============================
# Random Forest Model
# ==============================
rf_model <- randomForest(
  LUNG_CANCER ~ .,
  data = train_data,
  ntree = 500,
  mtry = floor(sqrt(ncol(train_data) - 1)),
  importance = TRUE
)

print(rf_model)

# ==============================
# Prediction (Classes)
# ==============================
rf_pred_class <- predict(
  rf_model,
  newdata = test_data,
  type = "response"
)

# ⚠ إضافة احتمالات التنبؤ (مهم للتقييم و ROC)
rf_pred_prob <- predict(
  rf_model,
  newdata = test_data,
  type = "prob"
)[,2]


# ==============================
# Confusion Matrix & Accuracy
# ==============================
conf_rf <- table(
  Predicted = rf_pred_class,
  Actual = test_data$LUNG_CANCER
)

conf_rf

accuracy_rf <- sum(diag(conf_rf)) / sum(conf_rf)
accuracy_rf


# ==============================
# Sensitivity & Specificity
# ==============================
TP <- conf_rf["1","1"]
FN <- conf_rf["0","1"]
FP <- conf_rf["1","0"]
TN <- conf_rf["0","0"]

sensitivity_rf <- TP / (TP + FN)
specificity_rf <- TN / (TN + FP)

sensitivity_rf
specificity_rf


# ==============================
# Variable Importance (كما هو 👍)
# ==============================
importance(rf_model)

varImpPlot(
  rf_model,
  main = "Variable Importance - Random Forest"
)


# ==============================
# ⭐ NEW — ROC Curve + AUC
# ==============================
roc_rf <- roc(
  response = test_data$LUNG_CANCER,
  predictor = rf_pred_prob,
  levels = c("0","1")
)

plot(roc_rf,
     col = "blue",
     lwd = 3,
     main = "ROC Curve — Random Forest")

auc(roc_rf)


# ==============================
# ⭐ NEW — Confusion Matrix Heatmap
# ==============================
conf_df <- as.data.frame(conf_rf)

ggplot(conf_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile(color = "black") +
  geom_text(aes(label = Freq), size = 6) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(
    title = "Confusion Matrix — Random Forest",
    x = "Actual",
    y = "Predicted"
  ) +
  theme_minimal()

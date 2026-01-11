# ==============================
# Clear environment
# ==============================
rm(list = ls())

# ==============================
# Load Libraries
# ==============================
library(ggplot2)
library(corrplot)

# ==============================
# Create outputs folder
# ==============================
dir.create("outputs/eda_figures", recursive = TRUE, showWarnings = FALSE)

# ==============================
# Import Data
# ==============================
df_raw <- read.csv("data/raw/lung cancer.csv.csv", stringsAsFactors = FALSE)

# ==============================
# Data Cleaning
# ==============================
df_clean <- df_raw

# Target variable
df_clean$LUNG_CANCER <- ifelse(df_clean$LUNG_CANCER == "YES", 1, 0)
df_clean$LUNG_CANCER <- factor(df_clean$LUNG_CANCER, levels = c(0, 1))

# Gender as factor
df_clean$GENDER <- factor(df_clean$GENDER)

# Binary columns
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

str(df_clean)
table(df_clean$LUNG_CANCER)


# =========================================
# 📊 1) Target Distribution — Lung Cancer
# =========================================
p_target <- ggplot(df_clean, aes(x = LUNG_CANCER)) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "Lung Cancer Case Distribution",
    x = "Lung Cancer (0 = No, 1 = Yes)",
    y = "Count"
  )

ggsave("outputs/eda_figures/01_target_distribution.png",
       p_target, width=6, height=4, dpi=300)


# =========================================
# 📊 2) Age Histogram — توزيع العمر
# =========================================
p_age <- ggplot(df_clean, aes(x = AGE)) +
  geom_histogram(bins = 25, fill = "gray") +
  labs(
    title = "Age Distribution",
    x = "Age",
    y = "Count"
  )

ggsave("outputs/eda_figures/02_age_histogram.png",
       p_age, width=6, height=4, dpi=300)


# =========================================
# 📊 3) Age vs Cancer — علاقة X مع Y
# =========================================
p_age_cancer <- ggplot(df_clean, aes(x = AGE, fill = LUNG_CANCER)) +
  geom_histogram(bins = 25, alpha = 0.6, position = "identity") +
  labs(
    title = "Age Distribution by Cancer Status",
    x = "Age",
    y = "Count",
    fill = "Cancer"
  )

ggsave("outputs/eda_figures/03_age_vs_cancer.png",
       p_age_cancer, width=6, height=4, dpi=300)


# =========================================
# 📊 4) Smoking vs Cancer — عامل خطورة
# =========================================
p_smoking <- ggplot(df_clean, aes(x = SMOKING, fill = LUNG_CANCER)) +
  geom_bar(position = "fill") +
  labs(
    title="Smoking vs Lung Cancer (Proportion)",
    x="Smoking (0 = No, 1 = Yes)",
    y="Proportion",
    fill="Cancer"
  )

ggsave("outputs/eda_figures/04_smoking_vs_cancer.png",
       p_smoking, width=6, height=4, dpi=300)


# =========================================
# 📊 5) Symptoms Count — عدد الأعراض لكل مريض
# =========================================
df_clean$SYMPTOM_COUNT <- rowSums(
  sapply(df_clean[binary_cols], function(x) as.numeric(as.character(x)))
)

p_symptoms <- ggplot(df_clean, aes(x = SYMPTOM_COUNT)) +
  geom_histogram(bins = 20, fill = "purple") +
  labs(
    title = "Total Positive Symptoms per Patient",
    x = "Number of Symptoms",
    y = "Count"
  )

ggsave("outputs/eda_figures/05_symptom_count.png",
       p_symptoms, width=6, height=4, dpi=300)


# =========================================
# 📊 6) Correlation Heatmap — علاقة الفيتشرز
# =========================================
numeric_df <- df_clean
numeric_df[binary_cols] <- lapply(
  numeric_df[binary_cols],
  function(x) as.numeric(as.character(x))
)

numeric_df$AGE <- as.numeric(numeric_df$AGE)

numeric_data <- numeric_df[, sapply(numeric_df, is.numeric)]
cor_matrix <- cor(numeric_data, use = "complete.obs")

png("outputs/eda_figures/06_correlation_heatmap.png",
    width=900, height=700)
corrplot(cor_matrix, method="color", tl.cex=0.8)
dev.off()


# =========================================
# Train / Test Split + Model + Accuracy
# =========================================
set.seed(1)

index <- sample(1:nrow(df_clean), size = 0.7 * nrow(df_clean))
train_data <- df_clean[index, ]
test_data  <- df_clean[-index, ]

model_logistic <- glm(
  LUNG_CANCER ~ .,
  data = train_data,
  family = binomial
)

pred_prob <- predict(model_logistic, newdata = test_data, type = "response")
pred_class <- factor(ifelse(pred_prob > 0.5, 1, 0), levels = c(0,1))

conf_matrix <- table(Predicted = pred_class, Actual = test_data$LUNG_CANCER)
conf_matrix

accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
accuracy

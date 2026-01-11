rm(list = ls())

library(ggplot2)

# =========================
# 1) Import Data
# =========================
df_raw <- read.csv("data/raw/heart.csv", stringsAsFactors = FALSE)

dim(df_raw)
names(df_raw)
head(df_raw)
str(df_raw)
table(df_raw$condition)

# =========================
# 2) Data Cleaning
# =========================
df_clean <- df_raw

# Target variable: 0 / 1  → factor
df_clean$condition <- factor(df_clean$condition, levels = c(0, 1))

# Convert binary numeric fields to factors
binary_cols <- c("sex", "fbs", "exang")

df_clean[binary_cols] <- lapply(
  df_clean[binary_cols],
  function(x) factor(ifelse(x == 1, 1, 0))
)

# Convert multi-category integer fields to factors
categorical_cols <- c("cp", "restecg", "slope", "ca", "thal")

df_clean[categorical_cols] <- lapply(
  df_clean[categorical_cols],
  function(x) factor(x)
)

str(df_clean)
table(df_clean$condition)

# =========================
# 3) EDA Plots
# =========================
dir.create("outputs/Heart pic", recursive = TRUE, showWarnings = FALSE)

# Distribution of Heart Disease
p1 <- ggplot(df_clean, aes(x = condition)) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "Distribution of Heart Disease Cases",
    x = "Condition (0 = No Disease, 1 = Disease)",
    y = "Count"
  )

ggsave("outputs/Heart pic/heart_condition_distribution.png",
       plot = p1, width = 6, height = 4, dpi = 300)

# Age vs Condition
p2 <- ggplot(df_clean, aes(x = condition, y = age)) +
  geom_boxplot(fill = "lightgreen") +
  labs(
    title = "Age vs Heart Disease",
    x = "Condition",
    y = "Age"
  )

ggsave("outputs/Heart pic/age_vs_condition.png",
       plot = p2, width = 6, height = 4, dpi = 300)

# Sex vs Condition
p3 <- ggplot(df_clean, aes(x = sex, fill = condition)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Sex vs Heart Disease",
    x = "Sex (0 = Female, 1 = Male)",
    y = "Count",
    fill = "Condition"
  )

ggsave("outputs/Heart pic/sex_vs_condition.png",
       plot = p3, width = 6, height = 4, dpi = 300)

# =========================
# 4) Train/Test Split
# =========================
set.seed(1)

index <- sample(1:nrow(df_clean), size = 0.7 * nrow(df_clean))

train_data <- df_clean[index, ]
test_data  <- df_clean[-index, ]

dim(train_data)
dim(test_data)

# =========================
# 5) Logistic Regression
# =========================
model_logistic <- glm(
  condition ~ .,
  data = train_data,
  family = binomial
)

summary(model_logistic)

# =========================
# 6) Predictions & Accuracy
# =========================
pred_prob <- predict(
  model_logistic,
  newdata = test_data,
  type = "response"
)

pred_class <- ifelse(pred_prob > 0.5, 1, 0)
pred_class <- factor(pred_class, levels = c(0, 1))

conf_matrix <- table(
  Predicted = pred_class,
  Actual = test_data$condition
)

conf_matrix

accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
accuracy

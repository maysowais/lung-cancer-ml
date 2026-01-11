# ==============================
# MANUAL FEATURE SELECTION MODEL
# ==============================

rm(list = ls())

library(ggplot2)
library(dplyr)

dir.create("outputs/manual_feature_selection_plots",
           recursive = TRUE, showWarnings = FALSE)

# ==============================
# Load & Clean Data
# ==============================
df_raw <- read.csv("data/raw/lung cancer.csv.csv",
                   stringsAsFactors = FALSE)

df <- df_raw
df$LUNG_CANCER <- ifelse(df$LUNG_CANCER=="YES",1,0)

binary_cols <- c(
  "SMOKING","YELLOW_FINGERS","ANXIETY","PEER_PRESSURE",
  "CHRONIC_DISEASE","FATIGUE","ALLERGY","WHEEZING",
  "ALCOHOL_CONSUMING","COUGHING","SHORTNESS_OF_BREATH",
  "SWALLOWING_DIFFICULTY","CHEST_PAIN"
)

df[binary_cols] <- lapply(df[binary_cols], function(x) ifelse(x==2,1,0))
df$GENDER <- ifelse(df$GENDER=="M",1,0)

# ==============================
# 🔹 MANUAL FEATURE SELECTION (CLINICAL)
# ==============================
manual_features <- c(
  "AGE",
  "SMOKING",
  "CHRONIC_DISEASE",
  "COUGHING",
  "WHEEZING",
  "SHORTNESS_OF_BREATH",
  "CHEST_PAIN"
)

cat("\n🟢 Selected Manual Clinical Features:\n")
print(manual_features)

# ==============================
# Train / Test Split
# ==============================
set.seed(1)
idx <- sample(1:nrow(df), 0.7*nrow(df))
train <- df[idx,]
test  <- df[-idx,]

formula_manual <- as.formula(
  paste("LUNG_CANCER ~", paste(manual_features, collapse=" + "))
)

# ==============================
# Logistic Regression (Manual Model)
# ==============================
model_manual <- glm(
  formula_manual,
  data=train,
  family="binomial"
)

summary(model_manual)

# ==============================
# Evaluation
# ==============================
prob <- predict(model_manual, newdata=test, type="response")
pred <- ifelse(prob>0.5,1,0)

cm <- table(Predicted=pred, Actual=test$LUNG_CANCER)
acc <- sum(diag(cm))/sum(cm)

cat("\nConfusion Matrix:\n")
print(cm)
cat("\nAccuracy =", acc, "\n")

# ==============================
# 📊 PLOT — Feature Importance
# ==============================
imp <- data.frame(
  Feature = names(coef(model_manual))[-1],
  Weight  = coef(model_manual)[-1]
)

p_imp <- ggplot(imp, aes(x=reorder(Feature,Weight), y=Weight)) +
  geom_col(fill="steelblue") +
  coord_flip() +
  labs(
    title="Manual Selected Feature Importance",
    x="Feature", y="Effect on Cancer Probability"
  )

ggsave("outputs/manual_feature_selection_plots/feature_importance.png",
       p_imp, width=7, height=5, dpi=300)

# ==============================
# 📊 PLOTS — X vs Y (Effect Plots)
# ==============================
for(f in manual_features){
  p <- ggplot(df, aes_string(x=f, y="LUNG_CANCER")) +
    geom_jitter(alpha=.25) +
    stat_smooth(method="glm",
                method.args=list(family="binomial"),
                se=FALSE, color="red") +
    labs(
      title=paste("Effect of", f, "on Lung Cancer Probability"),
      x=f, y="Lung Cancer (0/1)"
    )
  ggsave(paste0("outputs/manual_feature_selection_plots/effect_",f,".png"),
         p, width=7, height=5, dpi=300)
}

# ==============================
# 📊 Confusion Matrix Heatmap
# ==============================
cm_df <- as.data.frame(cm)

p_cm <- ggplot(cm_df, aes(x=Actual, y=Predicted, fill=Freq)) +
  geom_tile(color="black") +
  geom_text(aes(label=Freq), size=6) +
  scale_fill_gradient(low="white", high="red") +
  labs(
    title="Confusion Matrix — Manual Feature Model",
    x="Actual", y="Predicted"
  ) +
  theme_minimal()

ggsave("outputs/manual_feature_selection_plots/confusion_matrix.png",
       p_cm, width=7, height=5, dpi=300)

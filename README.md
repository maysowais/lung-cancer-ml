# Heart Disease & Lung Cancer Prediction (R)

Machine-learning project in **R** that predicts **heart disease** and **lung cancer** from patient data. It covers the full workflow: data cleaning, exploratory analysis, feature selection, model training, evaluation and decision-threshold tuning.

**Result:** the heart disease classifier reaches **85% accuracy**.

## Workflow

1. **Data cleaning:** convert targets and categorical fields to factors and remove missing values
2. **Exploratory analysis:** look at target distributions, age effects, smoking vs. cancer, symptom counts and a correlation heatmap
3. **Feature selection:** measure each feature's effect (age, smoking, chest pain, coughing, wheezing, shortness of breath, chronic disease…) and rank feature importance
4. **Modeling:** Logistic Regression and Random Forest (`caret`, `randomForest`) on an 80/20 train-test split
5. **Evaluation:** confusion matrices, ROC curves and AUC (`pROC`)
6. **Threshold tuning:** compare accuracy, sensitivity and specificity, and pick the threshold with Youden's index

## Sample outputs

| Random Forest ROC | Feature importance | Confusion matrix |
|---|---|---|
| ![ROC](<Lung Cancer (1)/Lung Cancer/outputs/rf_ROC_curve.png>) | ![Importance](<Lung Cancer (1)/Lung Cancer/outputs/rf_feature_importance.png>) | ![CM](<Lung Cancer (1)/Lung Cancer/outputs/rf_confusion_matrix.png>) |

| Smoking vs. cancer | Correlation heatmap | Sensitivity vs. specificity |
|---|---|---|
| ![Smoking](<Lung Cancer (1)/Lung Cancer/outputs/eda_figures/04_smoking_vs_cancer.png>) | ![Heatmap](<Lung Cancer (1)/Lung Cancer/outputs/eda_figures/06_correlation_heatmap.png>) | ![Threshold](<Lung Cancer (1)/Lung Cancer/outputs/Threshold pic/sensitivity_vs_specificity.png>) |

## Project structure

```
Lung Cancer (1)/Lung Cancer/
├── data/raw/                 # heart.csv, lung cancer dataset
├── R-cancer project.R        # lung cancer cleaning + EDA
├── sellection feature.R      # feature effects & selection
├── Random Forest Model.R     # model training & evaluation
├── THRESHOLD TUNING.R        # threshold / Youden index analysis
├── heart dataset.R           # heart disease EDA
└── outputs/                  # all generated figures
heart_disease.md              # heart disease modeling script (logistic regression + random forest)
```

## How to run

1. Open `Lung Cancer.Rproj` in RStudio.
2. Install the packages: `install.packages(c("dplyr", "ggplot2", "caret", "pROC", "randomForest"))`
3. Run the scripts in the order above. Figures are saved to `outputs/`.

---
Author: **Mays Owais** · [Portfolio](https://maysowais.github.io/Mays-s-Portfolio/) · [LinkedIn](https://www.linkedin.com/in/mays-owais-718bb725a)

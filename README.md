# 🎓 Credit Risk Scoring — Random Forest vs. Logistic Regression

> 📦 **Old code, fresh eyes.** This is the R code from my Bachelor's thesis (Università di Trento, Economics & Management). I'm slowly restyling and cleaning it up — so expect some rough edges while it's a work in progress. 🚧

## 📝 What this is

A comparison of two approaches to **credit risk scoring** on the [German Credit dataset](https://archive.ics.uci.edu/dataset/144/statlog+german+credit+data):

- 📈 **Logistic Regression** — built via manual backward elimination, using **AIC**, **BIC**, and the **Gini coefficient** (`2·AUC − 1`) as selection criteria.
- 🌳 **Random Forest** — with variable importance (Mean Decrease in Gini) and out-of-the-box prediction.

The data is first split **60/40 into training and test sets** (`set.seed(123)`): the logit is built on the training set and its discriminatory power (Gini) is assessed out-of-sample on the test set. Both models are then additionally validated with **10-fold cross-validation** and compared on misclassification error. 🎯

## 🗂️ Contents

- `thesis_script.R` — the full pipeline: data prep → logit model building → random forest → cross-validation.
- 📊 Outputs: ROC curves (initial vs. final logit), variable importance plot, CV error tables.

## ⚙️ How to run

You'll need: `ROCR`, `pROC`, `randomForest`, `readxl`, `boot`, `ggplot2`, `knitr`.

```r
install.packages(c("ROCR", "pROC", "randomForest", "readxl", "boot", "ggplot2", "knitr"))
```

📥 **Get the data:** download the CLEANED dataset folder from [Dropbox](https://www.dropbox.com/scl/fo/yk1gpw43jxzgflge85lv5/ALD42OHbTLizrDMtNtyBGk4?rlkey=58wlu57whlu2qvd3ti1x3rpa6&st=9ubwiw1c&dl=0).

⚠️ **Heads up:** the current version uses hardcoded local paths. Pointing the data path to your own copy of the dataset is needed until I migrate to relative paths (see roadmap). 📁

## 🛠️ Restyling roadmap

- [ ] 🧹 Replace `setwd()` + absolute paths with `here::here()` for portability
- [ ] ♻️ Refactor the repetitive `glm()` calls into a function / loop
- [ ] 🧪 Make results fully reproducible (seeds, session info)
- [ ] 📂 Reorganise into `data/`, `R/`, `output/` folders

## 📜 Note

This is **archived student work** — kept for transparency and as a record of where I started. The methodology reflects what I knew at the time, not necessarily what I'd do today. 🌱

---
*Università di Trento · Economics & Management · 

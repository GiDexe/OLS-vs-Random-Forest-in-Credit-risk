# ============================================================
# Credit Risk Scoring — Logistic Regression vs. Random Forest
# Bachelor thesis appendix script (restyled with here())
# ============================================================

# --- Dependencies: install any that are missing, then load all at once ---
required_packages <- c(
  "here", "ROCR", "readxl", "vctrs", "pROC",
  "boot", "knitr", "randomForest", "ggplot2"
)
to_install <- setdiff(required_packages, rownames(installed.packages()))
if (length(to_install) > 0) install.packages(to_install)
invisible(lapply(required_packages, library, character.only = TRUE))

# Write generated artifacts to Thesis/output/
dir.create(here("output"), showWarnings = FALSE)

td <- read_excel(here("data", "german_credit_clean.xlsx"))
data <- td
colnames(data) <- c("Status acount", "Duration", "Credit history", "Purpose", "Credit amount", "Savings account/bonds", "Present employment since", "Installment rate in percentage of disposable income", "Personal status and sex", "Other debtors / guarantors", "Present residence since", "Property", "Age", "Other installment plans", "Housing", "Number of existing credits at this bank", "Job", "Number of people being liable to provide maintenance for", "Telephone", "foreign worker", "class")

# Accommodating R's framework by providing a binary outcome
data$class <- ifelse(data$class == 1, 1, 0)


# Splitting the sample: generate a random sample of row indices; set seed to reproduce it
set.seed(123)

d <- sort(sample(nrow(data), nrow(data) * 0.6))
# Select training sample
train <- data[d, ]
test  <- data[-d, ]

# Regression for the Test. The one that will be used to compute this model's Gini
Logitest <- glm(class ~ `Status acount` + Duration + `Credit history` + Purpose + `Credit amount` + `Savings account/bonds` + `Present employment since` + `Installment rate in percentage of disposable income` + `Personal status and sex` + `Other debtors / guarantors` + `Present residence since` + Property + Age + `Other installment plans` + Housing + `Number of existing credits at this bank` + Job + `Number of people being liable to provide maintenance for` + Telephone + `foreign worker`, data = test, family = binomial)
summary(Logitest)

summary_output <- capture.output(summary(Logitest))

# Write the output to a text file
writeLines(summary_output, here("output", "logit_regression_summary.txt"))

# Function to instantly get the Gini coefficient. Useful as it is recomputed every time the model gets restricted
Ginicoef <- function(test_data, model) {
  test_data$score <- predict(model, type = "response", newdata = test_data)
  pred_TEST <- prediction(test_data$score, test_data$class)
  auc_value_TEST <- performance(pred_TEST, "auc")@y.values[[1]]
  gini_coefficient_TEST <- 2 * auc_value_TEST - 1
  return(gini_coefficient_TEST)
}

GINI_UNR <- Ginicoef(test, Logitest)
# First Gini 0.74

# First regression to get an idea on how to proceed
Logitrain <- glm(class ~ `Status acount` + Duration + `Credit history` + Purpose + `Credit amount` + `Savings account/bonds` + `Present employment since` + `Installment rate in percentage of disposable income` + `Personal status and sex` + `Other debtors / guarantors` + `Present residence since` + Property + Age + `Other installment plans` + Housing + `Number of existing credits at this bank` + Job + `Number of people being liable to provide maintenance for` + `foreign worker`, data = train, family = binomial)
LOGITR <- summary(Logitrain)
saveRDS(Logitrain, file = here("output", "Logitrain.rds"))

# Obtain predicted probabilities
predicted_probabilities_start <- predict(Logitest, type = "response")

# Compute ROC curve
roc_curve_start <- roc(test$class, predicted_probabilities_start)
saveRDS(roc_curve_start, file = here("output", "roc_curvest.rds"))

# It is now possible to proceed to the elimination.
# Gini, BIC and AIC will be used as criteria
summary(Logitrain)

# Reducing dummies for PURPOSE: CAR NEW, CAR USED, BUSINESS, TV/RADIO, OTHER
new_purpose <- function(Purpose) {
  ifelse(Purpose == "A40", "car new",
         ifelse(Purpose == "A41", "car used",
                ifelse(Purpose == "A43", "TV/RADIO",
                       ifelse(Purpose == "A49", "BUSINESS", "other"))))
}
train1 <- train
test1  <- test
train1$Purpose <- new_purpose(train1$Purpose)
test1$Purpose  <- new_purpose(test1$Purpose)

logitest1 <- glm(class ~ `Status acount` + Duration + `Credit history` + Purpose + `Credit amount` + `Savings account/bonds` + `Present employment since` + `Installment rate in percentage of disposable income` + `Personal status and sex` + `Other debtors / guarantors` + `Present residence since` + Property + Age + `Other installment plans` + Housing + `Number of existing credits at this bank` + Job + `Number of people being liable to provide maintenance for` + Telephone + `foreign worker`, data = test1, family = binomial)
BIC(logitest1)
AIC(logitest1)
BIC(Logitest)
AIC(Logitest)
Ginicoef(test1, logitest1)
GINI_UNR
# Gini decreases but not that much. BIC is better for the restricted model. Proceed with the simplification
logitrain1 <- glm(class ~ `Status acount` + Duration + `Credit history` + Purpose + `Credit amount` + `Savings account/bonds` + `Present employment since` + `Installment rate in percentage of disposable income` + `Personal status and sex` + `Other debtors / guarantors` + `Present residence since` + Property + Age + `Other installment plans` + Housing + `Number of existing credits at this bank` + Job + `Number of people being liable to provide maintenance for` + Telephone + `foreign worker`, data = train1, family = binomial)
summary(logitrain1)

new_ch <- function(`Credit history`) {
  ifelse(`Credit history` == "A34", "critical/others existing", "no issues/minor issues")
}
test1$`Credit history`  <- new_ch(test1$`Credit history`)
train1$`Credit history` <- new_ch(train1$`Credit history`)
logitrain2 <- glm(class ~ `Status acount` + Duration + `Credit history` + Purpose + `Credit amount` + `Savings account/bonds` + `Present employment since` + `Installment rate in percentage of disposable income` + `Personal status and sex` + `Other debtors / guarantors` + `Present residence since` + Property + Age + `Other installment plans` + Housing + `Number of existing credits at this bank` + Job + `Number of people being liable to provide maintenance for` + Telephone + `foreign worker`, data = train1, family = binomial)
summary(logitrain2)
logitest2 <- glm(class ~ `Status acount` + Duration + `Credit history` + Purpose + `Credit amount` + `Savings account/bonds` + `Present employment since` + `Installment rate in percentage of disposable income` + `Personal status and sex` + `Other debtors / guarantors` + `Present residence since` + Property + Age + `Other installment plans` + Housing + `Number of existing credits at this bank` + Job + `Number of people being liable to provide maintenance for` + Telephone + `foreign worker`, data = test1, family = binomial)
BIC(logitest2)
AIC(logitest2)
BIC(logitest1)
AIC(logitest1)
Ginicoef(test1, logitest2)
# Same as before. Gini and BIC improve.
logitrain2 <- glm(class ~ `Status acount` + Duration + `Credit history` + Purpose + `Credit amount` + `Savings account/bonds` + `Present employment since` + `Installment rate in percentage of disposable income` + `Personal status and sex` + `Other debtors / guarantors` + `Present residence since` + Property + Age + `Other installment plans` + Housing + `Number of existing credits at this bank` + Job + `Number of people being liable to provide maintenance for` + Telephone + `foreign worker`, data = train1, family = binomial)
summary(logitrain2)

# Now operating on present employment
new_pes <- function(`Present employment since`) {
  ifelse(`Present employment since` == "A71", "unemployed",
         ifelse(`Present employment since` == "A72", "<1y", ">=1y"))
}
test1$`Present employment since`  <- new_pes(test1$`Present employment since`)
train1$`Present employment since` <- new_pes(train1$`Present employment since`)
logitrain3 <- glm(class ~ `Status acount` + Duration + `Credit history` + Purpose + `Credit amount` + `Savings account/bonds` + `Present employment since` + `Installment rate in percentage of disposable income` + `Personal status and sex` + `Other debtors / guarantors` + `Present residence since` + Property + Age + `Other installment plans` + Housing + `Number of existing credits at this bank` + Job + `Number of people being liable to provide maintenance for` + Telephone + `foreign worker`, data = train1, family = binomial)
summary(logitrain3)
logitest3 <- glm(class ~ `Status acount` + Duration + `Credit history` + Purpose + `Credit amount` + `Savings account/bonds` + `Present employment since` + `Installment rate in percentage of disposable income` + `Personal status and sex` + `Other debtors / guarantors` + `Present residence since` + Property + Age + `Other installment plans` + Housing + `Number of existing credits at this bank` + Job + `Number of people being liable to provide maintenance for` + Telephone + `foreign worker`, data = test1, family = binomial)
BIC(logitest3)
AIC(logitest3)
BIC(logitest2)
AIC(logitest2)
Ginicoef(test1, logitest3)
# Gini decreases, however so does BIC. KEEP UP
summary(logitrain3)

new_sab <- function(`Savings account/bonds`) {
  ifelse(`Savings account/bonds` == "A65", "unknown/absent", "present")
}
test1$`Savings account/bonds`  <- new_sab(test1$`Savings account/bonds`)
train1$`Savings account/bonds` <- new_sab(train1$`Savings account/bonds`)

logitrain4 <- glm(class ~ `Status acount` + Duration + `Credit history` + Purpose + `Credit amount` + `Savings account/bonds` + `Present employment since` + `Installment rate in percentage of disposable income` + `Personal status and sex` + `Other debtors / guarantors` + `Present residence since` + Property + Age + `Other installment plans` + Housing + `Number of existing credits at this bank` + Job + `Number of people being liable to provide maintenance for` + Telephone + `foreign worker`, data = train1, family = binomial)
summary(logitrain4)
logitest4 <- glm(class ~ `Status acount` + Duration + `Credit history` + Purpose + `Credit amount` + `Savings account/bonds` + `Present employment since` + `Installment rate in percentage of disposable income` + `Personal status and sex` + `Other debtors / guarantors` + `Present residence since` + Property + Age + `Other installment plans` + Housing + `Number of existing credits at this bank` + Job + `Number of people being liable to provide maintenance for` + Telephone + `foreign worker`, data = test1, family = binomial)
BIC(logitest4)
AIC(logitest4)
BIC(logitest3)
AIC(logitest3)
Ginicoef(test1, logitest4)
# Still improving for BIC. In Gini's terms it does not get too weak. Go on
# Personal Status and Sex looks not significant and with many dummies. To be deleted here

logitrain5 <- glm(class ~ `Status acount` + Duration + `Credit history` + Purpose + `Credit amount` + `Savings account/bonds` + `Present employment since` + `Installment rate in percentage of disposable income` + `Other debtors / guarantors` + `Present residence since` + Property + Age + `Other installment plans` + Housing + `Number of existing credits at this bank` + Job + `Number of people being liable to provide maintenance for` + Telephone + `foreign worker`, data = train1, family = binomial)
summary(logitrain5)
logitest5 <- glm(class ~ `Status acount` + Duration + `Credit history` + Purpose + `Credit amount` + `Savings account/bonds` + `Present employment since` + `Installment rate in percentage of disposable income` + `Other debtors / guarantors` + `Present residence since` + Property + Age + `Other installment plans` + Housing + `Number of existing credits at this bank` + Job + `Number of people being liable to provide maintenance for` + Telephone + `foreign worker`, data = test1, family = binomial)
BIC(logitest4)
AIC(logitest4)
BIC(logitest5)
AIC(logitest5)
Ginicoef(test1, logitest5)
# ALL THE INDICATORS SHOW AN IMPROVEMENT
# Next variable to be deleted: JOB
logitrain6 <- glm(class ~ `Status acount` + Duration + `Credit history` + Purpose + `Credit amount` + `Savings account/bonds` + `Present employment since` + `Installment rate in percentage of disposable income` + `Other debtors / guarantors` + `Present residence since` + Property + Age + `Other installment plans` + Housing + `Number of existing credits at this bank` + `Number of people being liable to provide maintenance for` + Telephone + `foreign worker`, data = train1, family = binomial)
summary(logitrain6)
logitest6 <- glm(class ~ `Status acount` + Duration + `Credit history` + Purpose + `Credit amount` + `Savings account/bonds` + `Present employment since` + `Installment rate in percentage of disposable income` + `Other debtors / guarantors` + `Present residence since` + Property + Age + `Other installment plans` + Housing + `Number of existing credits at this bank` + `Number of people being liable to provide maintenance for` + Telephone + `foreign worker`, data = test1, family = binomial)
BIC(logitest6)
AIC(logitest6)
BIC(logitest5)
AIC(logitest5)
Ginicoef(test1, logitest6)
# Extremely small decrease in the Gini. There is however a solid improvement in terms of BIC and AIC


# Property
logitrain7 <- glm(class ~ `Status acount` + Duration + `Credit history` + Purpose + `Credit amount` + `Savings account/bonds` + `Present employment since` + `Installment rate in percentage of disposable income` + `Other debtors / guarantors` + `Present residence since` + Age + `Other installment plans` + Housing + `Number of existing credits at this bank` + `Number of people being liable to provide maintenance for` + Telephone + `foreign worker`, data = train1, family = binomial)
summary(logitrain7)
logitest7 <- glm(class ~ `Status acount` + Duration + `Credit history` + Purpose + `Credit amount` + `Savings account/bonds` + `Present employment since` + `Installment rate in percentage of disposable income` + `Other debtors / guarantors` + `Present residence since` + Age + `Other installment plans` + Housing + `Number of existing credits at this bank` + `Number of people being liable to provide maintenance for` + Telephone + `foreign worker`, data = test1, family = binomial)
BIC(logitest6)
AIC(logitest6)
BIC(logitest7)
AIC(logitest7)
Ginicoef(test1, logitest7)
# Both BIC and AIC improving as GINI almost stays the same

# Other installment plans
# Everything worsens, thus going back

# Removing Purpose as it is not significant anymore
logitest8 <- glm(class ~ `Status acount` + Duration + `Credit history` + `Credit amount` + `Savings account/bonds` + `Present employment since` + `Installment rate in percentage of disposable income` + `Other debtors / guarantors` + `Present residence since` + Age + `Other installment plans` + Housing + `Number of existing credits at this bank` + `Number of people being liable to provide maintenance for` + Telephone + `foreign worker`, data = test1, family = binomial)
logitrain8 <- glm(class ~ `Status acount` + Duration + `Credit history` + `Credit amount` + `Savings account/bonds` + `Present employment since` + `Installment rate in percentage of disposable income` + `Other debtors / guarantors` + `Present residence since` + Age + `Other installment plans` + Housing + `Number of existing credits at this bank` + `Number of people being liable to provide maintenance for` + Telephone + `foreign worker`, data = train1, family = binomial)
summary(logitrain8)
BIC(logitest8)
AIC(logitest8)
BIC(logitest7)
AIC(logitest7)
Ginicoef(test1, logitest8)


# Removing HOUSING
logitest9 <- glm(class ~ `Status acount` + Duration + `Credit history` + `Credit amount` + `Savings account/bonds` + `Present employment since` + `Installment rate in percentage of disposable income` + `Other debtors / guarantors` + `Present residence since` + Age + `Other installment plans` + `Number of existing credits at this bank` + `Number of people being liable to provide maintenance for` + Telephone + `foreign worker`, data = test1, family = binomial)
BIC(logitest8)
AIC(logitest8)
BIC(logitest9)
AIC(logitest9)
Ginicoef(test1, logitest9)
# General improvement after the removal of HOUSING. ACCEPTED
logitrain9 <- glm(class ~ `Status acount` + Duration + `Credit history` + `Credit amount` + `Savings account/bonds` + `Present employment since` + `Installment rate in percentage of disposable income` + `Other debtors / guarantors` + `Present residence since` + Age + `Other installment plans` + `Number of existing credits at this bank` + `Number of people being liable to provide maintenance for` + Telephone + `foreign worker`, data = train1, family = binomial)
summary(logitrain9)
# OTHER INSTALLMENT PLANS SHALL NOT BE REMOVED ACCORDING TO BIC AND AIC
logitest10 <- glm(class ~ `Status acount` + Duration + `Credit history` + `Credit amount` + `Savings account/bonds` + `Present employment since` + `Installment rate in percentage of disposable income` + `Other debtors / guarantors` + `Present residence since` + Age + `Number of existing credits at this bank` + `Number of people being liable to provide maintenance for` + Telephone + `foreign worker`, data = test1, family = binomial)
BIC(logitest10)
AIC(logitest10)
BIC(logitest9)
AIC(logitest9)
Ginicoef(test1, logitest10)

summary(logitrain9)
# THUS KEEPING OTH INST PLANS I REMOVE: N OF PEOPLE LIABLE MAINT, N OF CREDITS AT THIS BANK AND AGE. TELEPHONE IS KEPT.
logitest11 <- glm(class ~ `Status acount` + Duration + `Credit history` + `Credit amount` + `Savings account/bonds` + `Present employment since` + `Installment rate in percentage of disposable income` + `Other debtors / guarantors` + `Present residence since` + `Other installment plans` + Telephone, data = test1, family = binomial)
Ginicoef(test1, logitest11)
BIC(logitest10)
AIC(logitest10)
BIC(logitest11)
AIC(logitest11)

logitrain11 <- glm(class ~ `Status acount` + Duration + `Credit history` + `Credit amount` + `Savings account/bonds` + `Present employment since` + `Installment rate in percentage of disposable income` + `Other debtors / guarantors` + `Present residence since` + `Other installment plans` + Telephone, data = train1, family = binomial)
summary(logitrain11)

# REMOVING PRESENT RESIDENCE SINCE
logitest12 <- glm(class ~ `Status acount` + Duration + `Credit history` + `Credit amount` + `Savings account/bonds` + `Present employment since` + `Installment rate in percentage of disposable income` + `Other debtors / guarantors` + `Other installment plans` + Telephone, data = test1, family = binomial)
Ginicoef(test1, logitest12)
BIC(logitest12)
AIC(logitest12)
BIC(logitest11)
AIC(logitest11)
logitrain12 <- glm(class ~ `Status acount` + Duration + `Credit history` + `Credit amount` + `Savings account/bonds` + `Present employment since` + `Installment rate in percentage of disposable income` + `Other debtors / guarantors` + `Other installment plans` + Telephone, data = train1, family = binomial)
summary(logitrain12)


# REMOVING DURATION
logitest13 <- glm(class ~ `Status acount` + `Credit history` + `Credit amount` + `Savings account/bonds` + `Present employment since` + `Installment rate in percentage of disposable income` + `Other debtors / guarantors` + `Other installment plans` + Telephone, data = test1, family = binomial)
Ginicoef(test1, logitest13)
BIC(logitest12)
AIC(logitest12)
BIC(logitest13)
AIC(logitest13)

logitrain13 <- glm(class ~ `Status acount` + `Credit history` + `Credit amount` + `Savings account/bonds` + `Present employment since` + `Installment rate in percentage of disposable income` + `Other debtors / guarantors` + `Other installment plans` + Telephone, data = test1, family = binomial)
summary(logitrain13)


# REMOVING TELEPHONE
logitest14 <- glm(class ~ `Status acount` + `Credit history` + `Credit amount` + `Savings account/bonds` + `Present employment since` + `Installment rate in percentage of disposable income` + `Other debtors / guarantors` + `Other installment plans`, data = test1, family = binomial)
Ginicoef(test1, logitest14)
BIC(logitest14)
AIC(logitest14)
BIC(logitest13)
AIC(logitest13)
logitrain14 <- glm(class ~ `Status acount` + `Credit history` + `Credit amount` + `Savings account/bonds` + `Present employment since` + `Installment rate in percentage of disposable income` + `Other debtors / guarantors` + `Other installment plans`, data = train1, family = binomial)
summary(logitrain14)
saveRDS(logitrain14, file = here("output", "logitrain14.rds"))
### END OF THE MODEL BUILDING WITH LOGISTIC REGRESSION


# ANOTHER (FAILED) ATTEMPT TO REMOVE OTHER INSTALLMENT PLANS
logitest15 <- glm(class ~ `Status acount` + `Credit history` + `Credit amount` + `Savings account/bonds` + `Present employment since` + `Installment rate in percentage of disposable income` + `Other debtors / guarantors`, data = test1, family = binomial)
Ginicoef(test1, logitest15)
BIC(logitest14)
AIC(logitest14)
BIC(logitest15)
AIC(logitest15)

# ROC CURVE START
plot(roc_curve_start, main = "Initial ROC Curve", col = "blue", lwd = 2)

# ROC CURVE END
predicted_probabilities_end <- predict(logitest14, type = "response")

# Compute ROC curve
roc_curve_end <- roc(test1$class, predicted_probabilities_end)
saveRDS(roc_curve_end, file = here("output", "roc_curveend.rds"))
plot(roc_curve_end, main = "Final ROC Curve", col = "blue", lwd = 2)


# ============================================================
# Cross validation — Logistic Regression
# ============================================================
logitest14 <- glm(class ~ `Status acount` + `Credit history` + `Credit amount` + `Savings account/bonds` + `Present employment since` + `Installment rate in percentage of disposable income` + `Other debtors / guarantors` + `Other installment plans`, data = test1, family = binomial)
logitrain14 <- glm(class ~ `Status acount` + `Credit history` + `Credit amount` + `Savings account/bonds` + `Present employment since` + `Installment rate in percentage of disposable income` + `Other debtors / guarantors` + `Other installment plans`, data = train1, family = binomial)
summary(logitrain14)

data1 <- rbind(train1, test1)
data1 <- data1[, -22]

# Define the logistic regression formula
formula <- class ~ `Status acount` + `Credit history` + `Credit amount` + `Savings account/bonds` + `Present employment since` + `Installment rate in percentage of disposable income` + `Other debtors / guarantors` + `Other installment plans`

# Initialize vector to store cross-validation errors
cv_error <- rep(0, 10)

# K-fold cross-validation
set.seed(17)
k <- 10  # number of folds

for (i in 1:k) {
  # Split data into training and testing sets for this fold
  folds <- sample(1:k, nrow(data1), replace = TRUE)
  train_indices <- which(folds != i)
  test_indices  <- which(folds == i)
  train_data <- data1[train_indices, ]
  test_data  <- data1[test_indices, ]

  # Fit logistic regression model on training data
  glm.fit <- glm(formula, data = train_data, family = binomial)

  # Predict
  pred <- predict(glm.fit, newdata = test_data, type = "response")

  # Misclassification error
  cv_error[i] <- mean((pred > 0.5) != (test_data$class == 1))
}

# Display the cross-validation errors
print(cv_error)

# Mean of cross-validation error estimates
cv_mean_LOGIT <- mean(cv_error)
# Mean as the name of the last row
cv_data_LOGITMKD <- rbind(cv_data_LOGITMKD, c("Mean" = cv_mean_LOGIT))

# Create a data frame with cross-validation error estimates and mean
cv_data_LOGIT <- data.frame(
  "K-Fold Logit Estimate" = c(cv_error, cv_mean_LOGIT)
)

# Save the data frame as a CSV file
write.csv(cv_data_LOGIT, file = here("output", "cv_error_table.csv"), row.names = FALSE)


# ============================================================
# Random Forests
# ============================================================
rm(list = ls())

td <- read_excel(here("data", "german_credit_clean.xlsx"))
data <- td
colnames(data) <- c("Status_account", "Duration", "Credit_history", "Purpose", "Credit_amount", "Savings_account_bonds", "Present_employment_since", "Installment_rate_in_percentage_of_disposable_income", "Personal_status_and_sex", "Other_debtors_guarantors", "Present_residence_since", "Property", "Age", "Other_installment_plans", "Housing", "Number_of_existing_credits_at_this_bank", "Job", "N_maintenance_for", "Telephone", "foreign_worker", "class")
# Accommodating R's framework by providing a binary outcome
data$class <- ifelse(data$class == 1, 1, 0)
data <- data[, -22]
data$class <- factor(data$class)

set.seed(95)
GCR.train <- sample(1:nrow(data), nrow(data) / 2)
GCR.test  <- data[-GCR.train, "class"]

rf.GCR <- randomForest(class ~ ., data = data, subset = GCR.train, mtry = sqrt(20), importance = TRUE)
yhatGCR.rf <- predict(rf.GCR, newdata = data[-GCR.train, ])

# Plot variable importance
importance(rf.GCR)

# Extract variable importance data
var_importance <- rf.GCR$importance

# Convert variable importance to data frame
var_importance_df <- data.frame(Variable = rownames(var_importance), Importance = var_importance[, "MeanDecreaseGini"])

# Plot variable importance using ggplot2
plot_RF <- ggplot(var_importance_df, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8)) +
  labs(x = "Variable", y = "Importance", title = "Variable Importance")
ggsave(here("output", "plot_RF.png"), plot_RF)

# An idea for Gini computation
# Define the confusion matrix
conf_matrix <- matrix(c(66, 84, 47, 303), nrow = 2, byrow = TRUE)

# Proportions of each class
class_proportions <- colSums(conf_matrix) / sum(conf_matrix)

# Gini impurity
gini_impurity <- 1 - sum(class_proportions^2)

# The result
print(paste("Gini impurity:", round(gini_impurity, 4)))


# ============================================================
# Cross validation — Random Forest
# ============================================================
rm(list = ls())

# Load data
td <- read_excel(here("data", "german_credit_clean.xlsx"))
data <- td
colnames(data) <- c("Status_account", "Duration", "Credit_history", "Purpose", "Credit_amount", "Savings_account_bonds", "Present_employment_since", "Installment_rate_in_percentage_of_disposable_income", "Personal_status_and_sex", "Other_debtors_guarantors", "Present_residence_since", "Property", "Age", "Other_installment_plans", "Housing", "Number_of_existing_credits_at_this_bank", "Job", "N_maintenance_for", "Telephone", "foreign_worker", "class")
data$class <- ifelse(data$class == 1, 1, 0)  # Accommodating R's framework by providing a binary outcome
data <- data[, -22]
data$class <- factor(data$class)

# Store cross-validation errors
cv_error_rf <- rep(0, 10)

# K-fold cross-validation
set.seed(95)
k <- 10  # n of folds
for (i in 1:k) {
  # Split data into training and testing sets for this fold
  folds <- sample(1:k, nrow(data), replace = TRUE)
  train_indices <- which(folds != i)
  test_indices  <- which(folds == i)
  train_data <- data[train_indices, ]
  test_data  <- data[test_indices, ]

  # Fit random forest model on training data
  rf_model <- randomForest(class ~ ., data = train_data, mtry = sqrt(20))

  # Predict
  pred <- predict(rf_model, newdata = test_data)

  # Misclassification error
  cv_error_rf[i] <- mean(pred != test_data$class)
}

# Display the cross-validation errors
print(cv_error_rf)

cv_mean_rf <- mean(cv_error_rf)

# Create a data frame with the cross-validation error estimates and the mean
cv_data_rf <- data.frame(
  "Random Forest Estimate" = c(cv_error_rf, cv_mean_rf)
)

# Change in colname
colnames(cv_data_rf) <- c("K-Fold Random Forest Estimates")

# CSV file
write.csv(cv_data_rf, file = here("output", "cv_error_rf_table.csv"), row.names = FALSE)

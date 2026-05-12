library(dplyr)
library(car)
library(mgcv)
library(randomForest)
library(ggplot2)
library(purrr)

# Dataset
obesity_raw <- read.csv("ObesityDataSet_raw_and_data_sinthetic.csv")
# Data cleaning
# Calculate BMI
obesity <- obesity_raw %>% mutate(BMI = Weight/Height^2) %>%
  # Select variables of interest only
  select(-c(Weight, Height,NObeyesdad)) %>%
  # Convert into factors
  mutate(across(where(is.character), as.factor))
# BMI Distribution
hist(obesity$BMI,breaks = 5)
boxCox(lm(BMI~.,data = obesity),lambda = seq(0.1,0.8,0.1))
par(mfrow = c(1,4))
plot(lm(BMI~.,data = obesity))
plot(lm(sqrt(BMI)~.,data = obesity))
# Split data
set.seed(1234)
train_id <- sample(1:nrow(obesity),size = 0.8*nrow(obesity))
train <- obesity[train_id,]
valid <- obesity[-train_id,]

# Linear Regression
lm_mod <- lm(BMI~.,data = train)
summary(lm_mod)
# Diagnostic Plots
plot(lm_mod)
# VIF
vif(lm_mod)
# Outlier Test
outlierTest(lm_mod)
# RMSE
lm_pred <- predict(lm_mod, newdata = valid)
sqrt(mean((lm_pred - valid$BMI)^2))

# Explore non-linear variables
num_vars <- obesity %>%
  select(where(is.numeric)) %>%
  select(-BMI) %>%
  names()
# Plots
plots <- map(num_vars, function(v){
  obesity %>%
    group_by(x = round(.data[[v]])) %>%
    summarise(m_BMI = mean(BMI, na.rm = TRUE)) %>%
    ggplot(aes(x, m_BMI)) +
    geom_point() +
    geom_smooth(method = "lm",formula = y ~ poly(x,2),se = FALSE) +
    labs(x = v, y = "Mean BMI", title = paste("Mean BMI vs", v)) 
})
plots

# Polynomial Regression
poly_mod <- lm(BMI~.-Age-NCP-TUE+poly(Age,2)+poly(NCP,2)+poly(TUE,2),data = train)
summary(poly_mod)
# Diagnostic Plots
plot(poly_mod)
# Outlier Test
outlierTest(poly_mod)
# RMSE
poly_pred <- predict(poly_mod, newdata = valid)
sqrt(mean((poly_pred - valid$BMI)^2))

# Generalized Additive Models
gam_mod <- gam(BMI ~Gender + family_history_with_overweight+FAVC+
                 FCVC+CAEC+SMOKE+CH2O+SCC+FAF+ CALC+ MTRANS+
                 s(Age)+s(NCP)+s(TUE),data = train)
summary(gam_mod)
# RMSE
gam_pred <- predict(gam_mod, newdata = valid)
sqrt(mean((gam_pred - valid$BMI)^2))

# Random Forest
set.seed(1234)
rf_mod <- randomForest(BMI~.,data = train,importance = T)
rf_mod
#RMSE
rf_pred <- predict(rf_mod, newdata = valid)
sqrt(mean((rf_pred - valid$BMI)^2))
# Variable importance
importance(rf_mod)
varImpPlot(rf_mod)


# Final Year Project - S&P 1500 Analysis
# Christy Cargill 


# data loading and cleaning

library(dplyr)
library(readxl) 
library(janitor)

df <- read_excel("Desktop/SPGlobal_1500.xlsx", skip = 4) %>%
  clean_names()

# manually renaming columns

new_names <- c(
  "entity_name", "entity_id", "total_revenue_growth_percent", "return_on_equity_percent",
  "geography", "sector", "debt_to_equity_ratio", "credit_rating", "beta_one_year",
  "business_involvement_revenue_percent", "ebitda_interest_expense",
  "total_enterprise_value_million", "price_eps", "beta_three_year",
  "total_enterprise_value", "debt_to_ebitda", "interest_expense_thousand",
  "tev_to_ebitda", "quick_ratio", "ebit_margin_percent", "gross_margin_percent",
  "leverage_ratio_percent", "roace_percent"
)

names(df) <- new_names

# recode different placeholders for missing values as NA
df <- df %>%
  mutate(across(where(is.character), ~ na_if(., ""))) %>%
  mutate(across(where(is.character), ~ na_if(., "NA"))) %>%
  mutate(across(where(is.character), ~ na_if(., "N/A"))) %>%
  mutate(across(where(is.character), ~ na_if(., "NM"))) %>%
  mutate(across(where(is.character), ~ na_if(., " "))) %>%
  mutate(across(where(is.character), ~ na_if(., "-")))

num_vars <- c(
  "total_revenue_growth_percent", "return_on_equity_percent",
  "debt_to_equity_ratio", "beta_one_year", "business_involvement_revenue_percent",
  "ebitda_interest_expense", "total_enterprise_value_million",
  "price_eps", "beta_three_year", "total_enterprise_value",
  "debt_to_ebitda", "interest_expense_thousand", "tev_to_ebitda",
  "quick_ratio", "ebit_margin_percent", "gross_margin_percent",
  "leverage_ratio_percent", "roace_percent"
)

df <- df %>%
  mutate(across(all_of(num_vars), as.numeric))




# IDA - proportions for categorical vars

df %>%
  group_by(sector) %>%
  summarise(n = n()) %>%
  mutate(pct = n / sum(n) * 100)

df %>%
  group_by(geography) %>%
  summarise(n = n()) %>%
  mutate(pct = n / sum(n) * 100)

df %>%
  group_by(credit_rating) %>%
  summarise(n = n()) %>%
  mutate(pct = n / sum(n) * 100)


# IDA - summary stats for continuous vars

library(Hmisc)   

summarise_continuous <- function(data, var) {
  x <- data[[var]]
  
  tibble(
    variable = var,
    mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    iqr = IQR(x, na.rm = TRUE),
    gini_mean_diff = GiniMd(x, na.rm = TRUE),
    q01 = quantile(x, 0.01, na.rm = TRUE),
    q05 = quantile(x, 0.05, na.rm = TRUE),
    q25 = quantile(x, 0.25, na.rm = TRUE),
    median = quantile(x, 0.50, na.rm = TRUE),
    q75 = quantile(x, 0.75, na.rm = TRUE),
    q95 = quantile(x, 0.95, na.rm = TRUE),
    q99 = quantile(x, 0.99, na.rm = TRUE),
  )
}

summarise_continuous(df, "ebit_margin_percent")
summarise_continuous(df, "debt_to_equity_ratio")
summarise_continuous(df, "beta_one_year")
summarise_continuous(df, "beta_three_year")
summarise_continuous(df, "business_involvement_revenue_percent")
summarise_continuous(df, "debt_to_ebitda")
summarise_continuous(df, "ebitda_interest_expense")
summarise_continuous(df, "gross_margin_percent")
summarise_continuous(df, "interest_expense_thousand")
summarise_continuous(df, "leverage_ratio_percent")
summarise_continuous(df, "price_eps")
summarise_continuous(df, "quick_ratio")
summarise_continuous(df, "return_on_equity_percent")
summarise_continuous(df, "roace_percent")
summarise_continuous(df, "tev_to_ebitda")
summarise_continuous(df, "total_enterprise_value")
summarise_continuous(df, "total_revenue_growth_percent")


# histograms of continuous vars

vars <- c(
  "ebit_margin_percent",
  "debt_to_equity_ratio",
  "beta_one_year",
  "beta_three_year",
  "business_involvement_revenue_percent",
  "debt_to_ebitda",
  "ebitda_interest_expense",
  "gross_margin_percent",
  "interest_expense_thousand",
  "leverage_ratio_percent",
  "price_eps",
  "quick_ratio",
  "return_on_equity_percent",
  "roace_percent",
  "tev_to_ebitda",
  "total_enterprise_value",
  "total_revenue_growth_percent"
)

library(tidyr)
library(ggplot2)

df_long <- df %>%
  dplyr::select(all_of(vars)) %>%
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "value"
  ) %>%
  dplyr::filter(is.finite(value))


ggplot(df_long, aes(x = value)) +
  geom_histogram(bins = 100, fill = "grey70", color = "blue") +
  facet_wrap(~ variable, scales = "free", ncol = 4) +
  theme_minimal() +
  labs(
    title = "Distributions of Continuous Financial Variables",
    x = NULL,
    y = "Count"
  ) +
  theme(
    strip.text = element_text(size = 9),
    axis.text.x = element_text(size = 7)
  )


# spearman correlation matrix and heatmap

cont_vars <- df %>%
  dplyr::select(
    ebit_margin_percent,
    total_revenue_growth_percent,
    gross_margin_percent,
    return_on_equity_percent,
    roace_percent,
    debt_to_equity_ratio,
    debt_to_ebitda,
    quick_ratio,
    price_eps,
    tev_to_ebitda,
    interest_expense_thousand,
    beta_one_year,
    beta_three_year,
    business_involvement_revenue_percent,
    total_enterprise_value
  )


cor_spearman <- cor(
  cont_vars,
  method = "spearman",
  use = "pairwise.complete.obs"
)

round(cor_spearman, 2)


cor_long <- as.data.frame(cor_spearman) %>%
  mutate(var1 = rownames(.)) %>%
  pivot_longer(-var1, names_to = "var2", values_to = "correlation")

ggplot(cor_long, aes(var1, var2, fill = correlation)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    title = "Spearman Correlation Matrix of Financial Variables",
    x = "",
    y = "",
    fill = "ρ"
  )


# finding firms with extreme values for each variable

inspect_extremes <- function(data, var, n = 5) {
  
  data_clean <- data %>%
    filter(!is.na({{ var }}))
  
  bind_rows(
    data_clean %>%
      arrange({{ var }}) %>%
      slice_head(n = n),
    
    data_clean %>%
      arrange(desc({{ var }})) %>%
      slice_head(n = n)
  ) %>%
    select(
      entity_id,
      sector,
      total_enterprise_value_million,
      return_on_equity_percent,
      ebit_margin_percent,
      debt_to_equity_ratio,
      {{ var }}
    ) %>%
    mutate(
      log_tev = log(total_enterprise_value_million)
    )
}

inspect_extremes(df, debt_to_equity_ratio)
inspect_extremes(df, quick_ratio)
inspect_extremes(df, return_on_equity_percent)
inspect_extremes(df, roace_percent)
inspect_extremes(df, tev_to_ebitda)
inspect_extremes(df, price_eps)


# missingness patterns

df %>%
  mutate(missing_dte = is.na(debt_to_equity_ratio)) %>%
  group_by(sector) %>%
  summarise(
    prop_missing = mean(missing_dte, na.rm = TRUE),
    n = n()
  ) %>%
  arrange(desc(prop_missing))


df %>%
  mutate(
    missing_credit = is.na(credit_rating),
    log_tev = log(total_enterprise_value_million)
  ) %>%
  group_by(missing_credit) %>%
  summarise(
    mean_size = mean(log_tev, na.rm = TRUE),
    n = n()
  )


# missing values per variable - for table 4

var_missing <- df %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_missing") %>%
  mutate(pct_missing = n_missing / nrow(df) * 100)

print(var_missing, n = 24)


# missingness per firm

analysis_vars <- c(
  "ebit_margin_percent",
  "total_revenue_growth_percent",
  "gross_margin_percent",
  "return_on_equity_percent",
  "debt_to_equity_ratio",
  "debt_to_ebitda",
  "quick_ratio",
  "tev_to_ebitda",
  "beta_one_year",
  "sector"
)

df %>%
  mutate(
    missing_percent = rowMeans(is.na(select(., all_of(analysis_vars)))) * 100
  ) %>%
  summarise(
    n_zero_missing = sum(missing_percent == 0),
    n_0_10_missing = sum(missing_percent > 0 & missing_percent <= 10),
    n_10_20_missing = sum(missing_percent > 10 & missing_percent <= 20),
    n_over_20_missing = sum(missing_percent > 20)
  )


# dendrogram of missingness

miss_vars <- df %>%
  dplyr::select(
    leverage_ratio_percent, ebitda_interest_expense, credit_rating,
    debt_to_equity_ratio, debt_to_ebitda, quick_ratio, tev_to_ebitda, 
    ebit_margin_percent, total_revenue_growth_percent, gross_margin_percent,
    roace_percent, price_eps, interest_expense_thousand, beta_one_year,
    beta_three_year, business_involvement_revenue_percent, sector, 
    total_enterprise_value
  )


# convert to 1 = missing, 0 = observed
miss_mat <- miss_vars %>% 
  mutate_all(~ifelse(is.na(.), 1, 0))

dist_mat <- dist(t(miss_mat), method = "euclidean")

hc <- hclust(dist_mat, method = "average")  

plot(hc, main = "Dendrogram of Missingness Patterns",
     xlab = "", sub = "", cex = 0.8)

var_labels <- paste0(colnames(miss_mat), " (", round(colMeans(miss_mat)*100,1), "%)")
plot(hc, labels = var_labels, main = "Missingness Dendrogram", xlab = "", sub = "", cex = 0.8)


# remove variables with too much missingness

df_reduced <- df %>%
  dplyr::select(-c(leverage_ratio_percent, credit_rating, ebitda_interest_expense, 
                   entity_name, total_enterprise_value_million))


# proportion of missing per firm
df_reduced <- df_reduced %>%
  mutate(prop_missing = rowSums(is.na(.)) / ncol(.))

# only keep firms with less than 20% missing
df_filtered <- df_reduced %>%
  filter(prop_missing <= 0.2)

df_filtered <- df_filtered %>%
  dplyr::select(-prop_missing)

nrow(df_filtered)




# MICE for multiple imputation

library(mice)

# remove the observation with beta one year misisng and drop ID column
df_mice <- df_filtered %>%
  filter(entity_id != 4167648) %>%
  dplyr::select(-entity_id)


# some vars need cart method instead of pmm because they are too skewed
cart_vars <- c(
  "debt_to_equity_ratio",
  "price_eps",
  "debt_to_ebitda",
  "return_on_equity_percent",
  "total_revenue_growth_percent"
)


# get default methods and predictor matrix
ini <- mice(df_mice, maxit = 0)

meth <- ini$method
pred <- ini$predictorMatrix


# change selected vars to cart
meth[cart_vars] <- "cart"


# stop imputation of outcome variable but still use it as predictor
meth["ebit_margin_percent"] <- ""
pred["ebit_margin_percent", ] <- 0
pred[, "ebit_margin_percent"] <- 1
pred["ebit_margin_percent", "ebit_margin_percent"] <- 0

set.seed(123)

imp <- mice(
  df_mice,
  m = 10,
  maxit = 10,
  method = meth,
  predictorMatrix = pred
)


# check convergence
plot(imp)

# density plots to check imputations
vars <- c("debt_to_equity_ratio","total_revenue_growth_percent", "price_eps", 
          "interest_expense_thousand","business_involvement_revenue_percent", 
          "tev_to_ebitda", "gross_margin_percent", "debt_to_ebitda", 
          "roace_percent","beta_three_year")

for (v in vars) {
  print(densityplot(imp, as.formula(paste("~", v))))
}




# linear regression with 2 way interactions

linearmodel <- with(imp,
                    lm(ebit_margin_percent ~
                         (total_revenue_growth_percent + return_on_equity_percent + 
                            sector + geography + beta_three_year + 
                            business_involvement_revenue_percent + total_enterprise_value + 
                            debt_to_ebitda + interest_expense_thousand + tev_to_ebitda + 
                            quick_ratio + debt_to_equity_ratio + gross_margin_percent + 
                            roace_percent + price_eps + beta_one_year)^2))

pooled <- pool(linearmodel)
summary(pooled)


# diagnostics - use first fitted model 
fit1 <- linearmodel$analyses[[1]]

par(mfrow = c(2, 2))

# residuals vs fitted
plot(fitted(fit1), resid(fit1),
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red", lty = 2)

# qq plot
qqnorm(resid(fit1),
       main = "Normal Q-Q")
qqline(resid(fit1), col = "red")

# scale location
plot(fitted(fit1), sqrt(abs(resid(fit1))),
     xlab = "Fitted values",
     ylab = "Sqrt(|Residuals|)",
     main = "Scale-Location")
abline(h = 0, col = "red", lty = 2)

# residuals vs leverage
plot(hatvalues(fit1), resid(fit1),
     xlab = "Leverage",
     ylab = "Residuals",
     main = "Residuals vs Leverage")
abline(h = 0, col = "red", lty = 2)




# gamma GLM
# need a shift so all values are positive
c <- abs(min(df_mice$ebit_margin_percent, na.rm = TRUE)) + 0.01

gammamodel <- with(
  imp,
  glm(
    I(ebit_margin_percent + c) ~
      sector + total_revenue_growth_percent + return_on_equity_percent +
      beta_three_year + business_involvement_revenue_percent +
      total_enterprise_value + debt_to_ebitda + interest_expense_thousand +
      tev_to_ebitda + quick_ratio + debt_to_equity_ratio +
      gross_margin_percent + roace_percent + price_eps + beta_one_year,
    family = Gamma(link = "log")
  )
)

pool(gammamodel)


# gamma diagnostics
fit1 <- gammamodel$analyses[[1]]

par(mfrow = c(2, 2))

plot(fitted(fit1),
     residuals(fit1, type = "deviance"),
     xlab = "Fitted values",
     ylab = "Deviance residuals",
     main = "Deviance Residuals vs Fitted")
abline(h = 0, col = "red", lty = 2)

qqnorm(residuals(fit1, type = "deviance"),
       main = "Q-Q Plot (Deviance Residuals)")
qqline(residuals(fit1, type = "deviance"), col = "red")

plot(fitted(fit1),
     residuals(fit1, type = "pearson"),
     xlab = "Fitted values",
     ylab = "Pearson residuals",
     main = "Pearson Residuals vs Fitted")
abline(h = 0, col = "red", lty = 2)

plot(hatvalues(fit1),
     residuals(fit1, type = "deviance"),
     xlab = "Leverage",
     ylab = "Deviance residuals",
     main = "Residuals vs Leverage")
abline(h = 0, col = "red", lty = 2)




# MSE for linear and gamma to be able to compare with other methods
# averaged across 10 imputations

set.seed(123)
n_full    <- nrow(complete(imp, 1))
train_lm  <- sample(1:n_full, size = 0.7 * n_full)
test_lm   <- setdiff(1:n_full, train_lm)

mse_linear_int <- numeric(imp$m)
mse_gamma_int  <- numeric(imp$m)

shift_const <- abs(min(df_mice$ebit_margin_percent, na.rm = TRUE)) + 0.01

predictors_int <- c("total_revenue_growth_percent", "return_on_equity_percent",
                    "sector", "geography", "beta_three_year",
                    "business_involvement_revenue_percent", "total_enterprise_value",
                    "debt_to_ebitda", "interest_expense_thousand", "tev_to_ebitda", 
                    "quick_ratio", "debt_to_equity_ratio", "gross_margin_percent",
                    "roace_percent", "price_eps", "beta_one_year")

form_lm_int <- as.formula(
  paste("ebit_margin_percent ~ (",
        paste(predictors_int, collapse = " + "), ")^2")
)

for (m in 1:imp$m) {
  
  cat("imputation", m, "\n")
  
  dat   <- complete(imp, m)
  train <- dat[train_lm, ]
  test  <- dat[test_lm, ]
  
  # remove test rows with factor levels not in training
  for (v in c("sector", "geography")) {
    train_lvls <- unique(train[[v]])
    test       <- test[test[[v]] %in% train_lvls, ]
  }
  
  # linear
  fit_lm <- lm(form_lm_int, data = train)
  pred_lm <- predict(fit_lm, newdata = test)
  valid <- !is.na(pred_lm)
  mse_linear_int[m] <- mean(
    (test$ebit_margin_percent[valid] - pred_lm[valid])^2
  )
  
  # gamma
  train$y_shift   <- train$ebit_margin_percent + shift_const
  form_gamma_int  <- update(form_lm_int, y_shift ~ .)
  
  fit_gamma <- tryCatch(
    glm(form_gamma_int, data = train, family = Gamma(link = "log")),
    error = function(e) NULL,
    warning = function(w) {
      suppressWarnings(glm(form_gamma_int, data = train,
                           family = Gamma(link = "log")))
    }
  )
  
  if (!is.null(fit_gamma)) {
    pred_gamma_shift <- predict(fit_gamma, newdata = test, type = "response")
    pred_gamma <- pred_gamma_shift - shift_const
    
    valid <- is.finite(pred_gamma)
    mse_gamma_int[m] <- mean(
      (test$ebit_margin_percent[valid] - pred_gamma[valid])^2
    )
  } else {
    mse_gamma_int[m] <- NA
  }
  
  cat("  linear MSE:", round(mse_linear_int[m], 2),
      "| gamma MSE:", round(mse_gamma_int[m], 2), "\n")
}

# summary
cat("linear mean MSE:", round(mean(mse_linear_int, na.rm = TRUE), 3), "\n")
cat("linear median MSE:", round(median(mse_linear_int, na.rm = TRUE), 3), "\n")
cat("linear range:", round(min(mse_linear_int, na.rm = TRUE), 3), "to",
    round(max(mse_linear_int, na.rm = TRUE), 3), "\n")

cat("gamma mean MSE:", round(mean(mse_gamma_int, na.rm = TRUE), 3), "\n")
cat("gamma median MSE:", round(median(mse_gamma_int, na.rm = TRUE), 3), "\n")
cat("gamma range:", round(min(mse_gamma_int, na.rm = TRUE), 3), "to",
    round(max(mse_gamma_int, na.rm = TRUE), 3), "\n")


# check error distribution for linear model
all_errors <- list()

for (m in 1:imp$m) {
  dat   <- complete(imp, m)
  train <- dat[train_lm, ]
  test  <- dat[test_lm, ]
  
  for (v in c("sector", "geography")) {
    train_lvls <- unique(train[[v]])
    test       <- test[test[[v]] %in% train_lvls, ]
  }
  
  fit_lm <- lm(form_lm_int, data = train)
  pred_lm <- predict(fit_lm, newdata = test)
  valid <- !is.na(pred_lm)
  
  errors <- test$ebit_margin_percent[valid] - pred_lm[valid]
  all_errors[[m]] <- errors
}

all_errors_flat <- unlist(all_errors)
print(summary(all_errors_flat^2))

cat("% squared errors > 10000:",
    round(mean(all_errors_flat^2 > 10000) * 100, 2), "%\n")
cat("median squared error:",
    round(median(all_errors_flat^2), 2), "\n")




# penalised regression

library(glmnet)

completed_data <- complete(imp, 1)  

X <- model.matrix(ebit_margin_percent ~ 
                    total_revenue_growth_percent + return_on_equity_percent +
                    sector + beta_three_year + business_involvement_revenue_percent +
                    total_enterprise_value + debt_to_ebitda + interest_expense_thousand +
                    tev_to_ebitda + quick_ratio + debt_to_equity_ratio +
                    gross_margin_percent + roace_percent + price_eps + beta_one_year,
                  data = completed_data)[,-1]

y <- completed_data$ebit_margin_percent


# train/test split
set.seed(123)

n <- nrow(X)
train <- sample(1:n, size = 0.7 * n)
test  <- setdiff(1:n, train)

X.train <- X[train, ]
X.test  <- X[test, ]
y.train <- y[train]
y.test  <- y[test]

# ridge
set.seed(123)
cv_ridge <- cv.glmnet(X.train, y.train, alpha = 0)
lambda_ridge <- cv_ridge$lambda.min

# lasso
set.seed(123)
cv_lasso <- cv.glmnet(X.train, y.train, alpha = 1)
lambda_lasso <- cv_lasso$lambda.min


ridge.mod <- glmnet(X.train, y.train, alpha = 0, lambda = lambda_ridge)
lasso.mod <- glmnet(X.train, y.train, alpha = 1, lambda = lambda_lasso)


# elastic net - find best alpha
alphas <- seq(0.1, 0.9, 0.01)
cv_errors <- numeric(length(alphas))

set.seed(123)
for (i in seq_along(alphas)) {
  cv <- cv.glmnet(X.train, y.train, alpha = alphas[i])
  cv_errors[i] <- min(cv$cvm)
}

best_alpha <- alphas[which.min(cv_errors)]

elnet_cv <- cv.glmnet(X.train, y.train, alpha = best_alpha)
best_lambda_elnet <- elnet_cv$lambda.min
elnet.mod <- glmnet(X.train, y.train, alpha = best_alpha, lambda = best_lambda_elnet)


# test predictions
elnet.pred <- predict(elnet.mod, s = best_lambda_elnet, newx = X.test)
ridge.pred <- predict(ridge.mod, s = lambda_ridge, newx = X.test)
lasso.pred <- predict(lasso.mod, s = lambda_lasso, newx = X.test)

# test MSEs
elnet_mse <- mean((elnet.pred - y.test)^2)
ridge_mse <- mean((ridge.pred - y.test)^2)
lasso_mse <- mean((lasso.pred - y.test)^2)

ridge_mse
lasso_mse
elnet_mse
best_alpha

# lasso coefficients to see which are dropped
lasso_coefs <- coef(lasso.mod, s = lambda_lasso)
lasso_coefs


# MSE vs lambda plots
par(mfrow = c(1, 3))
plot(cv_ridge)
title(main = "Ridge MSE vs Lambda", line = 2.5)
plot(cv_lasso)
title(main = "Lasso MSE vs Lambda", line = 2.5)
plot(elnet_cv)
title(main = "Elastic Net MSE vs Lambda", line = 2.5)


# coefficient paths
ridge.path <- glmnet(X.train, y.train, alpha = 0)
plot(ridge.path, xvar = "lambda")
title(main = "Ridge Coefficient Paths", line= 2.5)

lasso.path <- glmnet(X.train, y.train, alpha = 1)
plot(lasso.path, xvar = "lambda")
title(main = "Lasso Coefficient Paths", line= 2.5)

elnet.path <- glmnet(X.train, y.train, alpha = best_alpha)
plot(elnet.path, xvar = "lambda")
title(main = "Elastic Net Coefficient Paths", line = 2.5)




# elastic net with 2 way interactions

completed_data <- complete(imp, 1)

completed_data <- completed_data %>%
  mutate(
    sector = as.factor(sector),
    geography = as.factor(geography)
  )

y <- completed_data$ebit_margin_percent

set.seed(1)
n <- nrow(completed_data)
train <- sample(1:n, size = 0.7 * n)
test  <- setdiff(1:n, train)

y.train <- y[train]
y.test  <- y[test]

# build on full data first so train and test have same columns
interaction_formula <- ebit_margin_percent ~ (
  total_revenue_growth_percent + return_on_equity_percent + sector + geography +
    beta_three_year + business_involvement_revenue_percent + total_enterprise_value +
    debt_to_ebitda + interest_expense_thousand + tev_to_ebitda + quick_ratio +
    debt_to_equity_ratio + gross_margin_percent + roace_percent + price_eps +
    beta_one_year
)^2

X_full <- model.matrix(interaction_formula, data = completed_data)[, -1]

X_train <- X_full[train, ]
X_test  <- X_full[test, ]


set.seed(1)
cv_elnet_int <- cv.glmnet(
  X_train, y.train,
  alpha = best_alpha,
  nfolds = 10
)

best_lambda_int <- cv_elnet_int$lambda.min

pred_int <- predict(cv_elnet_int, s = best_lambda_int, newx = X_test)
mse_int <- mean((pred_int - y.test)^2)
mse_int


# extract coefficients
coef_int <- coef(cv_elnet_int, s = best_lambda_int)
coef_mat <- as.matrix(coef_int)

nz <- coef_mat[coef_mat[, 1] != 0, , drop = FALSE]

coef_df_interactions <- data.frame(
  Predictor = rownames(nz),
  Coefficient = round(nz[, 1], 4)
) %>%
  filter(Predictor != "(Intercept)") %>%
  arrange(desc(abs(Coefficient)))

head(coef_df_interactions, 20)
coef_df_interactions




# penalised regression with categorisation

completed_data <- completed_data %>%
  mutate(
    # debt/ebitda
    debt_ebitda_cat = case_when(
      debt_to_ebitda < 2 ~ "Low",
      debt_to_ebitda >= 2 & debt_to_ebitda <= 4 ~ "Moderate",
      debt_to_ebitda > 4 ~ "High",
      TRUE ~ NA_character_
    ),
    
    # quick ratio
    quick_ratio_cat = case_when(
      quick_ratio < 1 ~ "Constrained",
      quick_ratio >= 1 & quick_ratio <= 2 ~ "Adequate",
      quick_ratio > 2 ~ "Excess",
      TRUE ~ NA_character_
    ),
    
    # ROE
    roe_cat = case_when(
      return_on_equity_percent < 0 ~ "Negative",
      return_on_equity_percent < 10 ~ "Low",
      return_on_equity_percent < 20 ~ "Moderate",
      return_on_equity_percent >= 20 ~ "High",
      TRUE ~ NA_character_
    ),
    
    # debt to equity 
    debt_equity_cat = case_when(
      debt_to_equity_ratio < 0.5 ~ "Low",
      debt_to_equity_ratio < 1.5 ~ "Moderate",
      debt_to_equity_ratio < 3 ~ "High",
      debt_to_equity_ratio >= 3 ~ "Very_High",
      TRUE ~ NA_character_
    ),
    
    # ROACE
    roace_cat = case_when(
      roace_percent < 0 ~ "Negative",
      roace_percent < 10 ~ "Low",
      roace_percent < 20 ~ "Moderate",
      roace_percent >= 20 ~ "High",
      TRUE ~ NA_character_
    ),
    
    # sector super-sector grouping
    sector_group = case_when(
      sector %in% c("Consumer Staples", "Utilities", "Health Care") ~ "Defensive",
      sector %in% c("Information Technology", "Communication Services",
                    "Energy", "Materials", "Industrials") ~ "Sensitive",
      sector %in% c("Financials", "Real Estate") ~ "Cyclical",
      sector %in% c("Consumer Discretionary") ~ "Consumer Cyclical",
      TRUE ~ "Other"
    )
  ) %>%
  mutate(
    across(
      c(debt_ebitda_cat, quick_ratio_cat, roe_cat, roace_cat,
        debt_equity_cat, sector_group),
      factor
    )
  )

completed_data$log_tev <- log1p(completed_data$total_enterprise_value)
completed_data$log_tev_to_ebitda <- log1p(completed_data$tev_to_ebitda)


full_formula <- ebit_margin_percent ~ (
  gross_margin_percent + beta_one_year + beta_three_year + price_eps +
    tev_to_ebitda + log_tev + debt_ebitda_cat + quick_ratio_cat + roe_cat +
    log_tev_to_ebitda + debt_equity_cat + roace_cat + sector_group
)^2   


set.seed(123)
n <- nrow(completed_data)
train <- sample(1:n, size = 0.7 * n)
test  <- setdiff(1:n, train)

y.train <- completed_data$ebit_margin_percent[train]
y.test  <- completed_data$ebit_margin_percent[test]


X_train <- model.matrix(full_formula, data = completed_data[train, ])
X_train <- X_train[, -1]   

X_test <- model.matrix(full_formula, data = completed_data[test, ])
X_test <- X_test[, -1]


# align columns between train and test
missing_cols <- setdiff(colnames(X_train), colnames(X_test))
for (col in missing_cols) {
  X_test <- cbind(X_test, 0)
  colnames(X_test)[ncol(X_test)] <- col
}

extra_cols <- setdiff(colnames(X_test), colnames(X_train))
X_test <- X_test[, !(colnames(X_test) %in% extra_cols)]

X_test <- X_test[, colnames(X_train)]


set.seed(123)
cv_elnet <- cv.glmnet(X_train, y.train, alpha = 0.5,
                      standardize = TRUE, nfolds = 10)

best_lambda <- cv_elnet$lambda.min

# non zero coefficients
coef_mat <- as.matrix(coef(cv_elnet, s = best_lambda))
nz <- coef_mat[coef_mat[,1] != 0, , drop = FALSE]
print(nz)

pred <- predict(cv_elnet, s = best_lambda, newx = X_test)
mse <- mean((y.test - pred)^2)
print(mse)




# elastic net with log transformations

completed_data <- completed_data %>%
  mutate(
    sector = factor(sector),
    geography = factor(geography)
  )


# log transform skewed vars, using pmax to handle negatives
completed_data <- completed_data %>%
  mutate(
    log_debt_to_ebitda = log1p(pmax(debt_to_ebitda, 0)),
    log_debt_to_equity = log1p(pmax(debt_to_equity_ratio, 0)),
    log_tev = log1p(pmax(total_enterprise_value, 0)),
    log_tev_to_ebitda = log1p(pmax(tev_to_ebitda, 0)),
    log_interest_expense = log1p(pmax(interest_expense_thousand, 0)),
    log_price_eps = log1p(pmax(price_eps, 0))
  )


model_vars <- c(
  "ebit_margin_percent",
  "gross_margin_percent", "beta_one_year", "beta_three_year",
  "log_price_eps", "log_interest_expense", 
  "log_debt_to_ebitda", "log_debt_to_equity",
  "log_tev", "log_tev_to_ebitda",
  "quick_ratio", "return_on_equity_percent",
  "roace_percent", "total_revenue_growth_percent",
  "sector", "geography"
)

completed_data_clean <- completed_data %>%
  dplyr::select(all_of(model_vars))


full_formula <- ebit_margin_percent ~ (
  gross_margin_percent + beta_one_year + beta_three_year + log_price_eps +
    log_interest_expense + log_debt_to_ebitda + log_debt_to_equity + log_tev +
    log_tev_to_ebitda + quick_ratio + return_on_equity_percent + roace_percent +
    total_revenue_growth_percent + sector + geography
)^2


set.seed(123)
n <- nrow(completed_data_clean)
train <- sample(1:n, size = 0.7 * n)
test  <- setdiff(1:n, train)

y.train <- completed_data_clean$ebit_margin_percent[train]
y.test  <- completed_data_clean$ebit_margin_percent[test]


X_train <- model.matrix(full_formula, data = completed_data_clean[train, ])[, -1]
X_test  <- model.matrix(full_formula, data = completed_data_clean[test, ])[, -1]


# align train and test columns
missing_cols <- setdiff(colnames(X_train), colnames(X_test))
for (col in missing_cols) {
  X_test <- cbind(X_test, 0)
  colnames(X_test)[ncol(X_test)] <- col
}

extra_cols <- setdiff(colnames(X_test), colnames(X_train))
X_test <- X_test[, !(colnames(X_test) %in% extra_cols)]
X_test <- X_test[, colnames(X_train)]


set.seed(123)
cv_elnet <- cv.glmnet(X_train, y.train, alpha = 0.5,
                      standardize = TRUE, nfolds = 10)

best_lambda <- cv_elnet$lambda.min

pred <- predict(cv_elnet, s = best_lambda, newx = X_test)
mse <- mean((y.test - pred)^2)
print(mse)




# stability selection - uses all 10 imputations

library(stabs)

# resolve function conflicts
complete  <- mice::complete
select    <- dplyr::select

stability_probs_lasso <- list()
stability_probs_enet  <- list()

# make sure train_id exists
if (!exists("train_id")) {
  set.seed(123)
  temp_data <- mice::complete(imp, 1)
  n_temp    <- nrow(temp_data)
  train_id  <- sample(1:n_temp, size = 0.7 * n_temp)
}

for (i in 1:10) {
  
  cat("stability selection imputation", i, "\n")
  
  completed_data_i <- mice::complete(imp, i)
  
  completed_data_i$sector    <- as.factor(completed_data_i$sector)
  completed_data_i$geography <- as.factor(completed_data_i$geography)
  
  X_stab <- tryCatch({
    model.matrix(ebit_margin_percent ~ 
                   total_revenue_growth_percent + return_on_equity_percent +
                   sector + beta_three_year + business_involvement_revenue_percent +
                   total_enterprise_value + debt_to_ebitda + interest_expense_thousand +
                   tev_to_ebitda + quick_ratio + debt_to_equity_ratio +
                   gross_margin_percent + roace_percent + price_eps + beta_one_year,
                 data = completed_data_i)[, -1]
  }, error = function(e) {
    cat("  model.matrix error:", conditionMessage(e), "\n")
    NULL
  })
  
  if (is.null(X_stab)) next
  
  y_stab <- completed_data_i$ebit_margin_percent
  
  train_idx_stab <- intersect(train_id, 1:nrow(X_stab))
  
  X_stab_train <- X_stab[train_idx_stab, ]
  y_stab_train <- y_stab[train_idx_stab]
  
  # remove rows with NA in y
  complete_rows <- !is.na(y_stab_train)
  X_stab_train  <- X_stab_train[complete_rows, ]
  y_stab_train  <- y_stab_train[complete_rows]
  
  # lasso stability selection
  set.seed(123)
  stab_lasso <- tryCatch({
    stabsel(
      x      = X_stab_train,
      y      = y_stab_train,
      fitfun = glmnet.lasso,
      cutoff = 0.9,
      PFER   = 1
    )
  }, error = function(e) {
    cat("  lasso error:", conditionMessage(e), "\n")
    NULL
  })
  
  if (!is.null(stab_lasso)) {
    stability_probs_lasso[[i]] <- stab_lasso$max
  }
  
  # elastic net with best_alpha
  glmnet_enet_custom <- function(x, y, q, ...) {
    glmnet.lasso(x, y, q, alpha = best_alpha, ...)
  }
  
  set.seed(123)
  stab_enet <- tryCatch({
    stabsel(
      x      = X_stab_train,
      y      = y_stab_train,
      fitfun = glmnet_enet_custom,
      cutoff = 0.9,
      PFER   = 1
    )
  }, error = function(e) {
    cat("  enet error:", conditionMessage(e), "\n")
    NULL
  })
  
  if (!is.null(stab_enet)) {
    stability_probs_enet[[i]] <- stab_enet$max
  }
}


# average selection probabilities
average_stab_probs <- function(prob_list) {
  
  prob_list <- prob_list[!sapply(prob_list, is.null)]
  
  if (length(prob_list) == 0) return(NULL)
  
  all_vars <- unique(unlist(lapply(prob_list, names)))
  
  avg <- sapply(all_vars, function(var) {
    probs <- sapply(prob_list, function(p) {
      if (var %in% names(p)) p[var] else 0
    })
    mean(probs)
  })
  
  sort(avg, decreasing = TRUE)
}

avg_lasso_probs <- average_stab_probs(stability_probs_lasso)
avg_enet_probs  <- average_stab_probs(stability_probs_enet)

# print results
if (!is.null(avg_lasso_probs)) {
  cat("lasso selection probabilities:\n")
  print(round(avg_lasso_probs, 3))
  
  stab_lasso_selected <- avg_lasso_probs[avg_lasso_probs >= 0.9]
  cat("stably selected (>= 0.9):\n")
  print(round(stab_lasso_selected, 3))
}

if (!is.null(avg_enet_probs)) {
  cat("elastic net selection probabilities:\n")
  print(round(avg_enet_probs, 3))
  
  stab_enet_selected <- avg_enet_probs[avg_enet_probs >= 0.9]
  cat("stably selected (>= 0.9):\n")
  print(round(stab_enet_selected, 3))
}




# random forest

rf_data <- complete(imp, 1)

rf_data <- rf_data %>%
  mutate(
    sector = as.factor(sector),
    geography = as.factor(geography)
  )

set.seed(123)
n <- nrow(rf_data)
train_id <- sample(1:n, size = 0.7 * n)
test_id  <- setdiff(1:n, train_id)

rf_train <- rf_data[train_id, ]
rf_test  <- rf_data[test_id, ]


library(randomForest)

set.seed(123)

p <- ncol(rf_train[, setdiff(names(rf_train), "ebit_margin_percent")])

rf_base <- randomForest(
  ebit_margin_percent ~ .,
  data = rf_train,
  ntree = 1000,
  mtry = floor(sqrt(p)),
  importance = TRUE
)

print(rf_base)


# tune mtry
set.seed(123)
tune_grid <- tuneRF(
  x = rf_train[, setdiff(names(rf_train), "ebit_margin_percent")],
  y = rf_train$ebit_margin_percent,
  ntreeTry = 500,
  stepFactor = 1.5,
  improve = 0.01,
  trace = TRUE,
  plot = TRUE
)

best_mtry <- tune_grid[which.min(tune_grid[,2]), 1]
best_mtry


# final model 
set.seed(123)
rf_final <- randomForest(
  ebit_margin_percent ~ .,
  data = rf_train,
  ntree = 1500,
  mtry = best_mtry,
  importance = TRUE
)

print(rf_final)
randomForest::importance(rf_final)

rf_pred <- predict(rf_final, newdata = rf_test)
rf_mse <- mean((rf_pred - rf_test$ebit_margin_percent)^2)
rf_r2  <- cor(rf_pred, rf_test$ebit_margin_percent)^2

rf_mse
rf_r2


# variable importance plot
par(mfrow = c(1, 1))
varImpPlot(rf_final,
           type = 1,
           main = "Random Forest Variable Importance (% Increase in MSE)")


# partial dependence plots

partialPlot(
  x = rf_final,
  pred.data = rf_train,
  x.var = "gross_margin_percent",
  main = "Partial Dependence: Gross Margin (%)",
  ylab = "Predicted EBIT Margin (%)"
)

partialPlot(
  x = rf_final,
  pred.data = rf_train,
  x.var = "roace_percent",
  main = "Partial Dependence: ROACE (%)",
  ylab = "Predicted EBIT Margin (%)"
)

partialPlot(
  x = rf_final,
  pred.data = rf_train,
  x.var = "return_on_equity_percent",
  main = "Partial Dependence: ROE (%)",
  ylab = "Predicted EBIT Margin (%)"
)




# hybrid model - RF + ridge across all 10 imputations

all_mse <- numeric(10)
all_preds <- matrix(NA, nrow = length(test_id), ncol = 10)
all_importance <- list()
all_n_selected <- numeric(10)
all_n_interactions <- numeric(10)

for (i in 1:10) {
  
  cat("imputation", i, "\n")
  
  rf_data <- complete(imp, i) %>%
    mutate(
      sector = as.factor(sector),
      geography = as.factor(geography)
    )
  
  train_df <- rf_data[train_id, ]
  test_df  <- rf_data[test_id, ]
  
  main_vars <- c("total_revenue_growth_percent", "return_on_equity_percent",
                 "beta_three_year", "business_involvement_revenue_percent",
                 "total_enterprise_value", "debt_to_ebitda",
                 "interest_expense_thousand", "tev_to_ebitda", "quick_ratio",
                 "debt_to_equity_ratio", "gross_margin_percent", "roace_percent",
                 "price_eps", "beta_one_year", "sector")
  
  formula_int <- as.formula(paste("ebit_margin_percent ~(", 
                                  paste(main_vars, collapse = " + "), ")^2"))
  
  X_train_full <- model.matrix(formula_int, data = train_df)[, -1]
  X_test_full  <- model.matrix(formula_int, data = test_df)[, -1]
  
  y_train <- train_df$ebit_margin_percent
  y_test  <- test_df$ebit_margin_percent
  
  # random forest on expanded set
  set.seed(123)
  rf_reg <- randomForest(
    x = X_train_full,
    y = y_train,
    ntree = 1000,
    mtry = 4,
    importance = TRUE
  )
  
  imp_scores <- randomForest::importance(rf_reg, type = 1)
  imp_df_i <- data.frame(
    variable = rownames(imp_scores),
    inc_mse  = imp_scores[, 1]
  ) %>% arrange(desc(inc_mse))
  
  all_importance[[i]] <- imp_df_i
  
  # select predictors above 1sd from mean importance
  threshold <- mean(imp_df_i$inc_mse) + sd(imp_df_i$inc_mse)
  
  top_vars_i <- imp_df_i %>%
    filter(inc_mse > threshold) %>%
    pull(variable)
  
  all_n_selected[i] <- length(top_vars_i)
  all_n_interactions[i] <- sum(grepl(":", top_vars_i))
  
  cat("  threshold:", round(threshold, 3), 
      "| selected:", length(top_vars_i),
      "| interactions:", sum(grepl(":", top_vars_i)), "\n")
  
  top_vars_i <- top_vars_i[top_vars_i %in% colnames(X_train_full)]
  
  X_train_sel <- X_train_full[, top_vars_i, drop = FALSE]
  X_test_sel  <- X_test_full[, top_vars_i, drop = FALSE]
  
  # deal with any train/test column mismatches
  common_cols <- intersect(colnames(X_train_sel), colnames(X_test_sel))
  X_train_sel <- X_train_sel[, common_cols, drop = FALSE]
  X_test_sel  <- X_test_sel[, common_cols, drop = FALSE]
  
  # ridge
  set.seed(123)
  cv_ridge <- cv.glmnet(X_train_sel, y_train, alpha = 0)
  best_lambda <- cv_ridge$lambda.min
  
  ridge_model <- glmnet(X_train_sel, y_train,
                        alpha = 0, lambda = best_lambda)
  
  preds <- predict(ridge_model, newx = X_test_sel)[, 1]
  all_preds[, i] <- preds
  all_mse[i] <- mean((preds - y_test)^2)
  
  cat("  MSE:", round(all_mse[i], 3), "\n")
}


# average across imputations
avg_preds <- rowMeans(all_preds)

final_mse <- mean((avg_preds - y_test)^2)
cat("final averaged MSE:", round(final_mse, 3), "\n")

# r squared
ss_res <- sum((y_test - avg_preds)^2)
ss_tot <- sum((y_test - mean(y_test))^2)
final_r2 <- 1 - ss_res/ss_tot
cat("R squared:", round(final_r2, 3), "\n")

# selection summary
cat("mean predictors selected:", round(mean(all_n_selected), 1), "\n")
cat("mean interactions selected:", round(mean(all_n_interactions), 1), "\n")
cat("MSE range:", round(min(all_mse), 3), "to", round(max(all_mse), 3), "\n")


# average importance

avg_importance <- all_importance[[1]] %>%
  rename(inc_mse_1 = inc_mse)

for (i in 2:10) {
  avg_importance <- avg_importance %>%
    left_join(all_importance[[i]] %>%
                rename(!!paste0("inc_mse_", i) := inc_mse),
              by = "variable")
}

avg_importance$mean_inc_mse <- rowMeans(
  avg_importance[, paste0("inc_mse_", 1:10)],
  na.rm = TRUE
)

avg_importance <- avg_importance %>%
  dplyr::select(variable, mean_inc_mse) %>%
  arrange(desc(mean_inc_mse))

head(avg_importance, 20)


# extract and average ridge coefficients across imputations

all_coefs <- list()

for (i in 1:10) {
  
  cat("coefficients imputation", i, "\n")
  
  rf_data <- complete(imp, i) %>%
    mutate(
      sector = as.factor(sector),
      geography = as.factor(geography)
    )
  
  main_vars <- c("total_revenue_growth_percent", "return_on_equity_percent",
                 "beta_three_year", "business_involvement_revenue_percent",
                 "total_enterprise_value", "debt_to_ebitda",
                 "interest_expense_thousand", "tev_to_ebitda",
                 "quick_ratio", "debt_to_equity_ratio",
                 "gross_margin_percent", "roace_percent",
                 "price_eps", "beta_one_year", "sector")
  
  formula_int <- as.formula(paste("ebit_margin_percent ~(",
                                  paste(main_vars, collapse = " + "), ")^2"))
  
  X_full <- model.matrix(formula_int, data = rf_data)[, -1]
  X_train_full <- X_full[train_id, ]
  
  y_train <- rf_data$ebit_margin_percent[train_id]
  
  set.seed(123)
  rf_reg <- randomForest(
    x = X_train_full,
    y = y_train,
    ntree = 1000,
    mtry = 4,
    importance = TRUE
  )
  
  imp_scores <- randomForest::importance(rf_reg, type = 1)
  imp_df_i <- data.frame(
    variable = rownames(imp_scores),
    inc_mse  = imp_scores[, 1]
  ) %>% arrange(desc(inc_mse))
  
  threshold <- mean(imp_df_i$inc_mse) + sd(imp_df_i$inc_mse)
  
  top_vars_i <- imp_df_i %>%
    filter(inc_mse > threshold) %>%
    pull(variable)
  
  top_vars_i <- top_vars_i[top_vars_i %in% colnames(X_train_full)]
  
  X_train_sel <- X_train_full[, top_vars_i, drop = FALSE]
  
  set.seed(123)
  cv_ridge <- cv.glmnet(X_train_sel, y_train, alpha = 0)
  best_lambda <- cv_ridge$lambda.min
  
  ridge_model <- glmnet(X_train_sel, y_train,
                        alpha = 0, lambda = best_lambda)
  
  coefs <- as.matrix(coef(ridge_model))
  coef_vec <- setNames(coefs[, 1], rownames(coefs))
  all_coefs[[i]] <- coef_vec
}


# average coefficients across imputations
all_coef_names <- unique(unlist(lapply(all_coefs, names)))

coef_matrix <- matrix(0,
                      nrow = 10,
                      ncol = length(all_coef_names),
                      dimnames = list(
                        paste0("imp_", 1:10),
                        all_coef_names
                      ))

for (i in 1:10) {
  coef_i <- all_coefs[[i]]
  coef_matrix[i, names(coef_i)] <- coef_i
}

avg_coefs       <- colMeans(coef_matrix)
sd_coefs        <- apply(coef_matrix, 2, sd)
selection_freq  <- colMeans(coef_matrix != 0)


hybrid_coef_table <- data.frame(
  Predictor = names(avg_coefs),
  Mean_Coefficient = round(avg_coefs, 4),
  SD = round(sd_coefs, 4),
  Selection_Freq = round(selection_freq, 2)
) %>%
  filter(Predictor != "(Intercept)") %>%
  arrange(desc(abs(Mean_Coefficient)))

# predictors selected in at least 50% of imputations
hybrid_robust <- hybrid_coef_table %>%
  filter(Selection_Freq >= 0.5) %>%
  arrange(desc(abs(Mean_Coefficient)))

print(hybrid_robust)


# diagnostic plots for hybrid
par(mfrow = c(1, 2))

plot(y_test, avg_preds,
     xlab = "Actual EBIT Margin",
     ylab = "Predicted EBIT Margin",
     main = "Hybrid Model: Predicted vs Actual")
abline(a = 0, b = 1, col = "red", lty = 2)

residuals_hybrid <- y_test - avg_preds
plot(avg_preds, residuals_hybrid,
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Hybrid Model: Residuals vs Fitted")
abline(h = 0, col = "red", lty = 2)




# simulation study

library(MASS)

simulate_data <- function(n, p, rho, beta_true, noise_sd = 2) {
  Sigma       <- matrix(rho, nrow = p, ncol = p)
  diag(Sigma) <- 1
  X           <- mvrnorm(n, mu = rep(0, p), Sigma = Sigma)
  colnames(X) <- paste0("x", 1:p)
  y           <- X %*% beta_true + rnorm(n, sd = noise_sd)
  list(X = X, y = as.numeric(y))
}


# fit all methods and return MSEs
fit_all_methods <- function(X_train, y_train, X_test,  y_test) {
  
  mse <- function(pred) mean((y_test - pred)^2)
  
  # OLS
  df_train  <- data.frame(y = y_train, X_train)
  df_test   <- data.frame(X_test)
  ols_fit   <- lm(y ~ ., data = df_train)
  mse_ols   <- mse(predict(ols_fit, newdata = df_test))
  
  # ridge
  cv_ridge  <- cv.glmnet(X_train, y_train, alpha = 0)
  mse_ridge <- mse(predict(cv_ridge, X_test, s = "lambda.min"))
  
  # lasso
  cv_lasso  <- cv.glmnet(X_train, y_train, alpha = 1)
  mse_lasso <- mse(predict(cv_lasso, X_test, s = "lambda.min"))
  
  # elastic net
  cv_en     <- cv.glmnet(X_train, y_train, alpha = 0.5)
  mse_en    <- mse(predict(cv_en, X_test, s = "lambda.min"))
  
  # random forest
  rf_fit    <- randomForest::randomForest(X_train, y_train, ntree = 500)
  mse_rf    <- mse(predict(rf_fit, X_test))
  
  c(OLS = mse_ols, Ridge = mse_ridge, Lasso = mse_lasso,
    ElasticNet = mse_en, RandomForest = mse_rf)
}


# parameters
n        <- 800
p        <- 15
n_sim    <- 100
rho_vals <- c(0.0, 0.3, 0.6, 0.9)

# dense true model - all betas equal
beta_dense <- rep(0.5, p)


# monte carlo MSE comparison
run_simulation <- function(beta_true, rho, n_sim, model_name) {
  
  results           <- matrix(NA, nrow = n_sim, ncol = 5)
  colnames(results) <- c("OLS", "Ridge", "Lasso", "ElasticNet", "RandomForest")
  
  for (i in 1:n_sim) {
    dat     <- simulate_data(n, p, rho, beta_true)
    X       <- dat$X
    y       <- dat$y
    n_train <- floor(0.7 * n)
    idx     <- sample(1:n, n_train)
    X_train <- X[idx, ];  y_train <- y[idx]
    X_test  <- X[-idx, ]; y_test  <- y[-idx]
    results[i, ] <- tryCatch(
      fit_all_methods(X_train, y_train, X_test, y_test),
      error = function(e) rep(NA, 5)
    )
  }
  
  data.frame(
    Rho      = rho,
    Method   = colnames(results),
    Mean_MSE = round(colMeans(results, na.rm = TRUE), 3),
    SD_MSE   = round(apply(results, 2, sd, na.rm = TRUE), 3)
  )
}


# run across all rho values
cat("running dense simulations...\n")
dense_results <- do.call(rbind, lapply(rho_vals, function(r) {
  cat("  rho =", r, "\n")
  run_simulation(beta_dense, r, n_sim, "Dense")
}))

dense_table <- dense_results %>%
  dplyr::select(Rho, Method, Mean_MSE, SD_MSE) %>%
  arrange(Rho, Mean_MSE)
print(dense_table)






# coefficient recovery at rho = 0.9

set.seed(123)

rho_worst  <- 0.9
n_iter_cr  <- 50
p          <- 15

# single non zero coefficient
beta_recovery        <- rep(0, p)
beta_recovery[1]     <- 3

ols_b1    <- numeric(n_iter_cr)
ridge_b1  <- numeric(n_iter_cr)
lasso_b1  <- numeric(n_iter_cr)

for (i in 1:n_iter_cr) {
  
  dat <- simulate_data(n, p, rho_worst, beta_recovery)
  X   <- dat$X
  y   <- dat$y
  colnames(X) <- paste0("x", 1:p)
  
  # OLS
  df_train  <- data.frame(y = y, X)
  ols_fit   <- lm(y ~ ., data = df_train)
  ols_b1[i] <- coef(ols_fit)["x1"]
  
  # ridge
  cv_r         <- cv.glmnet(X, y, alpha = 0)
  ridge_fit    <- glmnet(X, y, alpha = 0, lambda = cv_r$lambda.min)
  ridge_b1[i]  <- coef(ridge_fit)["x1", 1]
  
  # lasso
  cv_l         <- cv.glmnet(X, y, alpha = 1)
  lasso_fit    <- glmnet(X, y, alpha = 1, lambda = cv_l$lambda.min)
  lasso_b1[i]  <- coef(lasso_fit)["x1", 1]
  
  cat("iteration", i, "\n")
}


# summary
cat("true beta1 = 3, rho =", rho_worst, "\n")
cat(sprintf("OLS   - mean: %.3f, sd: %.3f, bias: %.3f\n",
            mean(ols_b1), sd(ols_b1), mean(ols_b1) - 3))
cat(sprintf("ridge - mean: %.3f, sd: %.3f, bias: %.3f\n",
            mean(ridge_b1), sd(ridge_b1), mean(ridge_b1) - 3))
cat(sprintf("lasso - mean: %.3f, sd: %.3f, bias: %.3f\n",
            mean(lasso_b1), sd(lasso_b1), mean(lasso_b1) - 3))


# hybrid coefficient recovery at rho = 0.9

hybrid_b1 <- numeric(n_iter_cr)

for (i in 1:n_iter_cr) {
  
  dat <- simulate_data(n, p, rho_worst, beta_recovery)
  X   <- dat$X
  y   <- dat$y
  colnames(X) <- paste0("x", 1:p)
  
  # random forest stage - select important predictors
  set.seed(123)
  rf_fit <- randomForest(X, y, ntree = 500, importance = TRUE)
  imp_scores <- randomForest::importance(rf_fit, type = 1)
  
  # threshold: 1 sd above mean importance
  threshold <- mean(imp_scores[,1]) + sd(imp_scores[,1])
  top_vars <- rownames(imp_scores)[imp_scores[,1] > threshold]
  
  # if x1 not selected by RF, coefficient is effectively 0
  if (!"x1" %in% top_vars) {
    hybrid_b1[i] <- 0
    cat("iteration", i, "(x1 not selected)\n")
    next
  }
  
  X_sel <- X[, top_vars, drop = FALSE]
  
  # ridge stage
  cv_r <- cv.glmnet(X_sel, y, alpha = 0)
  ridge_fit <- glmnet(X_sel, y, alpha = 0, lambda = cv_r$lambda.min)
  hybrid_b1[i] <- coef(ridge_fit)["x1", 1]
  
  cat("iteration", i, "\n")
}


cat(sprintf("hybrid - mean: %.3f, sd: %.3f, bias: %.3f\n",
            mean(hybrid_b1), sd(hybrid_b1), mean(hybrid_b1) - 3))


Modeling
================
Shea Conaway

``` r
# markdown-wide packages
```

# Data

``` r
# data
df <- read.csv('../data/2_cleaned/cleaned_data.csv')
```

``` r
# one-hot encode categoricals

# home type
# default single family
# encode condo or townhouse
df$condo <- ifelse(df$home_type == 'CONDO', 1, 0)
df$townhouse <- ifelse(df$home_type == 'TOWNHOUSE', 1, 0)

# neighborhood
# default neighborhood 1 (Green Hills / Forest Hills / Belle Meade)
neighborhoods <- unique(df$neighborhood)
num_hoods <- length(neighborhoods) # 24
# loop encoding
for(i in 2:num_hoods) {
  new <- ifelse(df$neighborhood == neighborhoods[i], 1, 0)
  df[ , ncol(df) + 1] <- new
  colnames(df)[ncol(df)] <- paste0('neighborhood_', i) 
  }
```

``` r
# subset to desired variables
df <- subset(df,select = -c(zpid
                   ,price_sqft
                   ,home_type
                   ,date_sold
                   ,date_listed
                   #,days_on_market
                   ,date_sold_previous
                   #,age
                   ,year_built
                   ,description
                   ,photo_count
                   #,longitude
                   #,latitude
                   ,neighborhood
                   ,address_state
                   ,address_city
                   ,address_zipcode
                   ,address_street
                   ,parcel_id
                   ,url
                   ,favorite_count
                   ,page_view_count
                   ,home_status))
```

``` r
# a little additional lot size cleaning
# lot size should be at least as large as living area
df$lot_size[df$lot_size < df$living_area] <- df$living_area[df$lot_size < df$living_area]
```

# Model

## Linear Regression

Linear regression models are well-understood and easily explained. They
serve as a good baseline model in a regression task to gut-check more
sophisticated approaches.

The target variable distribution is right skewed, as expected with home
prices. The log transformation does a decent job of normalizing, which
is more appropriate for a linear model. Log transformations are also
applied to the skewed features.

``` r
summary(df$price)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   62000  255000  335000  388684  452500 1449000

``` r
d = density(df$price)
plot(d, main = 'price')
polygon(d, col='gray')
```

![](modeling_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
d_log = density(log(df$price))
plot(d_log, main = 'Log Price')
polygon(d_log, col='gray')
```

![](modeling_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->

``` r
# feature transformations and imputations

# log transformations
df$bedrooms <- log(df$bedrooms)
df$bathrooms <- log(df$bathrooms)
df$living_area <- log(df$living_area)
df$lot_size <- log(df$lot_size)

# sqrt transformation
df$days_on_market[is.na(df$days_on_market)] <- mean(df$days_on_market, na.rm = TRUE)
df$days_on_market <- sqrt(df$days_on_market)
df$age <- sqrt(df$age)
```

We split our data in train/validate/test sets. The train dataset is used
for fitting our models. Validate is used for comparing models. While
train and validate will be used multiple times, our final test set is
used only once to estimate real-world performance.

``` r
# train/validate/test split
library(splitTools)
set.seed(20221217)

# 80/10/10
inds <- splitTools::partition(df$price, p = c(train = 0.6, valid = 0.1, test = 0.1))
str(inds)
```

    ## List of 3
    ##  $ train: int [1:21762] 1 4 5 6 7 8 9 10 11 14 ...
    ##  $ valid: int [1:3632] 2 12 13 26 34 37 75 89 96 106 ...
    ##  $ test : int [1:3634] 3 15 27 32 42 45 46 49 50 61 ...

``` r
train <- df[inds$train, ]
valid <- df[inds$valid, ]
test <- df[inds$test, ]
```

``` r
# linear model training
model_lm = lm(log(price) ~ ., data=train)
summary(model_lm)
```

    ## 
    ## Call:
    ## lm(formula = log(price) ~ ., data = train)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.13348 -0.11776  0.00243  0.12927  1.22531 
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      1.014e+02  5.905e+00  17.167  < 2e-16 ***
    ## bedrooms        -4.285e-03  8.316e-03  -0.515  0.60635    
    ## bathrooms        1.414e-01  6.255e-03  22.612  < 2e-16 ***
    ## living_area      5.735e-01  7.292e-03  78.641  < 2e-16 ***
    ## days_on_market  -2.039e-02  7.456e-04 -27.350  < 2e-16 ***
    ## age             -1.583e-02  6.956e-04 -22.758  < 2e-16 ***
    ## lot_size         5.315e-02  2.643e-03  20.108  < 2e-16 ***
    ## downtown_dist   -4.880e-02  1.122e-03 -43.481  < 2e-16 ***
    ## longitude        4.510e-01  6.122e-02   7.368 1.80e-13 ***
    ## latitude        -1.480e+00  7.719e-02 -19.177  < 2e-16 ***
    ## condo           -6.396e-02  6.252e-03 -10.230  < 2e-16 ***
    ## townhouse       -8.728e-02  7.194e-03 -12.132  < 2e-16 ***
    ## neighborhood_2  -4.186e-01  1.146e-02 -36.525  < 2e-16 ***
    ## neighborhood_3  -2.705e-01  1.290e-02 -20.974  < 2e-16 ***
    ## neighborhood_4  -1.672e-02  1.552e-02  -1.077  0.28134    
    ## neighborhood_5  -1.141e-01  1.049e-02 -10.871  < 2e-16 ***
    ## neighborhood_6  -3.360e-01  1.825e-02 -18.412  < 2e-16 ***
    ## neighborhood_7  -3.006e-01  1.389e-02 -21.637  < 2e-16 ***
    ## neighborhood_8  -3.307e-01  1.358e-02 -24.356  < 2e-16 ***
    ## neighborhood_9  -3.750e-01  1.486e-02 -25.228  < 2e-16 ***
    ## neighborhood_10 -5.415e-01  1.492e-02 -36.292  < 2e-16 ***
    ## neighborhood_11 -1.271e-01  1.623e-02  -7.834 4.93e-15 ***
    ## neighborhood_12 -2.969e-01  1.856e-02 -16.001  < 2e-16 ***
    ## neighborhood_13 -4.908e-01  1.622e-02 -30.258  < 2e-16 ***
    ## neighborhood_14 -1.745e-01  2.050e-02  -8.510  < 2e-16 ***
    ## neighborhood_15 -5.811e-01  1.560e-02 -37.240  < 2e-16 ***
    ## neighborhood_16 -1.198e-01  1.441e-02  -8.311  < 2e-16 ***
    ## neighborhood_17  3.866e-02  1.175e-02   3.291  0.00100 ***
    ## neighborhood_18 -1.260e-01  1.276e-02  -9.871  < 2e-16 ***
    ## neighborhood_19  4.192e-02  1.290e-02   3.250  0.00116 ** 
    ## neighborhood_20 -4.091e-01  1.514e-02 -27.015  < 2e-16 ***
    ## neighborhood_21  8.198e-02  1.388e-02   5.906 3.55e-09 ***
    ## neighborhood_22 -1.639e-01  3.926e-02  -4.174 3.00e-05 ***
    ## neighborhood_23 -1.685e-01  1.733e-02  -9.720  < 2e-16 ***
    ## neighborhood_24  1.091e-01  3.987e-02   2.736  0.00622 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.2095 on 21727 degrees of freedom
    ## Multiple R-squared:  0.7897, Adjusted R-squared:  0.7893 
    ## F-statistic:  2399 on 34 and 21727 DF,  p-value: < 2.2e-16

It is good practice to check for multicollinearity in the predictors for
a linear model, as the presence of relationships between the predictors
can make coefficients and their p-values unreliable. Here, we have some
severe variance inflaction factor scores for variables having to do with
location.

Given that our primary interest is in prediction performance, rather
than the independent relationship between each predictor and the price
target, we can leave this issue unaddressed.

``` r
# check for multicollinearity in our predictors
library(car) # variance inflation factor
```

    ## Loading required package: carData

``` r
vif <- car::vif(model_lm) # variance inflation factor
vif[vif > 5] # severe
```

    ##   downtown_dist       longitude        latitude  neighborhood_2  neighborhood_6 
    ##        8.129839       18.456485       15.037132        5.878043       11.565420 
    ##  neighborhood_8 neighborhood_10 neighborhood_11 neighborhood_12 neighborhood_14 
    ##        5.086193       12.284257        8.453308        7.042106        6.906328 
    ## neighborhood_20 
    ##        5.718117

In our diagnostic plots, we’re assessing the assumptions we’ve made in a
linear model. - In the Residuals vs Fitted plot, we’re seeing a stable
goodness of fit for most of the fitted value range, with some issues at
the tails. Also, the spread of residuals shifts at the upper end,
representing some homoskedasticity. - In the Normal QQ-plot, we’re
seeing decent normality in the residuals but evidence of some non-linear
characteristics not captured by our model. - The Cook’s distance plot
checks for observations that have high influence on the model fit. Here
there are three observations worthy of investigation.

``` r
# linear regression diagnostic plots
dev.new(width=100, height=50, unit="px")
plot(model_lm, which = c(1,2,4))
```

``` r
# linear regression prediction and root mean squared error
pred_lm <- predict(model_lm, newdata = valid)
rmse_lm <- sqrt(sum((exp(pred_lm) - valid$price)^2)/length(valid$price))
rmse_lm
```

    ## [1] 95309.27

In this plot of actual vs predicted, the model performs better for lower
cost housing.

Over \$700,000 the model appears to underestimate prices. Only 8% of the
houses are in this range. For more expensive houses, there are likely
characteristics we don’t have in our data that capture some of their
value. Think luxury features like hardwood floors, expensive lighting
fixtures, ensuite bathrooms, etc.

``` r
# plot
plot(valid$price, exp(pred_lm))
abline(coef = c(0, 1), c = 'red')
```

![](modeling_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
sum(df$price > 700000)/length(df$price)
```

    ## [1] 0.07820036

## XGBoost

``` r
# xgboost package
library(xgboost) 
```

XGBoost belongs to a class of models popular throughout many industries
because of superior performance on a variety problems. Its benefits
include capturing non-linear relationships, detecing complex
interactions, and robustness to outliers and other data issues.

An XGBoost model consists of many weak classifiers trained iteratively
to reduce residuals, also known as boosting. This decision-tree based
ensemble algorithm uses the gradient boosting framework, which allows
for flexibility in loss function selection.

``` r
# additional xgboost data formatting

# train
train_x = data.matrix(train[, -1])
train_y = train[,1]
# test
valid_x = data.matrix(valid[, -1])
valid_y = valid[,1]
# final format for xgboost
xgb_train = xgb.DMatrix(data = train_x, label = train_y)
xgb_valid = xgb.DMatrix(data = valid_x, label = valid_y)
```

``` r
# xgboost training
model_xgb = xgb.train(data = xgb_train, max.depth = 3, nrounds = 350)
```

``` r
# xgb prediction and root mean squared error
pred_xgb <- predict(model_xgb, newdata = valid_x)
rmse_xgb <- sqrt(sum((pred_xgb - valid$price)^2)/length(valid$price))
rmse_xgb
```

    ## [1] 80350.32

XGBoost results in a 16% reduction in RMSE.

Although not as pronounced as the linear fit, the XGBoost model is still
underestimating more expensive homes.

``` r
# plot
plot(valid$price, pred_xgb, main = 'XGBoost Predicted vs Actuals', xlab = 'Actuals', ylab = 'Predicted')
abline(coef = c(0, 1), c = 'red')
```

![](modeling_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

``` r
1 - rmse_xgb/rmse_lm # performance comparison
```

    ## [1] 0.1569517

``` r
# prep analysis dataframe
xgb_analysis <- data.frame(1:length(valid$price),valid$price, pred_xgb)
xgb_analysis$ae <- abs(xgb_analysis$valid.price - xgb_analysis$pred_xgb) # absolute error
xgb_analysis$ae_p <- xgb_analysis$ae/xgb_analysis$valid.price
```

The mean absolute error for predicted price is \$53k or 15% of true home
price. The median figures are \$35k and 11%.

``` r
summary(xgb_analysis$ae)
```

    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ##      6.4  16170.1  35361.2  53210.7  69894.5 807892.2

``` r
summary(xgb_analysis$ae_p)
```

    ##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
    ## 0.0000157 0.0475862 0.1060771 0.1456514 0.1906463 1.8836322

## SAINT (Work in Progress)

### Self-Attention and Intersample Attention Transformer

-   A hybrid deep learning approach to solving tabular data problems
-   SAINT performs attention over both rows and columns and includes an
    enhanced embedding method

# Test

Once we’ve settled on our modeling decisions, we can train our test
model on all the non-test data and assess real-world performance on the
test set. I’m not ready to do this yet.

``` r
# test model training

# model_test = model(price ~ ., data=(train + valid))
```

``` r
# final model prediction and root mean squared error

# pred_test <- predict(model_test, newdata = test)
# rmse_test <- sqrt(sum(pred_test - test$price)^2)/length(test$price))
# rmse_test
```

# Final Model

When it’s all said and done, we can train our final model on all the
data we have. Then we’re ready to use our model to price some houses!

``` r
# final model training

# model_final = model(price ~ ., data=df)
```

Modeling
================
Shea Conaway

``` r
# packages
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
# time variables

# basic time variable
# captures inflation in nashville market
df$time <- as.numeric(as.Date(df$date_sold) - as.Date('2021-11-11'))

# days between
df$days_since_previous_sale <- as.numeric(as.Date(df$date_sold) - as.Date(df$date_sold_previous))
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
# feature transformations and imputations

# imputation
df$days_on_market[is.na(df$days_on_market)] <- mean(df$days_on_market, na.rm = TRUE)
df$days_since_previous_sale[is.na(df$days_since_previous_sale)] <- 0 # never sold before
df$previous_price[is.na(df$previous_price)] <- df$price[is.na(df$previous_price)] # fill na's
df$previous_price[df$previous_price > df$price] <- df$price[df$previous_price > df$price] # price should increase
df$lot_size[df$lot_size < df$living_area] <- df$living_area[df$lot_size < df$living_area] # lot size at least sqft

# log transformations
df$bedrooms <- log(df$bedrooms)
df$bathrooms <- log(df$bathrooms)
df$living_area <- log(df$living_area)
df$lot_size <- log(df$lot_size)

# sqrt transformations
df$days_on_market <- sqrt(df$days_on_market)
df$age <- sqrt(df$age)
df$previous_price <- sqrt(df$previous_price)
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

![](modeling_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
d_log = density(log(df$price))
plot(d_log, main = 'Log Price')
polygon(d_log, col='gray')
```

![](modeling_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->

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
    ## -1.12772 -0.09313  0.00243  0.10033  1.11912 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               8.312e+01  4.988e+00  16.664  < 2e-16 ***
    ## bedrooms                  8.273e-03  7.015e-03   1.179 0.238307    
    ## bathrooms                 1.229e-01  5.279e-03  23.284  < 2e-16 ***
    ## living_area               4.255e-01  6.569e-03  64.771  < 2e-16 ***
    ## days_on_market           -7.523e-03  6.493e-04 -11.587  < 2e-16 ***
    ## previous_price            8.376e-04  1.355e-05  61.804  < 2e-16 ***
    ## age                      -1.166e-02  6.076e-04 -19.198  < 2e-16 ***
    ## lot_size                  5.756e-02  2.236e-03  25.746  < 2e-16 ***
    ## downtown_dist            -3.758e-02  9.592e-04 -39.176  < 2e-16 ***
    ## longitude                 3.775e-01  5.163e-02   7.310 2.76e-13 ***
    ## latitude                 -1.139e+00  6.526e-02 -17.458  < 2e-16 ***
    ## condo                    -5.904e-02  5.273e-03 -11.196  < 2e-16 ***
    ## townhouse                -9.385e-02  6.080e-03 -15.435  < 2e-16 ***
    ## neighborhood_2           -2.941e-01  9.916e-03 -29.657  < 2e-16 ***
    ## neighborhood_3           -1.561e-01  1.109e-02 -14.076  < 2e-16 ***
    ## neighborhood_4           -1.847e-03  1.310e-02  -0.141 0.887833    
    ## neighborhood_5           -5.468e-02  8.926e-03  -6.126 9.16e-10 ***
    ## neighborhood_6           -2.421e-01  1.549e-02 -15.624  < 2e-16 ***
    ## neighborhood_7           -1.946e-01  1.183e-02 -16.450  < 2e-16 ***
    ## neighborhood_8           -2.126e-01  1.165e-02 -18.241  < 2e-16 ***
    ## neighborhood_9           -2.400e-01  1.277e-02 -18.788  < 2e-16 ***
    ## neighborhood_10          -3.970e-01  1.285e-02 -30.908  < 2e-16 ***
    ## neighborhood_11          -6.059e-02  1.372e-02  -4.416 1.01e-05 ***
    ## neighborhood_12          -2.009e-01  1.576e-02 -12.741  < 2e-16 ***
    ## neighborhood_13          -3.522e-01  1.393e-02 -25.289  < 2e-16 ***
    ## neighborhood_14          -1.152e-01  1.735e-02  -6.641 3.19e-11 ***
    ## neighborhood_15          -4.285e-01  1.343e-02 -31.907  < 2e-16 ***
    ## neighborhood_16          -5.025e-02  1.224e-02  -4.105 4.07e-05 ***
    ## neighborhood_17           3.722e-02  9.904e-03   3.757 0.000172 ***
    ## neighborhood_18          -5.668e-02  1.084e-02  -5.228 1.73e-07 ***
    ## neighborhood_19           3.641e-02  1.088e-02   3.348 0.000816 ***
    ## neighborhood_20          -2.898e-01  1.294e-02 -22.391  < 2e-16 ***
    ## neighborhood_21           8.052e-02  1.170e-02   6.882 6.08e-12 ***
    ## neighborhood_22          -9.243e-02  3.313e-02  -2.790 0.005276 ** 
    ## neighborhood_23          -1.107e-01  1.465e-02  -7.562 4.14e-14 ***
    ## neighborhood_24           1.769e-01  3.362e-02   5.261 1.44e-07 ***
    ## time                      2.331e-04  4.020e-06  57.983  < 2e-16 ***
    ## days_since_previous_sale  1.425e-05  6.474e-07  22.008  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1766 on 21724 degrees of freedom
    ## Multiple R-squared:  0.8506, Adjusted R-squared:  0.8503 
    ## F-statistic:  3342 on 37 and 21724 DF,  p-value: < 2.2e-16

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
    ##        8.354738       18.476390       15.125233        6.193188       11.730566 
    ##  neighborhood_8 neighborhood_10 neighborhood_11 neighborhood_12 neighborhood_14 
    ##        5.271122       12.814833        8.507649        7.151156        6.958939 
    ## neighborhood_16 neighborhood_20 
    ##        5.065349        5.877659

In our diagnostic plots, we’re assessing the assumptions we’ve made in a
linear model. - In the Residuals vs Fitted plot, we’re seeing a stable
goodness of fit for most of the fitted value range, with some issues at
the tails. Also, the spread of residuals shifts at the upper end,
representing some homoskedasticity. - In the Normal QQ-plot, we’re
seeing strong evidence of non-linear characteristics not captured by our
model based on the non-normality of the residuals. - The Cook’s distance
plot checks for observations that have high influence on the model fit.
Here there are three observations worthy of investigation.

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

    ## [1] 79819.37

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

    ## [1] 59983.95

XGBoost results in a 25% reduction in RMSE compared to the linear fit.

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

    ## [1] 0.2485038

``` r
# prep analysis dataframe
xgb_analysis <- data.frame(1:length(valid$price),valid$price, pred_xgb)
xgb_analysis$ae <- abs(xgb_analysis$valid.price - xgb_analysis$pred_xgb) # absolute error
xgb_analysis$ae_p <- xgb_analysis$ae/xgb_analysis$valid.price
```

The mean absolute error for predicted price is \$36k or 10% of true home
price. The median figures are \$22k and 7%.

``` r
summary(xgb_analysis$ae)
```

    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ##     11.7   9436.9  22425.4  36000.8  43950.3 787900.2

``` r
summary(xgb_analysis$ae_p)
```

    ##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
    ## 0.0000394 0.0297687 0.0658148 0.0982233 0.1207899 2.2455532

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

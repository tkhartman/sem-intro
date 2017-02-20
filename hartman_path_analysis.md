Path Analysis Using R
================
Todd K. Hartman
2017-02-20

Housekeeping

``` r
## Install and load necessary packages using 'pacman'
# install.packages('pacman')  # First installation only (uncomment and run if needed)
pacman::p_load(lavaan)

## Check and set your working directory (Uncomment for your system)
getwd()  # Get the current working directory
```

    ## [1] "/R"

``` r
# setwd("C:/ENTER/YOUR/FOLDER/PATH/HERE")  # For PC (Note the forward slashes!)
# setwd("~/ENTER/YOUR/FOLDER/PATH/HERE")  # For Mac OSX 
```

Load the dataset

``` r
maths <- read.csv("MathHmwk.csv")
head(maths)  # Check the first rows of the data
```

    ##   homework achievement
    ## 1        2          54
    ## 2        0          53
    ## 3        4          53
    ## 4        0          56
    ## 5        2          59
    ## 6        0          30

OLS regression model using the lm() function

``` r
## 'achievement' is the outcome variable; 'homework' is the predictor
## we save the regression output as an object called 'math.lm'
math.lm <- lm(achievement ~ homework, data = maths)
summary(math.lm)  # Display the results from the regression model
```

    ## 
    ## Call:
    ## lm(formula = achievement ~ homework, data = maths)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -26.0120  -8.0071   0.4979   8.0101  21.9979 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  47.0316     1.6940  27.763  < 2e-16 ***
    ## homework      1.9902     0.5952   3.344  0.00117 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 10.75 on 98 degrees of freedom
    ## Multiple R-squared:  0.1024, Adjusted R-squared:  0.09324 
    ## F-statistic: 11.18 on 1 and 98 DF,  p-value: 0.001173

Standardized OLS regression model using the scale() function

``` r
math2.lm <- lm(scale(achievement) ~ scale(homework), data = maths)
summary(math2.lm)
```

    ## 
    ## Call:
    ## lm(formula = scale(achievement) ~ scale(homework), data = maths)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.30478 -0.70946  0.04411  0.70973  1.94911 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)     3.267e-16  9.522e-02   0.000  1.00000   
    ## scale(homework) 3.200e-01  9.570e-02   3.344  0.00117 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.9522 on 98 degrees of freedom
    ## Multiple R-squared:  0.1024, Adjusted R-squared:  0.09324 
    ## F-statistic: 11.18 on 1 and 98 DF,  p-value: 0.001173

Path analysis (structural model) using the 'lavaan' package

``` r
math.model <- ' achievement ~ homework '  # Note the single quotes
math.fit <- sem(math.model, data = maths)  # Unstandardized results with an intercept
summary(math.fit)  # Display the results and compare to OLS (intercept will not be shown)
```

    ## lavaan (0.5-22) converged normally after  15 iterations
    ## 
    ##   Number of observations                           100
    ## 
    ##   Estimator                                         ML
    ##   Minimum Function Test Statistic                0.000
    ##   Degrees of freedom                                 0
    ##   Minimum Function Value               0.0000000000000
    ## 
    ## Parameter Estimates:
    ## 
    ##   Information                                 Expected
    ##   Standard Errors                             Standard
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   achievement ~                                       
    ##     homework          1.990    0.589    3.378    0.001
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .achievement     113.190   16.007    7.071    0.000

``` r
math2.fit <- sem(math.model, data = maths, meanstructure = TRUE)  # Unstandardized results with an intercept
summary(math2.fit)  # Display the results and compare to OLS (intercept will be shown now)
```

    ## lavaan (0.5-22) converged normally after  17 iterations
    ## 
    ##   Number of observations                           100
    ## 
    ##   Estimator                                         ML
    ##   Minimum Function Test Statistic                0.000
    ##   Degrees of freedom                                 0
    ## 
    ## Parameter Estimates:
    ## 
    ##   Information                                 Expected
    ##   Standard Errors                             Standard
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   achievement ~                                       
    ##     homework          1.990    0.589    3.378    0.001
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .achievement      47.032    1.677   28.045    0.000
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .achievement     113.190   16.007    7.071    0.000

Path analysis (standardized results)

``` r
##  Model without an intercept (standardized results are in the Std.all column)
summary(math.fit, standardized = TRUE)
```

    ## lavaan (0.5-22) converged normally after  15 iterations
    ## 
    ##   Number of observations                           100
    ## 
    ##   Estimator                                         ML
    ##   Minimum Function Test Statistic                0.000
    ##   Degrees of freedom                                 0
    ##   Minimum Function Value               0.0000000000000
    ## 
    ## Parameter Estimates:
    ## 
    ##   Information                                 Expected
    ##   Standard Errors                             Standard
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   achievement ~                                                         
    ##     homework          1.990    0.589    3.378    0.001    1.990    0.320
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .achievement     113.190   16.007    7.071    0.000  113.190    0.898

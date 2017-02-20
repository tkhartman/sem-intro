##' ---
##' title: "Path Analysis Using R"
##' author: "Todd K. Hartman"
##' date: "`r format(Sys.Date())`"
##' output: "github_document"
##' ---

##' Housekeeping
## Install and load necessary packages using 'pacman'
# install.packages('pacman')  # First installation only (uncomment and run if needed)
pacman::p_load(lavaan)

## Check and set your working directory (Uncomment for your system)
getwd()  # Get the current working directory
# setwd("C:/ENTER/YOUR/FOLDER/PATH/HERE")  # For PC (Note the forward slashes!)
# setwd("~/ENTER/YOUR/FOLDER/PATH/HERE")  # For Mac OSX 

##' Load the dataset
maths <- read.csv("MathHmwk.csv")
head(maths)  # Check the first rows of the data

##' OLS regression model using the lm() function
## 'achievement' is the outcome variable; 'homework' is the predictor
## we save the regression output as an object called 'math.lm'
math.lm <- lm(achievement ~ homework, data = maths)
summary(math.lm)  # Display the results from the regression model

##' Standardized OLS regression model using the scale() function
math2.lm <- lm(scale(achievement) ~ scale(homework), data = maths)
summary(math2.lm)

##' Path analysis (structural model) using the 'lavaan' package
math.model <- ' achievement ~ homework '  # Note the single quotes
math.fit <- sem(math.model, data = maths)  # Unstandardized results with an intercept
summary(math.fit)  # Display the results and compare to OLS (intercept will not be shown)
math2.fit <- sem(math.model, data = maths, meanstructure = TRUE)  # Unstandardized results with an intercept
summary(math2.fit)  # Display the results and compare to OLS (intercept will be shown now)

##' Path analysis (standardized results)
##  Model without an intercept (standardized results are in the Std.all column)
summary(math.fit, standardized = TRUE)


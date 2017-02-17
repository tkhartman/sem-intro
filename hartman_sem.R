##' ---
##' title: "Structural Equation Modelling Using R"
##' author: "Todd K. Hartman"
##' date: "`r format(Sys.Date())`"
##' output: "github_document"
##' ---

##' Empathy Indicators
## Davis (1980, 1983) Interpersonal Reactivity Index
## 4 Sub-scales: Perspective-Taking; Empathic Concern; Personal Distress; Fantasy
## Response options 1 - 5, where 5 = "Describes me very well"
## [ept2] W22Q1_5. I try to look at everybody's side of a disagreement before I make a decision.
## [ept3] W22Q1_8. I sometimes try to understand my friends better by imagining how things look from their perspective.
## [ept5] W22Q1_16. I believe that there are two sides to every question and try to look at them both.
## [ept7] W22Q1_21. Before criticizing somebody, I try to imagine how I would feel if I were in their place.
## [ec1] W22Q1_1. I often have tender, concerned feelings for people less fortunate than me.
## [ec2] W22Q1_3. Sometimes I don't feel very sorry for other people when they are having problems. (R)
## [ec4] W22Q1_10. Other people's misfortunes do not usually disturb me a great deal. (R)
## [epd4] W22Q1_12. Being in a tense emotional situation scares me.


##' Housekeeping 
## Install and load necessary packages using 'pacman'
install.packages('pacman')  # First installation only
pacman::p_load(haven, lavaan)  # haven loads foreign datasets; lavaan is for SEM

## Check and set your working directory (Uncomment for your type of computer OS)
getwd()  # Get the current working directory
# setwd("C:/ENTER/YOUR/FOLDER/PATH/HERE")  # For PC (Note the forward slashes!)
# setwd("~/ENTER/YOUR/FOLDER/PATH/HERE")  # For Mac OSX 

##' Load the ANES Panel Dataset (from Stata; use 'read_spss' for SPSS)
anes <- read_dta("anes.dta")
head(anes)  # Check the first rows of the data

##' CFA for Empathy (Note the single quote marks!)
## 1-Factor Model of empathy forcing cognitive and affective items 
## to load onto a single latent construct
cfa1 <- ' empathy =~ ept2 + ept3 + ept5 + ept7 + ec1 + ec2 + ec4 + epd4 '
model.cfa1 <- cfa(cfa1,                  # Use the 'cfa()' wrapper for the 'cfa1' model
           data=anes,                  # Specify the dataset
           ordered=c("ept2","ept3",    # Use this for ordinal indicators
                     "ept5","ept7",
                     "ec1", "ec2",
                     "ec4", "epd4"))
summary(model.cfa1)  # Display the CFA results
standardizedSolution(model.cfa1)  # Print standardized estimates
fitMeasures(model.cfa1)  # Model fit statistics (e.g., RMSEA, CFI, TLI)
semPaths(model.cfa1, "std", edge.label.cex = 1, curvePivot = FALSE)  # Make figure of std. results

## 2-Factor Model of empathy
cfa2 <- ' cempathy =~ ept2 + ept3 + ept5 + ept7
          aempathy =~ ec1 + ec2 + ec4 + epd4
        '
model.cfa2 <- cfa(cfa2,
                data=anes, 
                ordered=c("ept2","ept3",
                          "ept5","ept7",
                          "ec1", "ec2",
                          "ec4", "epd4"))
summary(model.cfa2)
standardizedSolution(model.cfa2)
fitMeasures(model.cfa2)  # The 2-factor CFA fits the data better
semPaths(model.cfa2, "std", edge.label.cex = 1, curvePivot = FALSE)  # Make figure of std. results



## Full SEM for Empathy, Humanitarianism, and Social Spending
sem1 <- '  # Measurement component
            cempathy =~ ept2 + ept3 + ept5 + ept7
            aempathy =~ ec1 + ec2 + ec4 + epd4
            human =~ hu1 + hu2 + hu3 + hu4 + hu5 + hu6 + hu7 + hu8
            social =~ school15 + ss15 + poor15 + job15
            
            # Structural component (regressions)
            aempathy ~ a*cempathy
            human ~ b*aempathy
            social ~ c*human
            social ~ ideology + party + male + age + hispanic + 
                        nonwhite + education + income + south

            # Indirect Effects 
            ab := a*b
            bc := b*c
            abc := a*b*c         
            
            # Residuals
            cempathy ~~ aempathy  
        '
model.sem1 <- sem(sem1,
           data=anes, 
           ordered=c("ept2","ept3",
                     "ept5","ept7",
                     "ec1", "ec2",
                     "ec4", "epd4",
                     "hu1","hu2",
                     "hu3","hu4",
                     "hu5","hu6",
                     "hu7","hu8"))
summary(model.sem1, standardized = TRUE)
parameterEstimates(model.sem1)
fitMeasures(model.sem1)



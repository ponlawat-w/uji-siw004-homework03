data <- read.csv('./data.csv', header = TRUE)

# Perform a PCA and interpret the results.

data <- data[, -1] # Eliminate `Group` column
attach(data)

# Column Heart-Lung Function Value `Factor68` (third column) was selected to be dependent variable
data.principleComponent <- princomp(data[, -3], cor = TRUE)
summary(data.principleComponent, loadings = TRUE)

# Result
# Importance of components:
#                           Comp.1    Comp.2    Comp.3
# Standard deviation     1.1938606 1.0020548 0.7553694
# Proportion of Variance 0.4751011 0.3347046 0.1901943
# Cumulative Proportion  0.4751011 0.8098057 1.0000000

# Loadings:
#        Comp.1 Comp.2 Comp.3
# HR             0.994  0.102
# BW      0.706         0.702
# Gesage  0.708        -0.705

# From the result of principle component analysis, two first components are chosen because the cumulative proportion starts to be over 0.8 from the second component.
# From loadings data, the first component is mostly dominant by birth weight and gestational age, while the second component is composed from only heart rate. Lastly, the ignored third component is composed from birth weight and negative relation of gestational age.
# It is able to write the equation of linear regression to predict the value of heart-lung function value (f):
## f = β0 + β1y1 + β2y2

linearRegressionModel <- lm(
  Factor68 ~ (
    data.principleComponent$scores[, 1] +
    data.principleComponent$scores[, 2]
  )
)

linearRegressionModel
# Result
# Call:
# lm(formula = Factor68 ~ (data.principleComponent$scores[, 1] +
#     data.principleComponent$scores[, 2]))

# Coefficients:
#                         (Intercept)  data.principleComponent$scores[, 1]  
#                             0.33322                             -0.01357  
# data.principleComponent$scores[, 2]  
#                             0.01760
## f = 0.33322 - 0.01357y1 + 0.01760y2

# Intepret the linear regression model
summary(linearRegressionModel)

# Call:
# lm(formula = Factor68 ~ (data.principleComponent$scores[, 1] +
#     data.principleComponent$scores[, 2]))
# 
# Residuals:
#      Min       1Q   Median       3Q      Max 
# -0.17765 -0.04986 -0.01077  0.03558  0.28108 
# 
# Coefficients:
#                                      Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                          0.333215   0.010592  31.459   <2e-16 ***
# data.principleComponent$scores[, 1] -0.013570   0.008872  -1.529    0.131    
# data.principleComponent$scores[, 2]  0.017602   0.010570   1.665    0.101    
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.0854 on 62 degrees of freedom
# Multiple R-squared:  0.07617,   Adjusted R-squared:  0.04637 
# F-statistic: 2.556 on 2 and 62 DF,  p-value: 0.08576

# From the result of linear regression summary, it is noticeable that Pr(>|t|) of components scores are greater than 0.05, which mean they are not significant to the model. In other words, the regression model of principle component analysis is not fitting to the data, as its R-square value is also very low.

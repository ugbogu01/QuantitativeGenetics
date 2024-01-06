#What is the corresponding p-value for the F-score in the table? Is the difference of means between treatments significant at level α=0.05?

pf(8, 3, 16, lower.tail = F)
## [1] 0.001755584
1 - pf(8, 3, 16)
## [1] 0.001755584

#Answer: P-value is smaller than 0.05, so we rejected null hypothesis, i.e. the difference of means between treatments are statistical significant.

#Question 2
#Read the data set of maize yield (Ex2.csv) and format it to the data frame that ANOVA required. For this question, please use the first three columns in raw data corresponding to three environments measurement in first family only. Assume that all samples are independent, please write the one-way ANOVA table for environment factors. What conclusion you can make from this ANOVA table?
  #### load data ####
data <- read.table("Ex2.csv", sep = ",", header = T)[, 2:7]
#### format data ####
n <- nrow(data)
m <- ncol(data)
anovadata <- NULL 
for (i in 1:6) {
  datai <- as.matrix(data[, i], n, 1)
  anovadata <- rbind(anovadata, datai)
}
Edata <- rep(c(rep("E1", n), rep("E2", n), rep("E3", n)),
             2)
Fdata <- c(rep("F1", n * m/2), rep("F2", n * m/2))

anovadata <- data.frame(anovadata, Edata, Fdata)
colnames(anovadata) <- c("yield", "env", "family")
#### use first family only ####
anovadata1 <- anovadata[1:150, ]
#### run one-way ANOVA ####
summary(aov(yield ~ env, data = anovadata1))
##              Df Sum Sq Mean Sq F value Pr(>F)
## env           2 150.98   75.49   205.7 <2e-16 ***
## Residuals   147  53.95    0.37
## ---
## Signif. codes:  0 ’***’ 0.001 ’**’ 0.01 ’*’ 0.05 ’.’ 0.1 ’ ’ 1
#Answer: The means between environmental groups are significantly different.

#Question 3
#Use the data from previous exercise (Ex1.csv) to build regression model for body weight against body height, as below:
#  Weight = X + Height × β + ε
#(1) What is the value of t-statistic of regression coefficient of intercept and height, respectively? How did these values be calculated? Write the corresponding numerical equations based on the regression summary table.
#### load data ####
data <- read.table("Ex1.csv", sep = ",", header = T) ## Modified data from Machine Learning for Hackers, ## Drew Conway et al.
#### regression model ####
Height <- data$Height
Weight <- data$Weight
df <- data.frame(Height, Weight)
lm <- lm(Weight ~ Height, data = df)
out <- summary(lm)
out
##
## Call:
## lm(formula = Weight ~ Height, data = df)
##
## Residuals:
##      Min       1Q   Median       3Q      Max
## -23.5555  -3.7343  -0.0522   3.7507  21.2448
##
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)


## (Intercept) -1.590e+02  9.580e-01  -166.0   <2e-16 ***
## Height       1.378e+00  5.674e-03   242.8   <2e-16 ***
## ---
## Signif. codes:  0 ’***’ 0.001 ’**’ 0.01 ’*’ 0.05 ’.’ 0.1 ’ ’ 1
##
## Residual standard error: 5.544 on 9998 degrees of freedom
## Multiple R-squared:  0.855,  Adjusted R-squared:  0.855
## F-statistic: 5.896e+04 on 1 and 9998 DF,  p-value: < 2.2e-16
out$coefficients[1, 1]/out$coefficients[1, 2]
## [1] -165.9684
out$coefficients[2, 1]/out$coefficients[2, 2]
## [1] 242.8084
#Answer: The t value of Intercept is -1.590e+02/9.580e-01, the t value of Height is 1.378e+00/5.674e-03.

#(2) Try to write the ANOVA table for this regression. What is the value of degree of freedom of each coefficient, and why? Explain in numerical equations. What is the value of F-statistic? Is it significant at level α=0.05? Try to calculate the Coefficient of determination (R2) based on the ANOVA table.
anova(lm)
## Analysis of Variance Table
##
## Response: Weight
##             Df  Sum Sq Mean Sq F value    Pr(>F)
## Height       1 1812337 1812337   58956 < 2.2e-16 ***
## Residuals 9998  307344      31
## ---
## Signif. codes:  0 ’***’ 0.001 ’**’ 0.01 ’*’ 0.05 ’.’ 0.1 ’ ’ 1
#Answer: The df of Height is number of predictors p=1 and df of Residual is n-p-1=10000-1-1=9998.
#The F-score is 58956, the corresponding p-value is < 2.2e-16 which is significant level α=0.05. The regression coefficient of Height is significant.
#R2 is 1-SS_residual/SS_total=1-307344/(1812337+307344)=0.855.


#Question 4
#Are the mean values in three environment groups significantly different between each other? Try to use several multiple comparison methods to clarify, including Bonferroni, Holm, HSD. Try to understand the HSD method, what is the principle idea of HSD method? Are the conclusions from different methods identical?
#  pairwise.t.test(anovadata1$yield, anovadata1$env, p.adj = "bonferroni")
##
##  Pairwise comparisons using t tests with pooled SD
##
## data:  anovadata1$yield and anovadata1$env
##
##    E1     E2
## E2 <2e-16 -
## E3 <2e-16 1
##
## P value adjustment method: bonferroni
pairwise.t.test(anovadata1$yield, anovadata1$env, p.adj = "holm")
##
##  Pairwise comparisons using t tests with pooled SD
##
## data:  anovadata1$yield and anovadata1$env
##
##    E1     E2
## E2 <2e-16 -
## E3 <2e-16 0.37
##
## P value adjustment method: holm
TukeyHSD(aov(yield ~ env, data = anovadata1))
##   Tukey multiple comparisons of means
##     95% family-wise confidence level
##
## Fit: aov(formula = yield ~ env, data = anovadata1)
##
## $env
##            diff        lwr       upr     p adj
## E2-E1 2.0722351  1.7853505 2.3591197 0.0000000
## E3-E1 2.1801679  1.8932833 2.4670525 0.0000000
## E3-E2 0.1079328 -0.1789518 0.3948174 0.6469723
#Answer: The means between E1 and E2 as well as E1 and E3 are significant different. But the mean difference between E2 and E3 is not significant. The three multiple comparison approaches have the same conclusion.


#Question 5
#For this question, please use all raw data corresponding to two factors: environment and family. Assume that all samples are independent, please write the two-way ANOVA tables with or without interaction, respectively in R. Please write the equations how to calculate the degree of freedom of each factor, and try to understand how to get the sum square of each factor in the two-way with interaction ANOVA table. What conclusions you can make from the ANOVA tables with or without interaction, respectively?
summary(aov(yield ~ env + family, data = anovadata))
##
## env
## family
## Residuals
## ---
## Signif. codes:  0 ’***’ 0.001 ’**’ 0.01 ’*’ 0.05 ’.’ 0.1 ’ ’ 1
#Df Sum Sq Mean Sq F value Pr(>F)
#2   256.60  128.30  337.41 <2e-16 ***
#1   32.86   32.86   86.41 <2e-16 ***
#296 112.55   0.38
summary(aov(yield ~ env * family, data = anovadata))
##
## env
## family
## env:family    2  16.68    8.34   25.58 5.73e-11 ***
## Residuals   294  95.87    0.33
## ---
## Signif. codes:  0 ’***’ 0.001 ’**’ 0.01 ’*’ 0.05 ’.’ 0.1 ’ ’ 1
anova(lm(yield ~ env * family, data = anovadata))
## Analysis of Variance Table
##
## Response: yield
#Df Sum Sq Mean Sq F value   Pr(>F)
#2 256.60  128.30  393.45  < 2e-16 ***
#1  32.86   32.86  100.76  < 2e-16 ***
  ##
  ## env
  ## family
  ## env:family   2  16.683   8.342   25.58 5.726e-11 ***
  ## Residuals  294  95.871   0.326
  ## ---
  ## Signif. codes:  0 ’***’ 0.001 ’**’ 0.01 ’*’ 0.05 ’.’ 0.1 ’ ’ 1

#Answer: Degree of freedom of residuals: (300-1)-(3-1)-(2-1)=296
#Degree of freedom of interaction: (3-1) × (2-1)=2
#Degree of freedom of residuals: 3× 2 × (50-1)=294 or 300-3× 2=294 The the means between environment, family, and environment family interaction are all significant different.
#Df  Sum Sq Mean Sq F value    Pr(>F)

 
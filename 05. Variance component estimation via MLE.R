# Question 1
# Read data
data <- read.table("Ex4_1.csv", sep = ",", header = T)
data <- data.frame(data)

# Fit linear mixed-effects model
varcomp <- lmer(yield ~ 1 + (1 | geno) + (1 | env) + (1 | geno:env), data = data)
summary(varcomp)

# Perform EMSANOVA analysis
tr <- EMSanova(yield ~ geno + env, data = data, type = c("R", "R"))

# Calculate variance components
geno <- (tr$MS[1] - tr$MS[3])/12
geno
env <- (tr$MS[2] - tr$MS[3])/18
env
geno.env <- (tr$MS[3] - tr$MS[4])/6
geno.env

# Answer: The variance components estimated from the ANOVA and MLE are the same (the last digits might be different).

# Question 2
# Extract fixed and random effects
f <- fixef(varcomp)
f
r <- ranef(varcomp)$geno
r

# Perform likelihood ratio test
varcompr <- lmer(yield ~ 1 + (1 | geno) + (1 | env), data = data)
anova(varcompr, varcomp, test = "LRT")

# Perform LR test using lmtest
library(lmtest)
lrtest(varcompr, varcomp)

# Answer: The interaction term is significant at α=0.01.

# Question 3
# Read data
data <- read.table("Ex4_2.csv", sep = ",", header = T)
data <- data.frame(data)

# Fit linear mixed-effects model
df <- lmer(yield ~ 1 + (1 | sire/dame), data = data)
summary(df)

# Perform EMSANOVA analysis with nested factors
tr <- EMSanova(yield ~ sire + dame, data = data, type = c("R", "R"), nested = c(NA, "sire"))

# Calculate variance components
sire <- (tr$MS[1] - tr$MS[2])/105
sire
dame <- (tr$MS[2] - tr$MS[3])/21
dame

# Extract additive and dominance effects
var.trans <- lme4::VarCorr(df)
dame <- as.numeric(var.trans[1])
sire <- as.numeric(var.trans[2])
e <- attr(var.trans, "sc")ˆ2
add <- 4 * sire
add
dom <- 4 * (dame - sire)
dom

# Answer: The additive effect variance is 5.74, the dominance effect variance is 4.54.

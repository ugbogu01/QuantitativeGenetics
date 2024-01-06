# Question 1
# Read data
data <- read.table("Ex6_1.csv", sep = ",", header = T)
data <- data.frame(data)

# ANOVA
tr <- EMSanova(biomass ~ family, data = data, type = c("R"))
error <- tr$MS[2]
family <- (tr$MS[1] - tr$MS[2])/10
family
error

# Mixed model
summary(lmer(biomass ~ 1 + (1 | family), data))

# Heritability
h2 <- 2 * family / (family + error)
h2
# Answer: The narrow-sense heritability is 0.48.

# Question 2
# Read data
data <- read.table("Ex6_2.csv", sep = ",", header = T)
data <- data.frame(data)

# ANOVA
tr <- EMSanova(yield ~ sire + dame, data = data, type = c("R", "R"), nested = c(NA, "sire"))
error <- tr$MS[3]
dame <- (tr$MS[2] - tr$MS[3])/21
sire <- (tr$MS[1] - tr$MS[2])/105
sire
dame
error

# Mixed model
summary(lmer(yield ~ 1 + (1 | sire/dame), data = data))
# Answer: The narrow-sense heritability is 0.769.

# Question 3
# Read data
data <- read.table("Ex6_3.csv", sep = ",", header = T)
data <- data.frame(data)

# Linear model
lm <- summary(lm(offspring ~ parent, data = data))
lm
slope <- cov(data$parent, data$offspring) / var(data$parent)
slope

# Heritability
h2 <- 2 * slope
h2
# Answer: The narrow-sense heritability is 0.787.

# Question 4
# Read data
data <- read.table("Ex6_4.csv", sep = ",", header = T)
midparent <- rowMeans(data[, 1:2])
offspring <- data[, 3]
data <- data.frame(midparent, offspring)

# Linear model
lm <- summary(lm(offspring ~ midparent, data = data))
lm
slope <- cov(data$midparent, data$offspring) / var(data$midparent)
slope

# Heritability
h2 <- lm$coefficients[2]
h2
# Answer: The narrow-sense heritability is 0.592.

# Question 5
# Read data
data <- read.table("Ex6_5.csv", sep = ",", header = T)
data <- data.frame(data)

# Mixed model
varcomp <- lmer(yield ~ 1 + (1 | geno) + (1 | env) + (1 | rep) + (1 | geno:env), data = data)
summary(varcomp)

var.trans <- lme4::VarCorr(varcomp)

# Variance components
GEvar <- as.numeric(var.trans[1])
Gvar <- as.numeric(var.trans[2])
Pvar <- as.numeric(var.trans[3])
Evar <- as.numeric(var.trans[4])
evar <- attr(var.trans, "sc")ˆ2

# Residual standard deviation is stored as attribute 'sc'
e <- length(unique(data$env))
n <- length(intersect(intersect(which(data$env == 1), which(data$geno == 1)), which(data$rep == 1)))
r <- length(unique(data$rep))

# Heritability
H2 <- Gvar / (Gvar + Evar + GEvar / e + Pvar / (e * r) + evar / (e * r * n))
H2
# Answer: The broad-sense heritability is 0.167.

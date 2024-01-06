# Load data for Question 2
data <- read.table("Ex4_1.csv", sep = ",", header = T)
anovadata <- data.frame(data)

# Perform EMSANOVA analysis
tr <- EMSanova(yield ~ geno + env, data = anovadata, type = c("R", "R"))
tr

# Convert data for ANOVA analysis
anovadata <- data.frame(yield = as.numeric(data[, 1]), geno = as.factor(data[, 2]), env = as.factor(data[, 3]))

# Perform ANOVA analysis
summary(aov(yield ~ geno * env, data = anovadata))

# Calculate variance components
geno <- (tr$MS[1] - tr$MS[3])/12
geno
env <- (tr$MS[2] - tr$MS[3])/18
env

# Calculate F-values
geno.env <- (tr$MS[3] - tr$MS[4])/6
geno.env

F_geno <- tr$MS[1]/tr$MS[3]
F_geno
F_env <- tr$MS[2]/tr$MS[3]
F_env
F_geno.env <- tr$MS[3]/tr$MS[4]
F_geno.env


# Question 3
# Load data for Question 3
data <- read.table("Ex4_2.csv", sep = ",", header = T)
data <- data.frame(data)

# Perform EMSANOVA analysis with nested factors
tr <- EMSanova(yield ~ sire + dame, data = data, type = c("R", "R"), nested = c(NA, "sire"))
tr

# Calculate variance components
sire <- (tr$MS[1] - tr$MS[2])/105
sire
dame <- (tr$MS[2] - tr$MS[3])/21
dame
error <- tr$MS[3]
error

# Calculate F-values
F_sire <- tr$MS[1]/tr$MS[2]
F_sire
F_dame <- tr$MS[2]/tr$MS[3]
F_dame

# Answer: Dame nested in sire is significant at α 0.01.

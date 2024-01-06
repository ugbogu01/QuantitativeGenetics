# Load necessary libraries
library(ggplot2)

# Read data from the CSV file and create a data frame
data <- read.table("Ex3.csv", sep = ",", header = T)
data <- data.frame(data)

# Scatter plot for 'sire' group with regression line
ggplot(data = data, aes(x = soil, y = yield, col = sire, group = sire)) +
  geom_point(size = 1.2, alpha = 0.8) +        # Add points to the plot
  theme_classic() +                            # Apply a classic theme
  theme(legend.position = "none") +            # Remove legend
  scale_color_gradientn(colours = c("red", "blue")) +  # Set color gradient
  geom_smooth(method = lm, se = FALSE, size = 0.5, alpha = 0.8)  # Add regression line

# Save the plot as a TIFF file
ggsave("plot_sire.tiff")

# Scatter plot for 'dame' group with regression line
ggplot(data = data, aes(x = soil, y = yield, col = dame, group = dame)) +
  geom_point(size = 1.2, alpha = 0.8) +
  theme_classic() +
  theme(legend.position = "none") +
  scale_color_gradientn(colours = rainbow(50)) +
  geom_smooth(method = lm, se = FALSE, size = 0.5, alpha = 0.8)

# Save the plot as a TIFF file
ggsave("plot_dame.tiff")

# Load additional libraries for linear mixed models
library(lme4)
library(lmerTest)

# Linear mixed model for 'sire' group
RanIntModel <- lme4::lmer(yield ~ soil + (1 | sire), data = data)
summary(RanIntModel)

# Another linear mixed model for 'sire' group using lmerTest
RanIntModel <- lmerTest::lmer(yield ~ soil + (1 | sire), data = data)
summary(RanIntModel)

# Linear mixed model for 'dame' group with random slope only
RanSloModel_only <- lmer(yield ~ soil + (0 + soil | dame), data = data)
summary(RanSloModel_only)
ranova(RanSloModel_only)

# Linear mixed model for both 'sire' and 'dame' groups
RanSloModel_both <- lmer(yield ~ soil + (1 | sire) + (soil | dame), data = data)
summary(RanSloModel_both)
ranova(RanSloModel_both)

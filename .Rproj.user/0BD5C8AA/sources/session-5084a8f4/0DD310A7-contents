#list.files() --check 
#fastDummies
lib <- c("fastDummies","ggplot2", "pegas", "lme4", "lmerTest", "EMSaov", "qtl", "rrBLUP", "BGLR")

# Install the packages if not already installed
for (pkg in lib) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

# Load the packages
for (pkg in lib) {
  library(pkg, character.only = TRUE)
}

#Read data into R workspace
data = read.csv("Ex1.csv")

#check the dimension of the data
dimensions = dim(data)
#10000, 3

#get the number of samples
num_samples = nrow(data)
print(num_samples)
#10000

#variables in the data
str(data)
#there are three variables

#what type of variables are they?
#gender-discrete
#Height and Weight is continous

#2
#Histogram of Body weight using all samples
hist(data$Weight, main= "Histogram of weight", xlab = "Weight", col = "lightblue", border = "black")
#Add a density curve of this distribution
lines(density(data$Weight), col = "red", lwd = 2)

# Create separate histograms for male and female weights
Gender <- data$Gender
Weight <- data$Weight
df <- data.frame(Gender,Weight)

#Create a histogram seperated by gender
gender_histogram = hist(data$Weight[data$Gender == "Male"], col = "blue", main = "Histogram of Weight by Gender", 
                        xlab = "Weight", ylab = "Frequency", xlim = c(140, 200), ylim = c(0, 15), border = "black", breaks = 15,
                        text(genden_histogram$mids, genden_histogram$counts, labels= gender_histogram$counts))
#print the histogram object
print(gender_histogram)
hist(data$Weight[data$Gender == "Female"], col = "pink", add = TRUE, border = "black", breaks = 15)

# Add legend
legend("topright", legend = c("Male", "Female"), fill = c("blue", "pink"))


# Create a ggplot with the df data frame
ggplot(df, aes(x = Weight, color = Gender, fill = Gender)) +
  
  # Add a histogram layer with density scaling and transparency
  geom_histogram(aes(y = ..density..), position = "identity", alpha = 0.5, binwidth = 5) +
  
  # Add a density plot layer
  geom_density(alpha = 0.2, fill = NA) +
  
  # Manual color scale for the 'Gender' variable
  scale_color_manual(values = c("#F032E6", "#4363D8")) +
  
  # Manual fill scale for the 'Gender' variable
  scale_fill_manual(values = c("#F032E6", "#4363D8")) +
  
  # Apply a classic theme
  theme_classic() +
  
  # Customize axis text and title appearance
  theme(axis.text = element_text(size = 10, color = "black"),
        axis.title = element_text(size = 10, color = "black")) +
  
  # Set plot title and axis labels
  labs(title = "", x = "Weight", y = "Density")


# Create a multi-panel plot
par(mfrow = c(1, 2), mar = c(5, 4, 4, 2) + 0.1)


















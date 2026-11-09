# Load necessary libraries
library(ggplot2)

# Load the data
chl <- read.table("data/Processed_data/chl_corr.txt", header=TRUE, stringsAsFactors=TRUE)

# Filter data for Site D only
chl_siteD <- subset(chl, Site == 'C')

# Rename treatments to Closed, Open, and Semi-enclosed
treatment_rename <- c("Closed", "Open", "Semi-enclosed")
names(treatment_rename) <- c("C", "O", "S") # Original levels in the dataset
chl_siteD$Treatment <- factor(chl_siteD$Treatment, levels = names(treatment_rename), labels = treatment_rename)

# Reorder treatments as Open, Semi-enclosed, Closed
chl_siteD$Treatment <- factor(chl_siteD$Treatment, levels = c("Open", "Semi-enclosed", "Closed"))

# Check the levels and presence of each treatment after filtering
treatment_table <- table(chl_siteD$Treatment)
print(treatment_table)

# Check if there are at least two levels to perform ANOVA
if (length(levels(chl_siteD$Treatment)) < 2) {
  stop("Not enough levels in 'Treatment' after filtering to perform ANOVA.")
}

# Run the One-Way ANOVA
anova_results <- aov(chl ~ Treatment, data = chl_siteD)

# Display the summary of ANOVA results
summary(anova_results)

# Check assumptions
# Normality of residuals
qqnorm(residuals(anova_results))
qqline(residuals(anova_results))

# Homogeneity of variances
plot(residuals(anova_results) ~ fitted(anova_results))

# Bartlett test for equal variances
bartlett.test(chl ~ Treatment, data = chl_siteD)

# Tukey HSD test
tukey_test <- TukeyHSD(anova_results)
print(tukey_test)

# Plotting means for visual inspection
# Calculate means and standard deviations
chl_means <- aggregate(chl ~ Treatment, data = chl_siteD, FUN = mean)
chl_sd <- aggregate(chl ~ Treatment, data = chl_siteD, FUN = sd)

# Creating a bar chart with standard deviation error bars
t <- ggplot(chl_means, aes(x = Treatment, y = chl, fill = Treatment)) + 
  geom_bar(stat = "identity", position = position_dodge(), color = "black") +
  geom_errorbar(aes(ymin = chl - chl_sd$chl, ymax = chl + chl_sd$chl), width = 0.2, position = position_dodge(0.9)) +
  scale_fill_manual(values = c("#D9D9D9", "#BFBFBF", "#A6A6A6")) +
  labs(y = "Chlorophyll (μg/cm2)", x = "Treatment") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save the plot
ggsave(
  filename = "Site_D_tiles_sd.jpeg", # Updated filename for standard deviation
  plot = t, width = 10, height = 8, units = "in", dpi = 300, device = "jpeg"
)




#____________________________________________________________________________
# Load necessary libraries
library(ggplot2)

# Load the data
chl <- read.table("data/Processed_data/chl_corr.txt", header=TRUE, stringsAsFactors=TRUE)

# Filter data for Site A only
chl_siteA <- subset(chl, Site == 'A')

# Rename treatments to Closed, Open, and Semi-enclosed
treatment_rename <- c("Closed", "Open", "Semi-enclosed")
names(treatment_rename) <- c("C", "O", "S") # Original levels in the dataset
chl_siteA$Treatment <- factor(chl_siteA$Treatment, levels = names(treatment_rename), labels = treatment_rename)

# Reorder treatments as Open, Semi-enclosed, Closed
chl_siteA$Treatment <- factor(chl_siteA$Treatment, levels = c("Open", "Semi-enclosed", "Closed"))

# Check the levels and presence of each treatment after filtering
treatment_table <- table(chl_siteA$Treatment)
print(treatment_table)

# Check if there are at least two levels to perform ANOVA
if (length(levels(chl_siteA$Treatment)) < 2) {
  stop("Not enough levels in 'Treatment' after filtering to perform ANOVA.")
}

# Run the One-Way ANOVA
anova_results <- aov(chl ~ Treatment, data = chl_siteA)

# Display the summary of ANOVA results
summary(anova_results)

# Check assumptions
# Normality of residuals
qqnorm(residuals(anova_results))
qqline(residuals(anova_results))

# Homogeneity of variances
plot(residuals(anova_results) ~ fitted(anova_results))

# Bartlett test for equal variances
bartlett.test(chl ~ Treatment, data = chl_siteA)

# Tukey HSD test
tukey_test <- TukeyHSD(anova_results)
print(tukey_test)

# Plotting means for visual inspection
chl_means <- aggregate(chl ~ Treatment, data = chl_siteA, FUN = mean)
chl_se <- aggregate(chl ~ Treatment, data = chl_siteA, FUN = function(x) sd(x) / sqrt(length(x)))

# Creating a bar chart with error bars
p <- ggplot(chl_means, aes(x = Treatment, y = chl, fill = Treatment)) + 
  geom_bar(stat = "identity", position = position_dodge(), color = "black") +
  geom_errorbar(aes(ymin = chl - chl_se$chl, ymax = chl + chl_se$chl), width = 0.2, position = position_dodge(0.9)) +
  scale_fill_manual(values = c("#D9D9D9", "#BFBFBF", "#A6A6A6")) +
  labs(y = "Chlorophyll (μg/cm2)", x = "Treatment") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save the plot
ggsave(
  filename = "Site_B_tiles.jpeg",
  plot = p, width = 10, height = 8, units = "in", dpi = 300, device = "jpeg"
)



#____________________________________________________________________________
# Load necessary libraries
library(ggplot2)

# Load the data
chl <- read.table("data/Processed_data/chl_corr.txt", header=TRUE, stringsAsFactors=TRUE)

# Filter data for Site A only
chl_siteA <- subset(chl, Site == 'A')

# Rename treatments to Closed, Open, and Semi-enclosed
treatment_rename <- c("Closed", "Open", "Semi-enclosed")
names(treatment_rename) <- c("C", "O", "S") # Original levels in the dataset
chl_siteA$Treatment <- factor(chl_siteA$Treatment, levels = names(treatment_rename), labels = treatment_rename)

# Reorder treatments as Open, Semi-enclosed, Closed
chl_siteA$Treatment <- factor(chl_siteA$Treatment, levels = c("Open", "Semi-enclosed", "Closed"))

# Check the levels and presence of each treatment after filtering
treatment_table <- table(chl_siteA$Treatment)
print(treatment_table)

# Check if there are at least two levels to perform ANOVA
if (length(levels(chl_siteA$Treatment)) < 2) {
  stop("Not enough levels in 'Treatment' after filtering to perform ANOVA.")
}

# Run the One-Way ANOVA
anova_results <- aov(chl ~ Treatment, data = chl_siteA)

# Display the summary of ANOVA results
summary(anova_results)

# Check assumptions
# Normality of residuals
qqnorm(residuals(anova_results))
qqline(residuals(anova_results))

# Homogeneity of variances
plot(residuals(anova_results) ~ fitted(anova_results))

# Bartlett test for equal variances
bartlett.test(chl ~ Treatment, data = chl_siteA)

# Tukey HSD test
tukey_test <- TukeyHSD(anova_results)
print(tukey_test)

# Plotting means for visual inspection
chl_means <- aggregate(chl ~ Treatment, data = chl_siteA, FUN = mean)
chl_sd <- aggregate(chl ~ Treatment, data = chl_siteA, FUN = sd)

# Creating a bar chart with standard deviation error bars
p <- ggplot(chl_means, aes(x = Treatment, y = chl, fill = Treatment)) + 
  geom_bar(stat = "identity", position = position_dodge(), color = "black") +
  geom_errorbar(aes(ymin = chl - chl_sd$chl, ymax = chl + chl_sd$chl), width = 0.2, position = position_dodge(0.9)) +
  scale_fill_manual(values = c("#D9D9D9", "#BFBFBF", "#A6A6A6")) +
  labs(y = "Chlorophyll (μg/cm2)", x = "Treatment") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save the plot
ggsave(
  filename = "Site_B_tiles_sd.jpeg",
  plot = p, width = 10, height = 8, units = "in", dpi = 300, device = "jpeg"
)


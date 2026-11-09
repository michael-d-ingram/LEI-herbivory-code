# Load essential libraries
library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(ggplot2)

# Set working directory and load data
setwd("/Users/quackers/Library/CloudStorage/OneDrive-SouthernCrossUniversity/R directories/d15N_coral")
d15N_coral <- read.table("data/Processed_data/15N_coral_linearmodel.txt", header = TRUE, stringsAsFactors = TRUE)

# Format factors
d15N_coral$Season <- as.factor(d15N_coral$Season)
d15N_coral$Year <- as.factor(d15N_coral$Year)

# Visualize raw data
ggplot(d15N_coral, aes(x = Season, y = d15N, color = Year, group = Year)) +
  geom_point(position = position_jitter(width = 0.1), size = 2) +
  geom_line(stat = "summary", fun = mean, size = 1) +
  theme_minimal() +
  labs(title = "δ¹⁵N by Season and Year", y = expression(delta^15*N~"\u2030"))


# Fit linear mixed model with Season as fixed and Year as random
model_mixed <- lmer(d15N ~ Season + (1 | Year), data = d15N_coral)

#checking what data was included
used_data <- model.frame(model_mixed)
nrow(used_data)  # Number of observations used
head(used_data)  # Preview the data

#check for normality
# Extract residuals and fitted values
resid_vals <- resid(model_mixed)
fitted_vals <- fitted(model_mixed)

# 1. QQ plot: normality of residuals
qqnorm(resid_vals)
qqline(resid_vals)

# 2. Histogram of residuals
hist(resid_vals, main = "Histogram of Residuals", xlab = "Residuals")

# 3. Residuals vs. fitted: check for homoscedasticity
plot(fitted_vals, resid_vals, main = "Residuals vs Fitted", 
     xlab = "Fitted values", ylab = "Residuals")
abline(h = 0, lty = 2)

# 4. Shapiro-Wilk test (optional for residuals)
shapiro.test(resid_vals)


# Type II ANOVA to test overall effect of Season
Anova(model_mixed, type = "II")

# Optional: pairwise comparisons between all seasons
emmeans(model_mixed, pairwise ~ Season)

# Optional: plot estimated marginal means
plot(emmeans(model_mixed, ~ Season))

#-----------------NOW CHECK FOR YEARLY DIFFERENCES_____________

# Treat Year as fixed and test if d15N varies across years
year_model <- aov(d15N ~ Year, data = d15N_coral)
summary(year_model)

# Optional: post-hoc test to see where the differences lie
TukeyHSD(year_model)

# Check assumptions visually
par(mfrow = c(2, 2))
plot(year_model)



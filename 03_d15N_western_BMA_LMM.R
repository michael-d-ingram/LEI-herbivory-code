# Load essential libraries
install.packages("car")
library(car)
library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(ggplot2)

# Set working directory and load data
setwd("/Users/quackers/Library/CloudStorage/OneDrive-SouthernCrossUniversity/R directories/d15N_coral")
d15N_coral <- read.table("data/Processed_data/15N_BMA_linearmodel.txt", header = TRUE, stringsAsFactors = TRUE)

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


#PLot charts_________________________________________
# Load necessary packages
library(ggplot2)
library(dplyr)

# Clean and recode season labels - FIXED
d15N_coral <- d15N_coral %>%
  filter(!is.na(d15N), !is.na(Season), !is.na(Year)) %>%
  mutate(
    Season = case_when(
      Season == "F" ~ "February",
      Season == "J" ~ "July",
      Season == "O" ~ "October",
      TRUE ~ as.character(Season)
    ),
    Season = factor(Season, levels = c("February", "July", "October"))
  )

# Summarise data
summary_data <- d15N_coral %>%
  group_by(Season, Year) %>%
  summarise(
    mean_d15N = mean(d15N, na.rm = TRUE),
    se_d15N = sd(d15N, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  filter(mean_d15N - se_d15N >= 4, mean_d15N + se_d15N <= 12)

# Create plot
d15N_plot <- ggplot(summary_data, aes(x = Season, y = mean_d15N, color = factor(Year), shape = factor(Year))) +
  geom_point(position = position_dodge(width = 0.3), size = 3) +
  geom_errorbar(aes(ymin = mean_d15N - se_d15N, ymax = mean_d15N + se_d15N),
                width = 0.1, position = position_dodge(width = 0.3)) +
  scale_y_continuous(limits = c(4, 12), expand = c(0, 0)) +
  theme_classic(base_size = 14, base_family = "Times") +
  labs(
    y = expression(BMA~delta^{15}*N~("\u2030")),
    x = "Month",
    color = "Year",
    shape = "Year"  # Adds shape to legend
  ) +
  theme(
    axis.line = element_line(color = "black", linewidth = 0.6),
    axis.ticks = element_line(color = "black"),
    axis.text = element_text(color = "black"),
    axis.title.x = element_text(size = 12),
    legend.position = "right",
    panel.grid = element_blank(),
    plot.title = element_blank()
  )

# Save plot to working directory
ggsave("d15N_seasonal_BMA_means_plot.png", plot = d15N_plot, width = 6, height = 4.5, dpi = 300)

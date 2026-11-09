# BMA version of 01_d15N_reef_flats_LMM.R.
# Identical to that script except that it reads the BMA site file.
# Created for deposit; the original analysis was run by editing the read.table line by hand.

# Load essential libraries
library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(ggplot2)
library(car)

# Set working directory and load your data
setwd("/Users/quackers/Library/CloudStorage/OneDrive-SouthernCrossUniversity/R directories/Linear Model")
d15N_BMA <- read.table("Data/15N_BMA_site_linearmodel.txt", 
                       header = TRUE, stringsAsFactors = TRUE)


# Convert relevant columns to factors
d15N_BMA$Season <- as.factor(d15N_BMA$Season)
d15N_BMA$Site <- as.factor(d15N_BMA$Site)
d15N_BMA$Year <- as.factor(d15N_BMA$Year)

# Quick structure and summary check
str(d15N_BMA)
summary(d15N_BMA)

# Visualise raw data
ggplot(d15N_BMA, aes(x = Season, y = d15N, color = Site, group = Site)) +
  geom_point(position = position_jitter(width = 0.1), size = 2) +
  geom_line(stat = "summary", fun = mean, size = 1) +
  theme_minimal() +
  labs(title = expression("Raw " * delta^15 * "N by Season and Site"), 
       y = expression(delta^15*N~"\u2030"))

# -------------------------------
# Linear mixed-effects model
# -------------------------------
# Fixed effects: Season and Site
# Random effect: Year
model_mixed <- lmer(d15N ~ Season + Site + (1 | Year), data = d15N_BMA)

# Model diagnostics
resid_vals <- resid(model_mixed)
fitted_vals <- fitted(model_mixed)

# QQ plot
qqnorm(resid_vals)
qqline(resid_vals)

# Histogram
hist(resid_vals, main = "Histogram of Residuals", xlab = "Residuals")

# Residuals vs fitted
plot(fitted_vals, resid_vals, main = "Residuals vs Fitted", 
     xlab = "Fitted values", ylab = "Residuals")
abline(h = 0, lty = 2)

# Shapiro-Wilk test for normality
cat("\nShapiro-Wilk normality test:\n")
print(shapiro.test(resid_vals))

# Type II ANOVA for fixed effects
cat("\nType II ANOVA results:\n")
anova_results <- Anova(model_mixed, type = "II")
print(anova_results)

# Estimated marginal means and pairwise comparisons
cat("\nEstimated Marginal Means - Season:\n")
emm_season <- emmeans(model_mixed, pairwise ~ Season)
print(emm_season)

cat("\nEstimated Marginal Means - Site:\n")
emm_site <- emmeans(model_mixed, pairwise ~ Site)
print(emm_site)

# Plot estimated marginal means
plot(emmeans(model_mixed, ~ Season), comparisons = TRUE)
plot(emmeans(model_mixed, ~ Site), comparisons = TRUE)

# -------------------------------
# Optional: Test for Year differences separately
# -------------------------------
cat("\nOne-way ANOVA testing Year as fixed effect:\n")
year_model <- aov(d15N ~ Year, data = d15N_BMA)
print(summary(year_model))

cat("\nTukey HSD post-hoc for Year:\n")
print(TukeyHSD(year_model))

# Assumption check for the year model
par(mfrow = c(2, 2))
plot(year_model)
par(mfrow = c(1, 1))  # Reset plot layout


# Group quadrants A, B, and C together and keep D separate
d15N_BMA$QuadrantGroup <- ifelse(d15N_BMA$Site %in% c("A", "B", "C"), "ABC", "D")

# Calculate raw mean and n for each group
group_means <- d15N_BMA %>%
  group_by(QuadrantGroup) %>%
  summarise(
    mean_d15N = mean(d15N, na.rm = TRUE),
    se_d15N = sd(d15N, na.rm = TRUE) / sqrt(n()),
    n = n()
  )

# View results
print(group_means)



#_________MAKE BAR CHART____________________________________________________
# Load libraries
library(ggplot2)
library(emmeans)

# Step 1: Get emmeans by Season and Site (Site as quadrants)
emm_df <- emmeans(model_mixed, ~ Season * Site) |>
  as.data.frame()

# Step 2: Recode Season to full month names
emm_df$Season <- factor(emm_df$Season,
                        levels = c("F", "J", "O"),
                        labels = c("February", "June", "October"))

# Step 3: Define a reversed greyscale palette for quadrants
cbPalette <- c("A" = "#CCCCCC", "B" = "#999999", "C" = "#666666", "D" = "#333333")

# Step 4: Create a professional theme with larger axis labels
bar_chart_theme <- theme_minimal(base_family = "Times") +
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black", size = 0.5),
    axis.ticks = element_line(colour = "black", size = 0.5),
    axis.text.x = element_text(size = 13, colour = "black"),  # Enlarged axis labels
    axis.text.y = element_text(size = 13, colour = "black"),
    axis.title.y = element_text(size = 14, colour = "black"),
    axis.title.x = element_blank(),  # Remove x-axis title
    legend.key = element_rect(color = "black", size = 0.3),   # Thinner outline
    legend.text = element_text(size = 11, family = "Times", colour = "black"),
    legend.title = element_text(size = 12, family = "Times", colour = "black")
  )

# Step 5: Create the plot
ggplot(emm_df, aes(x = Season, y = emmean, fill = Site)) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.8),
           color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE),
                position = position_dodge(width = 0.8),
                width = 0.2) +
  scale_fill_manual(values = cbPalette, name = "Quadrant") +
  scale_y_continuous(
    limits = c(0, 10),
    breaks = seq(0, 10, 2.5),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(y = expression(delta^15*N~"(\u2030)")) +
  bar_chart_theme


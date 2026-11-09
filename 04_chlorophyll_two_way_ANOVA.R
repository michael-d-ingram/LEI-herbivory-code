# 1. Load required packages
library(tidyverse)

# 2. Set working directory as needed (optional)
# setwd("/path/to/your/directory")

# 3. Read data
chl <- read.table("data/Processed_data/chl_final.txt", 
                  header = TRUE, stringsAsFactors = TRUE)

# 4. Ensure Treatment has the correct factor levels
chl$Treatment <- factor(chl$Treatment, levels = c("C", "O"))

# Re-factor Site so that "C" and "A" are recognized in the order you want
chl$Site <- factor(chl$Site, levels = c("C","A"))

# -------------------------------------------------------------------------
# NEW PART: Two-way ANOVA and Tukey's HSD
# -------------------------------------------------------------------------
# Fit a two-way ANOVA model with interaction
anova_model <- aov(chl ~ Site * Treatment, data = chl)
summary(anova_model)

# Perform Tukey's HSD post-hoc test
tukey_results <- TukeyHSD(anova_model)
tukey_results
# -------------------------------------------------------------------------

# 5. Summarize data: mean + standard error
chl_plot <- chl %>%
  group_by(Site, Treatment) %>%
  summarise(
    chl_mean = mean(chl, na.rm = TRUE),
    chl_se   = sd(chl, na.rm = TRUE) / sqrt(n()),
    .groups  = "drop"
  )

# 6. (Re)factor Treatment if needed
chl_plot$Treatment <- factor(chl_plot$Treatment, levels = c("C", "O"))

# 7. Create the bar chart
cbPalette <- c("grey52", "grey77")
plot <- ggplot(chl_plot, aes(fill = Treatment, y = chl_mean, x = Site)) +  
  geom_bar(position = "dodge", stat = "identity", color = "black") +  
  scale_fill_manual(values = cbPalette, labels = c("Exposed", "Caged")) +  
  geom_errorbar(
    aes(ymin = chl_mean - chl_se, ymax = chl_mean + chl_se),
    width = 0.2,
    position = position_dodge(0.9)
  ) +  
  labs(
    y = expression(paste("Chlorophyll (", mu, "g/cm"^2, ")")),
    x = "Site",
    fill = "Treatment"
  ) +
  # Match x labels to the factor levels above
  scale_x_discrete(labels = c("C" = "D", "A" = "B")) +
  scale_y_continuous(
    limits = c(0, 30),
    expand = expansion(mult = c(0, 0)),
    breaks = seq(0, 30, 5)
  ) + 
  theme(
    panel.background = element_rect(fill = "transparent"),
    axis.line        = element_line(colour = "black", size = 0.5, linetype = "solid"),
    axis.ticks       = element_line(colour = "black", size = 0.5, linetype = "solid"),
    axis.text.x      = element_text(size = 12, colour = "black", family = "Times"),
    axis.text.y      = element_text(size = 12, colour = "black", family = "Times"),
    text             = element_text(size = 12, colour = "black", family = "Times"),
    legend.background= element_rect(fill = "transparent"),
    legend.text      = element_text(size = 10,  colour = "black", family = "Times"),
    legend.key       = element_rect(color = NA, fill = NA),
    legend.key.size  = unit(1, "cm")
  )

# 8. Print and save the plot
print(plot)
ggsave("Open_closed.jpeg", plot, width = 10, height = 8, units = "in", dpi = 300)

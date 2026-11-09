# Three-treatment (orthogonal) two-way ANOVA: the cage-control model.
# Source of every contrast involving the semi-caged treatment.
# Original: Tiles/Scripts, "Two-way ANOVA (orthogonal) script", M. Ingram, 19/09/2023.
# Paths made relative to the repository root for deposit.
library(car)

chl <- read.table("data/03_chlorophyll_tiles/chl_corr.txt", header = TRUE, stringsAsFactors = TRUE)
chl$Treatment <- factor(chl$Treatment, levels = c('O', 'C', 'S'))

a2_int_fixed <- aov(chl ~ Site + Treatment + Site:Treatment, data = chl)
summary(a2_int_fixed)

shapiro.test(residuals(a2_int_fixed))
bartlett.test(chl ~ interaction(Site, Treatment), data = chl)

TukeyHSD(a2_int_fixed)

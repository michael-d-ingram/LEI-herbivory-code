# -----------------------------------------------------------------------------
# Herbivore-exclusion chlorophyll: single three-treatment model, and Figure 6.
#
# One model for everything. Treatment has three levels (caged, semi-caged,
# exposed) crossed with reef flat, n = 3 per cell, 18 tiles. Replaces the earlier
# split analysis in which caged-versus-exposed came from a two-treatment model
# (error df 8) and the cage-control contrasts from a three-treatment model
# (error df 12), which gave two different p-values for the same comparison.
#
# Site C = western reef flat (formerly quadrant D)
# Site A = eastern reef flat (formerly quadrant B)
# Treatment: C = caged, S = semi-caged (cage control), O = exposed
# -----------------------------------------------------------------------------
library(ggplot2)
library(dplyr)

chl <- read.table("data/03_chlorophyll_tiles/chl_corr.txt",
                  header = TRUE, stringsAsFactors = TRUE)
chl$Treatment <- factor(chl$Treatment, levels = c("C", "S", "O"))
chl$Site      <- factor(chl$Site,      levels = c("C", "A"))

# ---- model -------------------------------------------------------------------
m <- aov(chl ~ Site * Treatment, data = chl)
cat("\n=== Two-way ANOVA (Site x Treatment, three treatments) ===\n")
print(summary(m))

cat("\n=== Assumptions ===\n")
print(shapiro.test(residuals(m)))
print(bartlett.test(chl ~ interaction(Site, Treatment), data = chl))

cat("\n=== Tukey HSD, all six site-by-treatment cells ===\n")
tk  <- TukeyHSD(m)$`Site:Treatment`
mse <- summary(m)[[1]]["Residuals", "Mean Sq"]
dfe <- summary(m)[[1]]["Residuals", "Df"]
se  <- sqrt(2 * mse / 3)
out <- data.frame(contrast = rownames(tk),
                  diff = round(tk[, "diff"], 3),
                  t    = round(tk[, "diff"] / se, 3),
                  df   = dfe,
                  p    = round(tk[, "p adj"], 4))
print(out, row.names = FALSE)
cat(sprintf("\nMSE = %.4f, SE of a difference = %.4f, error df = %d\n", mse, se, dfe))

# ---- cell summaries ----------------------------------------------------------
cells <- chl %>%
  group_by(Site, Treatment) %>%
  summarise(mean = mean(chl), sd = sd(chl), se = sd(chl) / sqrt(n()),
            n = n(), .groups = "drop")
cat("\n=== Cell means (text reports SD; figure plots SE) ===\n")
print(as.data.frame(cells), row.names = FALSE)

# ---- figure ------------------------------------------------------------------
# Compact letter display from the Tukey results: the western caged mean differs
# from every other cell except the western semi-caged mean, which therefore
# shares both groups. Means sharing a symbol do not differ (Tukey, p < 0.05).
cells$cld <- c("**", "*\n**", "*", "*", "*", "*")[
  match(paste(cells$Site, cells$Treatment), c("C C", "C S", "C O", "A C", "A S", "A O"))]
DROP_OFF <- 0.085   # drop lines sit clear of the centred group symbols

pal <- c("C" = "#4A4A4A", "S" = "#8C8C8C", "O" = "#DCDCDC")
LW  <- 0.25
xpos <- c("C C" = 1 - 0.2667, "C S" = 1, "C O" = 1 + 0.2667,
          "A C" = 2 - 0.2667, "A S" = 2, "A O" = 2 + 0.2667)
top  <- setNames(cells$mean + cells$se, paste(cells$Site, cells$Treatment))

seg <- function(x, xe, y, ye) annotate("segment", x = x, xend = xe, y = y, yend = ye,
                                       linewidth = LW, colour = "black")
brk <- function(a, b, y, drop, lab, lab_x, lab_y) list(
  seg(xpos[a], xpos[b] + DROP_OFF, y, y),
  seg(xpos[a], xpos[a], y, y - 0.30),
  seg(xpos[b] + DROP_OFF, xpos[b] + DROP_OFF, y, drop),
  annotate("text", x = lab_x, y = lab_y, label = lab, parse = TRUE,
           size = 2.75, family = "serif"))

fig <- ggplot(cells, aes(x = Site, y = mean, fill = Treatment)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.72,
           colour = "black", linewidth = LW) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.11,
                position = position_dodge(width = 0.8), linewidth = LW) +
  geom_text(data = transform(cells,
              xx = xpos[paste(Site, Treatment)],
              yy = mean + se + 0.45),
            aes(x = xx, y = yy, label = cld), inherit.aes = FALSE,
            vjust = 0, lineheight = 0.70, size = 2.95, family = "serif") +
  brk("C C", "C O", 28.95, top[["C O"]] + 1.05, "italic(p)==0.024", 1.06, 28.30) +
  brk("C C", "A C", 29.35, top[["A C"]] + 1.05, "italic(p)==0.011", 1.53, 28.30) +
  brk("C C", "A O", 29.75, top[["A O"]] + 1.05, "italic(p)==0.002", 2.13, 28.30) +
  brk("C S", "C O", 15.40, top[["C O"]] + 1.05, "italic(p)==0.94", 1.133, 15.82) +
  brk("A S", "A O", 10.60, top[["A O"]] + 1.05, "italic(p)==0.81", 2.133, 11.02) +
  scale_fill_manual(values = pal, breaks = c("C", "S", "O"),
                    labels = c("Caged", "Semi-caged", "Exposed"), name = NULL) +
  guides(fill = guide_legend(byrow = TRUE)) +
  scale_x_discrete(labels = c("C" = "Western reef flat", "A" = "Eastern reef flat")) +
  scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, 5),
                     expand = expansion(mult = c(0, 0))) +
  labs(x = NULL, y = expression("Total chlorophyll (" * mu * "g cm"^-2 * ")")) +
  theme_classic(base_size = 10, base_family = "serif") +
  theme(legend.position = "right",
        legend.justification = c(0, 0.5),
        legend.key = element_rect(colour = "black", linewidth = LW),
        legend.key.size = unit(0.34, "cm"),
        legend.spacing.y = unit(0.09, "cm"),
        legend.text = element_text(size = 8, colour = "black"),
        legend.margin = margin(0, 0, 0, 1),
        axis.line = element_line(colour = "black", linewidth = LW),
        axis.ticks = element_line(colour = "black", linewidth = LW),
        axis.ticks.length = unit(0.10, "cm"),
        axis.text = element_text(colour = "black", size = 8.5),
        axis.title.y = element_text(size = 9.5, margin = margin(r = 4)),
        plot.margin = margin(5, 3, 3, 3),
        panel.background = element_rect(fill = "white", colour = NA),
        plot.background = element_rect(fill = "white", colour = NA))

ggsave("outputs/Fig6_chlorophyll.png",  fig, width = 6.6, height = 5.0, dpi = 300)
ggsave("outputs/Fig6_chlorophyll.tiff", fig, width = 6.6, height = 5.0, dpi = 300,
       compression = "lzw")
cat("\nFigure written to outputs/Fig6_chlorophyll.png and .tiff\n")

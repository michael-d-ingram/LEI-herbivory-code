suppressMessages({library(lme4); library(lmerTest); library(car); library(emmeans)})
options(width = 130)

W  <- "/mnt/user-data/uploads/R directories/d15N_coral/data/Processed_data/"
S  <- "/root/.claude/uploads/291b043d-d7b4-507e-b833-a97e1646682d/"

hdr <- function(x) cat("\n", strrep("=", 78), "\n", x, "\n", strrep("=", 78), "\n", sep = "")

## ---------- site-level models (216 rows), exactly as Linear Model.R ----------
site <- function(f, tag) {
  d <- read.table(f, header = TRUE, stringsAsFactors = TRUE)
  d$Season <- factor(d$Season); d$Site <- factor(d$Site); d$Year <- factor(d$Year)
  hdr(paste("SITE MODEL:", tag, " n =", nrow(d)))
  m <- lmer(d15N ~ Season + Site + (1 | Year), data = d)
  print(shapiro.test(resid(m)))
  print(Anova(m, type = "II"))
  print(emmeans(m, pairwise ~ Site)$contrasts)
}
site(paste0(S, "36975328-15N_coral_site_linearmodel.txt"), "CORAL")
site(paste0(S, "fe074b3d-15N_BMA_site_linearmodel.txt"),   "BMA")

## ---------- western models, exactly as the 2021-2024 scripts ----------
west <- function(f, tag, drop_dupe) {
  d <- read.table(f, header = TRUE, stringsAsFactors = TRUE)
  d$Season <- factor(d$Season); d$Year <- factor(d$Year)
  if (drop_dupe) d <- d[!(d$Season == "F" & d$Year == 2021), ]
  m <- lmer(d15N ~ Season + (1 | Year), data = d)
  hdr(paste("WESTERN:", tag, if (drop_dupe) "[Feb-2021 duplicate REMOVED]" else "[AS PUBLISHED]",
            " n used =", nrow(model.frame(m))))
  print(shapiro.test(resid(m)))
  print(Anova(m, type = "II"))
  print(emmeans(m, pairwise ~ Season))
  ym <- aov(d15N ~ Year, data = d)
  print(summary(ym)); print(TukeyHSD(ym))
}
for (dd in c(FALSE, TRUE)) {
  west(paste0(W, "15N_coral_linearmodel.txt"), "CORAL", dd)
  west(paste0(W, "15N_BMA_linearmodel.txt"),   "BMA",   dd)
}
cat("\n--- sessionInfo ---\n")
for (p in c("lme4","lmerTest","car","emmeans"))
  cat(sprintf("%-9s %s\n", p, as.character(packageVersion(p))))
cat("R", as.character(getRversion()), "\n")

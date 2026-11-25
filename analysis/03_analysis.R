# ============================================================
# 03_analysis.R - answers the ten research questions (A1-A10, Price topic)
# Input : dataset.csv, interim/D_decisions.csv (from 02_descriptive.R)
# Output: ../report/tables/A*_table.tex (+ checks), S_table.tex
#         ../report/figures/A*_figure.pdf (+ diagnostics)
# Packages: ggplot2, sandwich, lmtest.  Run from the "analysis" folder.
# alpha = .05 throughout. Hypotheses were fixed BEFORE looking at the data
# (they are written at the top of each block). Nothing here is exploratory
# unless it is labelled so.
# ============================================================
if (!file.exists("interim/helpers.R") && file.exists("analysis/interim/helpers.R")) setwd("analysis")
source("interim/helpers.R")
setup_dirs()
for (pk in c("sandwich", "lmtest"))
  if (!requireNamespace(pk, quietly = TRUE)) stop("Please run: install.packages('", pk, "')")

d <- load_data(); N <- nrow(d)
cat("Cafes in dataset.csv:", N, "\n")

# ---- parametric / non-parametric decisions come from the descriptive table ----
dec <- read.csv("interim/D_decisions.csv", stringsAsFactors = FALSE)
get_dec <- function(variable, groups, col) {
  r <- dec[dec$variable == variable & dec$group %in% groups, ]
  if (nrow(r) != length(groups)) stop("Decision missing in D_decisions.csv for: ", variable)
  all(r[[col]])
}
is_param  <- function(v, g) get_dec(v, g, "parametric")
is_normal <- function(v, g) get_dec(v, g, "normal")
meth <- function(flag) if (flag) "parametric" else "non-parametric"

# ---- summary table of all analyses ----
SUM <- data.frame()
add_sum <- function(id, method, estimate, ci, effect, test, p, n, lost)
  SUM <<- rbind(SUM, data.frame(Analysis = id, Method = method, Estimate = estimate, `95% CI` = ci,
                                `Effect size` = effect, Test = test, p = p, n = n, Lost = lost,
                                check.names = FALSE, stringsAsFactors = FALSE))
sh <- function(x) { print(x, row.names = FALSE); invisible(x) }
inf_cooks <- function(m) { cd <- cooks.distance(m); which(cd > 4 / length(cd)) }

# ============================================================
# A1  One sample: is the mean espresso price different from 3 USD?
# H0: mean espresso price = 3 USD (in CZK)   H1: it is different (two-tailed)
# ============================================================
cat("\n\n=========== A1 ===========\n")
usd_czk <- 21.76     # CNB rate, 3 November 2026 (21.762 CZK per USD); state this in the report
bench   <- 3 * usd_czk
cat("Benchmark: 3 USD x", usd_czk, "=", bench, "CZK\n")
x <- d$espresso[!is.na(d$espresso)]; n1 <- length(x)
par1 <- is_param(VN$esp, G_ALL); nor1 <- is_normal(VN$esp, G_ALL)
if (par1) {
  tt <- t.test(x, mu = bench)
  ci1 <- if (nor1) tt$conf.int else boot_ci(x)        # bootstrap CI if normality is doubtful (CLT case)
  est1 <- mean(x); p1 <- tt$p.value
  stat1 <- paste0("$t(", round(tt$parameter), ")=", f2(tt$statistic), "$")
  es1 <- paste0("$d=", f2((mean(x) - bench) / sd(x)), "$")
  m1 <- paste0("One-sample t-test", if (!nor1) " (bootstrap CI)" else "")
} else {
  wt <- wilcox.test(x, mu = bench, conf.int = TRUE, exact = FALSE)
  est1 <- unname(wt$estimate); ci1 <- wt$conf.int; p1 <- wt$p.value
  stat1 <- paste0("$V=", f2(wt$statistic), "$"); es1 <- "--"; m1 <- "Wilcoxon signed-rank (pseudo-median)"
}
A1 <- data.frame(Estimate = paste(f1(est1), "CZK"), `95% CI` = ci_str(ci1[1], ci1[2], f1),
                 Benchmark = paste(f1(bench), "CZK"), `Effect size` = es1, Test = stat1,
                 p = pfmt(p1), n = n1, check.names = FALSE)
sh(A1); write_tex(A1, "A1_table")
write_tex(data.frame(Check = c("Method chosen", "Cafes lost to missing espresso price"),
                     Result = c(m1, N - n1)), "A1_checks")
fig <- ggplot(data.frame(x = x), aes(x)) + geom_histogram(bins = 20, fill = "grey75", colour = "white") +
  annotate("rect", xmin = ci1[1], xmax = ci1[2], ymin = -Inf, ymax = Inf, alpha = .25, fill = "blue") +
  geom_vline(xintercept = est1, colour = "blue") +
  geom_vline(xintercept = bench, colour = "red", linetype = "dashed") +
  labs(x = "Espresso price (CZK)", y = "Number of cafes")
save_fig(fig, "A1_figure")
add_sum("A1", m1, paste(f1(est1), "CZK"), ci_str(ci1[1], ci1[2], f1), es1, stat1, pfmt(p1), n1, N - n1)

# ============================================================
# A2  Two groups: do roasteries charge more for an espresso?
# H0: mean espresso price equal in roasteries and other cafes
# H1: roasteries charge more (one-tailed; difference = roastery minus other)
# ============================================================
cat("\n\n=========== A2 ===========\n")
d2 <- d[!is.na(d$espresso) & !is.na(d$roastery_f), ]
x <- d2$espresso; g <- d2$roastery_f             # first level = Roastery, so "greater" = roastery > other
x1 <- x[g == "Roastery"]; x0 <- x[g == "Other cafes"]
par2 <- is_param(VN$esp, c("Roastery", "Other cafes")); lev2 <- levene_p(x, g)
if (par2) {
  t1 <- t.test(x ~ g, alternative = "greater"); t2 <- t.test(x ~ g)    # Welch
  est2 <- unname(t2$estimate[1] - t2$estimate[2]); ci2 <- t2$conf.int; p2 <- t1$p.value
  stat2 <- paste0("$t(", f2(t2$parameter), ")=", f2(t2$statistic), "$")
  es2 <- paste0("$d=", f2(cohen_d(x1, x0)), "$"); m2 <- "Welch t-test"
} else {
  w1 <- wilcox.test(x ~ g, alternative = "greater", conf.int = TRUE, exact = FALSE)
  w2 <- wilcox.test(x ~ g, conf.int = TRUE, exact = FALSE)
  est2 <- unname(w2$estimate); ci2 <- w2$conf.int; p2 <- w1$p.value
  stat2 <- paste0("$W=", f2(w1$statistic), "$"); es2 <- "--"; m2 <- "Wilcoxon rank-sum (Hodges-Lehmann shift)"
}
A2 <- data.frame(Estimate = paste(f1(est2), "CZK"), `95% CI` = ci_str(ci2[1], ci2[2], f1),
                 `Effect size` = es2, Test = stat2, p = paste(pfmt(p2), "(one-tailed)"),
                 `n roastery / other` = paste(length(x1), "/", length(x0)), check.names = FALSE)
sh(A2); write_tex(A2, "A2_table")
write_tex(data.frame(Check = c("Method chosen", "Levene test, equal variances", "Cafes lost to missing values"),
  Result = c(m2, paste0("p = ", pplain(lev2), " (Welch test used regardless)"), N - nrow(d2))), "A2_checks")
gc <- group_ci(x, g)
fig <- ggplot(d2, aes(roastery_f, espresso)) + geom_jitter(width = .12, alpha = .4, colour = "grey30") +
  geom_pointrange(data = gc, aes(group, mean, ymin = lo, ymax = hi), colour = "red", inherit.aes = FALSE) +
  labs(x = NULL, y = "Espresso price (CZK)")
save_fig(fig, "A2_figure")
add_sum("A2", m2, paste(f1(est2), "CZK"), ci_str(ci2[1], ci2[2], f1), es2, stat2,
        pfmt(p2), nrow(d2), N - nrow(d2))

# ============================================================
# A3  Paired: how much more does a flat white cost than a cappuccino?
# H0: mean (flat white - cappuccino) = 0   H1: > 0 (one-tailed, "more")
# ============================================================
cat("\n\n=========== A3 ===========\n")
d3 <- d[!is.na(d$fw_minus_cap), ]; x <- d3$fw_minus_cap; n3 <- length(x)
par3 <- is_param(VN$diff, G_DIFF)
if (par3) {
  t1 <- t.test(x, alternative = "greater"); t2 <- t.test(x)
  est3 <- mean(x); ci3 <- t2$conf.int; p3 <- t1$p.value
  stat3 <- paste0("$t(", round(t2$parameter), ")=", f2(t2$statistic), "$")
  es3 <- paste0("$d_z=", f2(mean(x) / sd(x)), "$"); m3 <- "Paired t-test"
} else {
  w1 <- wilcox.test(x, alternative = "greater", exact = FALSE)
  w2 <- wilcox.test(x, conf.int = TRUE, exact = FALSE)
  est3 <- unname(w2$estimate); ci3 <- w2$conf.int; p3 <- w1$p.value
  stat3 <- paste0("$V=", f2(w1$statistic), "$"); es3 <- "--"; m3 <- "Wilcoxon signed-rank (pseudo-median)"
}
A3 <- data.frame(Estimate = paste(f1(est3), "CZK"), `95% CI` = ci_str(ci3[1], ci3[2], f1),
                 `Effect size` = es3, Test = stat3, p = paste(pfmt(p3), "(one-tailed)"), n = n3,
                 check.names = FALSE)
sh(A3); write_tex(A3, "A3_table")
write_tex(data.frame(Check = c("Method chosen", "Cafes with identical flat white and cappuccino price",
                               "Cafes lost to missing values"),
                     Result = c(m3, sum(x == 0), N - n3)), "A3_checks")
fig <- ggplot(data.frame(x = x), aes(x)) + geom_histogram(binwidth = 5, fill = "grey75", colour = "white") +
  annotate("rect", xmin = ci3[1], xmax = ci3[2], ymin = -Inf, ymax = Inf, alpha = .25, fill = "blue") +
  geom_vline(xintercept = est3, colour = "blue") + geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "Flat white minus cappuccino (CZK)", y = "Number of cafes")
save_fig(fig, "A3_figure")
add_sum("A3", m3, paste(f1(est3), "CZK"), ci_str(ci3[1], ci3[2], f1), es3, stat3, pfmt(p3), n3, N - n3)

# ============================================================
# A4  More groups: does the cappuccino price differ by cafe type?
# H0: mean cappuccino price equal in all three types   H1: at least one differs (two-tailed)
# ============================================================
cat("\n\n=========== A4 ===========\n")
d4 <- d[!is.na(d$cappuccino) & !is.na(d$cafe_type_f), ]; d4$g <- droplevels(d4$cafe_type_f)
x <- d4$cappuccino; g <- d4$g
par4 <- is_param(VN$cap, levels(g)); lev4 <- levene_p(x, g)
gc <- group_ci(x, g)
if (par4) {
  wo <- oneway.test(x ~ g, var.equal = FALSE)            # Welch F
  stat4 <- paste0("$F(", round(wo$parameter[1]), ", ", f2(wo$parameter[2]), ")=", f2(wo$statistic), "$")
  p4 <- wo$p.value; m4 <- "Welch ANOVA + Games-Howell"
  ph <- games_howell(x, g)
  PH <- data.frame(Pair = ph$pair, `Difference (CZK)` = f1(ph$diff), `95% CI` = ci_str(ph$lo, ph$hi, f1),
                   `Adj. p` = pplain(ph$p_adj), check.names = FALSE)
} else {
  kw <- kruskal.test(x ~ g)
  stat4 <- paste0("$H(", kw$parameter, ")=", f2(kw$statistic), "$"); p4 <- kw$p.value
  m4 <- "Kruskal-Wallis + Dunn (Holm)"
  ph <- dunn_holm(x, g)
  PH <- data.frame(Pair = ph$pair, `Median difference (CZK)` = f1(ph$diff_median),
                   `Adj. p` = pplain(ph$p_adj), check.names = FALSE)
}
A4 <- rbind(data.frame(Group = gc$group, n = as.character(gc$n), `Mean (CZK)` = f1(gc$mean),
                       `95% CI` = ci_str(gc$lo, gc$hi, f1), Test = "", p = "", check.names = FALSE),
            data.frame(Group = "Omnibus test", n = as.character(nrow(d4)), `Mean (CZK)` = "",
                       `95% CI` = "", Test = stat4, p = pfmt(p4), check.names = FALSE))
sh(A4); write_tex(A4, "A4_table"); cat("\nPost hoc:\n"); sh(PH); write_tex(PH, "A4_posthoc")
write_tex(data.frame(Check = c("Method chosen", "Levene test, equal variances", "Smallest group size",
                               "Cafes lost to missing values"),
                     Result = c(m4, paste0("p = ", pplain(lev4), " (Welch/rank test does not assume equal variances)"),
                                min(table(g)), N - nrow(d4))), "A4_checks")
fig <- ggplot(d4, aes(g, cappuccino)) + geom_jitter(width = .12, alpha = .4, colour = "grey30") +
  geom_pointrange(data = gc, aes(group, mean, ymin = lo, ymax = hi), colour = "red", inherit.aes = FALSE) +
  labs(x = "Cafe type", y = "Cappuccino price (CZK)")
save_fig(fig, "A4_figure")
add_sum("A4", m4, paste0("Means ", paste(f1(gc$mean), collapse = " / "), " CZK"), "see A4 table", "--",
        stat4, pfmt(p4), nrow(d4), N - nrow(d4))

# ============================================================
# A5  Two categorical: is a flat white more likely to be above the median in Prague 1?
# H0: share above median equal in Prague 1 and elsewhere
# H1: share is higher in Prague 1 (one-tailed; difference = Prague 1 minus elsewhere)
# ============================================================
cat("\n\n=========== A5 ===========\n")
d5 <- d[!is.na(d$prague1_f) & !is.na(d$above_med_flatwhite), ]
d5$above_f <- factor(ifelse(d5$above_med_flatwhite == 1, "Above median", "At or below median"),
                     levels = c("Above median", "At or below median"))
tab5 <- table(d5$prague1_f, d5$above_f); N5 <- sum(tab5)
cat("Median flat white used:", median(d$flatwhite, na.rm = TRUE), "CZK\n"); print(tab5)
ct5 <- chisq.test(tab5, correct = FALSE); min_exp <- min(ct5$expected)
a <- tab5[1, 1]; n_a <- sum(tab5[1, ]); b <- tab5[2, 1]; n_b <- sum(tab5[2, ])
pp <- 100 * (a / n_a - b / n_b)
pt2 <- prop.test(c(a, b), c(n_a, n_b), correct = FALSE)               # two-sided CI for the difference
pt1 <- prop.test(c(a, b), c(n_a, n_b), alternative = "greater", correct = FALSE)
V5 <- sqrt(unname(ct5$statistic) / N5)
use_fisher <- min_exp < 5
if (use_fisher) {
  ft <- fisher.test(tab5, alternative = "greater"); p5 <- ft$p.value
  stat5 <- paste0("Fisher OR=", f2(ft$estimate)); m5 <- "Fisher exact test"
} else {
  p5 <- pt1$p.value; stat5 <- paste0("$\\chi^2(1, N=", N5, ")=", f2(ct5$statistic), "$")
  m5 <- "Chi-square test (one-tailed p via z-test)"
}
pc <- function(k, n) paste0(k, " (", f1(100 * k / n), "%)")
A5 <- rbind(
  data.frame(Group = "Prague 1", n = n_a, `Above median` = pc(a, n_a), `Difference (pp)` = "", `95% CI` = "",
             Test = "", `Cramer V` = "", p = "", check.names = FALSE),
  data.frame(Group = "Elsewhere", n = n_b, `Above median` = pc(b, n_b), `Difference (pp)` = "", `95% CI` = "",
             Test = "", `Cramer V` = "", p = "", check.names = FALSE),
  data.frame(Group = "Prague 1 minus elsewhere", n = N5, `Above median` = "", `Difference (pp)` = f1(pp),
             `95% CI` = ci_str(100 * pt2$conf.int[1], 100 * pt2$conf.int[2], f1),
             Test = stat5, `Cramer V` = f2(V5), p = paste(pfmt(p5), "(one-tailed)"), check.names = FALSE))
sh(A5); write_tex(A5, "A5_table")
cells <- expand.grid(Group = rownames(tab5), Price = colnames(tab5))
write_tex(data.frame(Group = cells$Group, `Flat white price` = cells$Price, Observed = as.vector(tab5),
                     Expected = f2(as.vector(ct5$expected)), `Std. residual` = f2(as.vector(ct5$stdres)),
                     check.names = FALSE), "A5_checks")
cat("Smallest expected count:", min_exp, "->", m5, "\n")
ci_g <- t(sapply(1:2, function(i) prop.test(tab5[i, 1], sum(tab5[i, ]), correct = FALSE)$conf.int))
gdf <- data.frame(group = rownames(tab5), share = 100 * c(a / n_a, b / n_b),
                  lo = 100 * ci_g[, 1], hi = 100 * ci_g[, 2])
fig <- ggplot(gdf, aes(factor(group, levels = c("Prague 1", "Elsewhere")), share)) +
  geom_col(fill = "grey75", width = .55) + geom_errorbar(aes(ymin = lo, ymax = hi), width = .15, colour = "red") +
  labs(x = NULL, y = "Cafes with flat white above the median (%)")
save_fig(fig, "A5_figure")
add_sum("A5", m5, paste(f1(pp), "pp"), ci_str(100 * pt2$conf.int[1], 100 * pt2$conf.int[2], f1),
        paste0("$V=", f2(V5), "$"), stat5, pfmt(p5), N5, N - N5)

# ============================================================
# A6  Correlation: cappuccino price and Adjusted Rating
# H0: no correlation (rho = 0)   H1: correlation differs from 0 (two-tailed)
# ============================================================
cat("\n\n=========== A6 ===========\n")
d6 <- d[!is.na(d$cappuccino) & !is.na(d$adj_rating), ]; x <- d6$cappuccino; y <- d6$adj_rating; n6 <- nrow(d6)
par6 <- is_param(VN$cap, G_ALL) && is_param(VN$adj, G_ALL)
if (par6) {
  ct6 <- cor.test(x, y); r6 <- unname(ct6$estimate); ci6 <- ct6$conf.int; p6 <- ct6$p.value
  stat6 <- paste0("$t(", ct6$parameter, ")=", f2(ct6$statistic), "$"); m6 <- "Pearson r"; sym <- "r"
} else {
  ct6 <- cor.test(x, y, method = "spearman", exact = FALSE); r6 <- unname(ct6$estimate)
  set.seed(2026)
  bs <- replicate(5000, { i <- sample.int(n6, n6, replace = TRUE); cor(x[i], y[i], method = "spearman") })
  ci6 <- quantile(bs, c(.025, .975), names = FALSE, na.rm = TRUE); p6 <- ct6$p.value
  stat6 <- paste0("$S=", f2(ct6$statistic), "$"); m6 <- "Spearman rho (bootstrap CI)"; sym <- "\\rho"
}
A6 <- data.frame(Coefficient = paste0("$", sym, "=", sub("^(-?)0", "\\1", f2(r6)), "$"),
                 `95% CI` = ci_str(ci6[1], ci6[2]), `Shared variance` = paste0(f1(100 * r6^2), "%"),
                 Test = stat6, p = pfmt(p6), n = n6, check.names = FALSE)
sh(A6); write_tex(A6, "A6_table")
write_tex(data.frame(Check = c("Method chosen", "Cafes lost to missing values"), Result = c(m6, N - n6)), "A6_checks")
fig <- ggplot(d6, aes(adj_rating, cappuccino)) + geom_point(alpha = .6) +
  geom_smooth(method = "lm", formula = y ~ x, colour = "red", fill = "red", alpha = .15) +
  labs(x = "Adjusted Rating (points)", y = "Cappuccino price (CZK)")
save_fig(fig, "A6_figure")
add_sum("A6", m6, paste0("$", sym, "=", sub("^(-?)0", "\\1", f2(r6)), "$"), ci_str(ci6[1], ci6[2]),
        paste0("$r^2=", sub("^0", "", f2(r6^2)), "$"), stat6, pfmt(p6), n6, N - n6)

# ============================================================
# A7  Simple regression: cappuccino price on Adjusted Rating
# H0: slope = 0   H1: slope differs from 0 (two-tailed)
# ============================================================
cat("\n\n=========== A7 ===========\n")
m7 <- lm(cappuccino ~ adj_rating, data = d6)
bp7 <- lmtest::bptest(m7)$p.value; rob7 <- bp7 < .05
vc7 <- if (rob7) sandwich::vcovHC(m7, type = "HC3") else vcov(m7)
rt7 <- reg_table(m7, vc7); sh(rt7$tab); write_tex(rt7$tab, "A7_table")
sw7 <- shapiro.test(resid(m7))$p.value; cd7 <- inf_cooks(m7)
cat("Breusch-Pagan p =", bp7, "| robust SE:", rob7, "| Shapiro residuals p =", sw7,
    "| influential (Cook's D > 4/n):", cd7, "\n")
write_tex(data.frame(Check = c("Breusch-Pagan (constant variance)", "Standard errors", "Shapiro-Wilk on residuals",
                               "Influential cafes (Cook's D > 4/n)", "Cafes lost to missing values"),
  Result = c(paste0("p = ", pplain(bp7)), if (rob7) "HC3 robust" else "classical", paste0("p = ", pplain(sw7)),
             if (length(cd7)) paste(cd7, collapse = ", ") else "none", N - n6)), "A7_checks")
save_diag(m7, "A7_diag")
b7 <- rt7$est["adj_rating"]; s7 <- paste0("$t(", rt7$df, ")=", f2(b7 / rt7$se["adj_rating"]), "$")
fig <- ggplot(d6, aes(adj_rating, cappuccino)) + geom_point(alpha = .6) +
  geom_smooth(method = "lm", formula = y ~ x, colour = "red", fill = "red", alpha = .15) +
  labs(x = "Adjusted Rating (points)", y = "Cappuccino price (CZK)")
save_fig(fig, "A7_figure")
add_sum("A7", paste0("OLS", if (rob7) " (HC3 SE)" else ""), paste(f1(b7), "CZK per rating point"),
        ci_str(rt7$lo["adj_rating"], rt7$hi["adj_rating"], f1),
        paste0("$R^2=", sub("^0", "", f2(summary(m7)$r.squared)), "$"), s7, pfmt(rt7$p["adj_rating"]), n6, N - n6)

# ============================================================
# A8  Multiple regression: cappuccino price on Adjusted Rating and espresso price together
# H0: slope of Adjusted Rating = 0 given espresso price   H1: differs from 0 (two-tailed)
# ============================================================
cat("\n\n=========== A8 ===========\n")
d8 <- d[!is.na(d$cappuccino) & !is.na(d$adj_rating) & !is.na(d$espresso), ]; n8 <- nrow(d8)
m8 <- lm(cappuccino ~ adj_rating + espresso, data = d8)      # Adjusted Rating and espresso price together
bp8 <- lmtest::bptest(m8)$p.value; rob8 <- bp8 < .05
vc8 <- if (rob8) sandwich::vcovHC(m8, type = "HC3") else vcov(m8)
rt8 <- reg_table(m8, vc8); sh(rt8$tab); write_tex(rt8$tab, "A8_table")
b8 <- rt8$est["adj_rating"]
b_simple <- coef(lm(cappuccino ~ adj_rating, data = d8))["adj_rating"]   # slope without espresso price (as in A7)
remain <- 100 * b8 / b_simple
cat("Adjusted Rating slope with espresso price:", b8, "| without:", b_simple, "| share that remains:", remain, "%\n")
vif8 <- 1 / (1 - summary(lm(adj_rating ~ espresso, data = d8))$r.squared)
sw8 <- shapiro.test(resid(m8))$p.value; cd8 <- inf_cooks(m8)
write_tex(data.frame(Check = c("Breusch-Pagan (constant variance)", "Standard errors", "VIF of Adjusted Rating",
                               "Shapiro-Wilk on residuals", "Influential cafes (Cook's D > 4/n)",
                               "Slope without espresso price (as in A7), CZK", "Share of that slope that remains",
                               "Cafes lost to missing values"),
  Result = c(paste0("p = ", pplain(bp8)), if (rob8) "HC3 robust" else "classical", f2(vif8),
             paste0("p = ", pplain(sw8)), if (length(cd8)) paste(cd8, collapse = ", ") else "none",
             f1(b_simple), paste0(f1(remain), "%"), N - n8)), "A8_checks")
save_diag(m8, "A8_diag")
# figure: Adjusted Rating and cappuccino price, each after removing what espresso price explains
# (the slope of this line is the Adjusted Rating coefficient of the multiple regression)
av <- data.frame(x = resid(lm(adj_rating ~ espresso, data = d8)), y = resid(lm(cappuccino ~ espresso, data = d8)))
fig <- ggplot(av, aes(x, y)) + geom_point(alpha = .6) +
  geom_smooth(method = "lm", formula = y ~ x, colour = "red", fill = "red", alpha = .15) +
  labs(x = "Adjusted Rating given espresso price (points)", y = "Cappuccino price given espresso price (CZK)")
save_fig(fig, "A8_figure")
s8 <- paste0("$t(", rt8$df, ")=", f2(b8 / rt8$se["adj_rating"]), "$")
add_sum("A8", paste0("Multiple regression (OLS)", if (rob8) ", HC3 SE" else ""),
        paste(f1(b8), "CZK per rating point"),
        ci_str(rt8$lo["adj_rating"], rt8$hi["adj_rating"], f1),
        paste0("$R^2=", sub("^0", "", f2(summary(m8)$r.squared)), "$"), s8, pfmt(rt8$p["adj_rating"]), n8, N - n8)

# ============================================================
# A9  Interaction: does the rating-price slope differ between roasteries and other cafes?
# H0: interaction coefficient = 0   H1: it differs from 0 (two-tailed)
# ============================================================
cat("\n\n=========== A9 ===========\n")
d9 <- d[!is.na(d$cappuccino) & !is.na(d$adj_rating) & !is.na(d$roastery_f), ]; n9 <- nrow(d9)
d9$grp <- relevel(d9$roastery_f, ref = "Other cafes")
cat("Roasteries in this analysis:", sum(d9$grp == "Roastery"), "of", n9, "\n")
m9 <- lm(cappuccino ~ adj_rating * grp, data = d9)
bp9 <- lmtest::bptest(m9)$p.value; rob9 <- bp9 < .05
vc9 <- if (rob9) sandwich::vcovHC(m9, type = "HC3") else vcov(m9)
rt9 <- reg_table(m9, vc9); sh(rt9$tab); write_tex(rt9$tab, "A9_table")
ni <- "adj_rating:grpRoastery"; dfr <- m9$df.residual; b <- coef(m9)
s_oth <- b["adj_rating"]; se_oth <- sqrt(vc9["adj_rating", "adj_rating"])
s_roa <- b["adj_rating"] + b[ni]
se_roa <- sqrt(vc9["adj_rating", "adj_rating"] + vc9[ni, ni] + 2 * vc9["adj_rating", ni])
h <- qt(.975, dfr)
SL <- data.frame(Group = c("Other cafes", "Roasteries"), `Slope (CZK per rating point)` = f1(c(s_oth, s_roa)),
                 `95% CI` = c(ci_str(s_oth - h * se_oth, s_oth + h * se_oth, f1),
                              ci_str(s_roa - h * se_roa, s_roa + h * se_roa, f1)), check.names = FALSE)
sh(SL); write_tex(SL, "A9_slopes")
sw9 <- shapiro.test(resid(m9))$p.value; cd9 <- inf_cooks(m9)
write_tex(data.frame(Check = c("Breusch-Pagan (constant variance)", "Standard errors", "Shapiro-Wilk on residuals",
                               "Influential cafes (Cook's D > 4/n)", "Roasteries in the sample",
                               "Cafes lost to missing values"),
  Result = c(paste0("p = ", pplain(bp9)), if (rob9) "HC3 robust" else "classical", paste0("p = ", pplain(sw9)),
             if (length(cd9)) paste(cd9, collapse = ", ") else "none", sum(d9$grp == "Roastery"), N - n9)), "A9_checks")
save_diag(m9, "A9_diag")
fig <- ggplot(d9, aes(adj_rating, cappuccino, colour = grp, fill = grp)) + geom_point(alpha = .6) +
  geom_smooth(method = "lm", formula = y ~ x, alpha = .15) +
  labs(x = "Adjusted Rating (points)", y = "Cappuccino price (CZK)", colour = NULL, fill = NULL)
save_fig(fig, "A9_figure")
int_est <- rt9$est[ni]
add_sum("A9", paste0("OLS with interaction", if (rob9) " (HC3 SE)" else ""),
        paste0("Slope difference (roasteries minus others) ", f1(int_est), " CZK per point"),
        ci_str(rt9$lo[ni], rt9$hi[ni], f1), paste0("$R^2=", sub("^0", "", f2(summary(m9)$r.squared)), "$"),
        paste0("$t(", rt9$df, ")=", f2(int_est / rt9$se[ni]), "$"), pfmt(rt9$p[ni]), n9, N - n9)

# ============================================================
# A10  Logistic regression: flat white above the median vs Adjusted Rating
# H0: coefficient of Adjusted Rating (per 0.1) = 0   H1: coefficient differs from 0 (two-tailed)
# ============================================================
cat("\n\n=========== A10 ===========\n")
d10 <- d[!is.na(d$above_med_flatwhite) & !is.na(d$adj_rating_tenth), ]; n10 <- nrow(d10)
m10 <- glm(above_med_flatwhite ~ adj_rating_tenth, data = d10, family = binomial)
cf <- summary(m10)$coefficients; b10 <- cf["adj_rating_tenth", 1]; se10 <- cf["adj_rating_tenth", 2]
z10 <- cf["adj_rating_tenth", 3]; p10 <- cf["adj_rating_tenth", 4]          # two-tailed Wald test
or10 <- exp(b10 + c(-1, 1) * qnorm(.975) * se10)
# average marginal effect (percentage points per 0.1) with delta-method CI
ame_fun <- function(par) {
  pp <- plogis(par[1] + par[2] * d10$adj_rating_tenth); mean(par[2] * pp * (1 - pp))
}
par_hat <- coef(m10); V <- vcov(m10); eps <- 1e-6
grad <- sapply(1:2, function(i) { hi <- par_hat; lo <- par_hat; hi[i] <- hi[i] + eps; lo[i] <- lo[i] - eps
                                  (ame_fun(hi) - ame_fun(lo)) / (2 * eps) })
ame <- ame_fun(par_hat); se_ame <- sqrt(as.numeric(t(grad) %*% V %*% grad))
ame_ci <- 100 * (ame + c(-1, 1) * qnorm(.975) * se_ame)
A10 <- data.frame(`AME (pp per 0.1)` = f2(100 * ame), `95% CI` = ci_str(ame_ci[1], ame_ci[2]),
                  `Odds ratio` = f2(exp(b10)), `OR 95% CI` = ci_str(or10[1], or10[2]),
                  Test = paste0("$z=", f2(z10), "$"), p = pfmt(p10), n = n10,
                  check.names = FALSE)
sh(A10); write_tex(A10, "A10_table")
m10q <- glm(above_med_flatwhite ~ adj_rating_tenth + I(adj_rating_tenth^2), data = d10, family = binomial)
lin_p <- anova(m10, m10q, test = "Chisq")[["Pr(>Chi)"]][2]
cd10 <- inf_cooks(m10); epv <- min(sum(d10$above_med_flatwhite), sum(1 - d10$above_med_flatwhite))
mcf <- 1 - as.numeric(logLik(m10) / logLik(update(m10, . ~ 1)))
write_tex(data.frame(Check = c("Linearity of the logit (LR test, quadratic term)", "Influential cafes (Cook's D > 4/n)",
                               "Cafes in the smaller outcome class", "McFadden pseudo-R2",
                               "Cafes lost to missing values"),
  Result = c(paste0("p = ", pplain(lin_p)), if (length(cd10)) paste(cd10, collapse = ", ") else "none",
             epv, f2(mcf), N - n10)), "A10_checks")
grid <- data.frame(adj_rating_tenth = seq(min(d10$adj_rating_tenth), max(d10$adj_rating_tenth), length.out = 100))
pr <- predict(m10, grid, type = "link", se.fit = TRUE)
grid$fit <- plogis(pr$fit); grid$lo <- plogis(pr$fit - 1.96 * pr$se.fit); grid$hi <- plogis(pr$fit + 1.96 * pr$se.fit)
grid$rating <- grid$adj_rating_tenth * 0.1; d10$rating <- d10$adj_rating_tenth * 0.1
fig <- ggplot() +
  geom_jitter(data = d10, aes(rating, above_med_flatwhite), height = .03, alpha = .4) +
  geom_ribbon(data = grid, aes(rating, ymin = lo, ymax = hi), alpha = .2, fill = "red") +
  geom_line(data = grid, aes(rating, fit), colour = "red") +
  labs(x = "Adjusted Rating (points)", y = "Probability that flat white price is above the median")
save_fig(fig, "A10_figure")
add_sum("A10", "Logit + average marginal effect", paste(f2(100 * ame), "pp per 0.1 rating point"),
        ci_str(ame_ci[1], ame_ci[2]), paste0("OR $=", f2(exp(b10)), "$"), paste0("$z=", f2(z10), "$"),
        pfmt(p10), n10, N - n10)

# ============================================================
# Summary table of the main results (S_table.tex)
# ============================================================
cat("\n\n=========== Summary of all analyses ===========\n")
options(width = 220); sh(SUM)
write_tex(SUM, "S_table", align = "lllllllrr")

# ============================================================
# Numbers for the report (numbers.tex): every number quoted in the text of the
# report is written here as a LaTeX macro, so the text always matches this analysis.
# ============================================================
NUM <- list()
num <- function(name, value) NUM[[name]] <<- as.character(value)
words <- c("One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten")
for (i in seq_len(nrow(SUM))) {                                  # the summary table, one macro per cell
  w <- paste0("A", words[i])
  num(paste0(w, "Method"), SUM$Method[i]);       num(paste0(w, "Est"), SUM$Estimate[i])
  num(paste0(w, "CI"), SUM[["95% CI"]][i]);      num(paste0(w, "Eff"), SUM[["Effect size"]][i])
  num(paste0(w, "Test"), SUM$Test[i]);           num(paste0(w, "P"), SUM$p[i])
  num(paste0(w, "N"), SUM$n[i]);                 num(paste0(w, "Lost"), SUM$Lost[i])
}
num("ATotal", N)
num("AOneBench", f1(bench)); num("AOneRate", f2(usd_czk)); num("AOneDiff", f1(est1 - bench))
num("ATwoNRoast", length(x1)); num("ATwoNOther", length(x0))
num("ATwoMeanRoast", f1(mean(x1))); num("ATwoMeanOther", f1(mean(x0))); num("ATwoLevene", pfmt(lev2))
num("AThreeMedian", f1(median(d3$fw_minus_cap))); num("AThreeSame", sum(d3$fw_minus_cap == 0))
num("AThreePct", f1(100 * est3 / mean(d$cappuccino, na.rm = TRUE)))
for (i in 1:3) {                                                 # A4 group means
  nm <- c("Ind", "Loc", "Int")[i]
  num(paste0("AFourN", nm), gc$n[i]); num(paste0("AFourMean", nm), f1(gc$mean[i]))
  num(paste0("AFourCI", nm), ci_str(gc$lo[i], gc$hi[i], f1))
}
num("AFourLevene", pfmt(lev4))
if (par4) for (i in 1:3) {                                       # A4 pairwise (Independent-Local, Independent-Intl, Local-Intl)
  nm <- c("One", "Two", "Three")[i]
  num(paste0("AFourPair", nm, "Diff"), f1(ph$diff[i]))
  num(paste0("AFourPair", nm, "CI"), ci_str(ph$lo[i], ph$hi[i], f1))
  num(paste0("AFourPair", nm, "P"), pfmt(ph$p_adj[i]))
}
num("AFiveYesOne", tab5[1, 1]); num("AFiveNOne", sum(tab5[1, ])); num("AFivePctOne", f1(100 * tab5[1, 1] / sum(tab5[1, ])))
num("AFiveYesTwo", tab5[2, 1]); num("AFiveNTwo", sum(tab5[2, ])); num("AFivePctTwo", f1(100 * tab5[2, 1] / sum(tab5[2, ])))
num("AFiveMedian", f1(median(d$flatwhite, na.rm = TRUE))); num("AFiveMinExp", f1(min_exp))
num("AFiveStdRes", f2(max(abs(ct5$stdres))))
num("ASevenPerTenth", f1(b7 / 10)); num("ASevenBP", pfmt(bp7)); num("ASevenSW", pfmt(sw7)); num("ASevenCook", length(cd7))
num("AEightSimple", f1(b_simple)); num("AEightRemain", f1(remain)); num("AEightVIF", f2(vif8))
num("AEightEsp", f2(rt8$est["espresso"])); num("AEightEspCI", ci_str(rt8$lo["espresso"], rt8$hi["espresso"]))
num("AEightEspP", pfmt(rt8$p["espresso"]))
num("AEightBP", pfmt(bp8)); num("AEightSW", pfmt(sw8)); num("AEightCook", length(cd8))
h9 <- qt(.975, m9$df.residual)
num("ANineSlopeOther", f1(s_oth)); num("ANineSlopeOtherCI", ci_str(s_oth - h9 * se_oth, s_oth + h9 * se_oth, f1))
num("ANineSlopeRoast", f1(s_roa)); num("ANineSlopeRoastCI", ci_str(s_roa - h9 * se_roa, s_roa + h9 * se_roa, f1))
num("ANineRoast", sum(d9$grp == "Roastery")); num("ANineInt", f1(int_est))
num("ANineBP", pfmt(bp9)); num("ANineSW", pfmt(sw9)); num("ANineCook", length(cd9))
num("ATenOR", f2(exp(b10))); num("ATenORCI", ci_str(or10[1], or10[2])); num("ATenEvents", epv)
num("ATenLin", pfmt(lin_p)); num("ATenCook", length(cd10)); num("ATenMcf", f2(mcf))
writeLines(vapply(names(NUM), function(k) paste0("\\newcommand{\\", k, "}{", tex_safe(NUM[[k]]), "}"), character(1)),
           "../report/tables/numbers.tex")
cat("\nAll tables and figures saved in ../report/\n")

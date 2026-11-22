# ============================================================
# interim/helpers.R - functions shared by 02_descriptive.R and 03_analysis.R
# (sourced by those scripts; not run on its own). ASCII only.
# ============================================================
suppressPackageStartupMessages(library(ggplot2))
theme_set(theme_bw(base_size = 11))

setup_dirs <- function() {
  dir.create("../report/tables",  recursive = TRUE, showWarnings = FALSE)
  dir.create("../report/figures", recursive = TRUE, showWarnings = FALSE)
}

# names used to link the descriptive table (02) to the analyses (03)
VN <- list(esp  = "Espresso price (CZK)",
           cap  = "Cappuccino price (CZK)",
           diff = "Flat white minus cappuccino (CZK)",
           fw   = "Flat white price (CZK)",
           adj  = "Adjusted Rating (points)")
G_ALL  <- "All cafes"
G_DIFF <- "Paired differences"

load_data <- function(file = "dataset.csv") {
  d <- read.csv(file, na.strings = c("", "NA"), stringsAsFactors = FALSE)
  ct <- tolower(trimws(d$cafe_type))
  d$cafe_type_f <- factor(ct, levels = c("independent", "local chain", "international chain"),
                          labels = c("Independent", "Local chain", "International chain"))
  if (any(!is.na(ct) & is.na(d$cafe_type_f)))
    warning("Unexpected cafe type labels: ", paste(unique(ct[!is.na(ct) & is.na(d$cafe_type_f)]), collapse = ", "))
  d$roastery_f <- factor(ifelse(d$roastery == 1, "Roastery", "Other cafes"),
                         levels = c("Roastery", "Other cafes"))
  d$prague1_f  <- factor(ifelse(d$prague1 == 1, "Prague 1", "Elsewhere"),
                         levels = c("Prague 1", "Elsewhere"))
  d
}

# ---- formatting ----
f1 <- function(x) formatC(x, format = "f", digits = 1)
f2 <- function(x) formatC(x, format = "f", digits = 2)
fx <- function(variable, x) if (grepl("CZK", variable)) f1(x) else f2(x)   # prices: 1 decimal
ci_str <- function(lo, hi, f = f2) paste0("[", f(lo), ", ", f(hi), "]")
pplain <- function(p) ifelse(p < .001, "<.001", sub("^0", "", formatC(p, format = "f", digits = 3)))
pfmt   <- function(p) ifelse(p < .001, "$p<.001$", paste0("$p=", sub("^0", "", formatC(p, format = "f", digits = 3)), "$"))

# ---- export ----
write_tex <- function(df, name, align = NULL) {
  esc <- function(x) gsub("([&%#_])", "\\\\\\1", as.character(x))
  df[] <- lapply(df, function(col) ifelse(is.na(col), "", esc(col)))
  if (is.null(align)) align <- paste0("l", strrep("r", ncol(df) - 1))
  hdr  <- paste(esc(names(df)), collapse = " & ")
  body <- do.call(paste, c(unname(as.list(df)), sep = " & "))
  lines <- c(paste0("\\begin{tabular}{", align, "}"), "\\toprule",
             paste0(hdr, " \\\\"), "\\midrule", paste0(body, " \\\\"),
             "\\bottomrule", "\\end{tabular}")
  writeLines(lines, file.path("../report/tables", paste0(name, ".tex")))
}
save_fig <- function(p, name, w = 6.5, h = 4.2)
  ggsave(file.path("../report/figures", paste0(name, ".pdf")), p, width = w, height = h)

# ---- statistics ----
skewness <- function(x) { x <- x[!is.na(x)]; m <- mean(x); mean((x - m)^3) / mean((x - m)^2)^1.5 }
cohen_d <- function(x, y) {
  nx <- length(x); ny <- length(y)
  sp <- sqrt(((nx - 1) * var(x) + (ny - 1) * var(y)) / (nx + ny - 2))
  (mean(x) - mean(y)) / sp
}
boot_means <- function(x, B = 5000, seed = 2026) {
  set.seed(seed); n <- length(x)
  replicate(B, mean(x[sample.int(n, n, replace = TRUE)]))
}
boot_ci <- function(x, B = 5000, seed = 2026) {          # percentile CI of the mean
  quantile(boot_means(x, B, seed), c(.025, .975), names = FALSE)
}
levene_p <- function(x, g) {                              # Brown-Forsythe / Levene test
  g <- droplevels(factor(g)); z <- abs(x - ave(x, g, FUN = median))
  anova(lm(z ~ g))[["Pr(>F)"]][1]
}
group_ci <- function(x, g) {                              # group means with t-based 95% CI
  g <- droplevels(factor(g))
  do.call(rbind, lapply(levels(g), function(l) {
    v <- x[g == l]; n <- length(v); m <- mean(v); h <- qt(.975, n - 1) * sd(v) / sqrt(n)
    data.frame(group = l, n = n, mean = m, lo = m - h, hi = m + h, stringsAsFactors = FALSE)
  }))
}
games_howell <- function(x, g) {
  g <- droplevels(factor(g)); lv <- levels(g)
  m <- as.numeric(tapply(x, g, mean)); v <- as.numeric(tapply(x, g, var)); n <- as.numeric(table(g))
  out <- NULL
  for (i in 1:(length(lv) - 1)) for (j in (i + 1):length(lv)) {
    diff <- m[i] - m[j]
    s2 <- v[i] / n[i] + v[j] / n[j]
    df <- s2^2 / ((v[i] / n[i])^2 / (n[i] - 1) + (v[j] / n[j])^2 / (n[j] - 1))
    q  <- abs(diff) / sqrt(s2) * sqrt(2)
    p  <- ptukey(q, length(lv), df, lower.tail = FALSE)
    h  <- qtukey(.95, length(lv), df) / sqrt(2) * sqrt(s2)
    out <- rbind(out, data.frame(pair = paste(lv[i], "minus", lv[j]), diff = diff,
                                 lo = diff - h, hi = diff + h, p_adj = p, stringsAsFactors = FALSE))
  }
  out
}
dunn_holm <- function(x, g) {
  g <- droplevels(factor(g)); lv <- levels(g); N <- length(x)
  r <- rank(x); n <- as.numeric(table(g)); rbar <- as.numeric(tapply(r, g, mean))
  tc <- table(x); sig2 <- N * (N + 1) / 12 - sum(tc^3 - tc) / (12 * (N - 1))
  med <- as.numeric(tapply(x, g, median)); out <- NULL
  for (i in 1:(length(lv) - 1)) for (j in (i + 1):length(lv)) {
    z <- (rbar[i] - rbar[j]) / sqrt(sig2 * (1 / n[i] + 1 / n[j]))
    out <- rbind(out, data.frame(pair = paste(lv[i], "minus", lv[j]), diff_median = med[i] - med[j],
                                 p = 2 * pnorm(-abs(z)), stringsAsFactors = FALSE))
  }
  out$p_adj <- p.adjust(out$p, "holm"); out
}

# ---- regression helpers ----
reg_table <- function(model, vc = NULL) {
  cf <- summary(model)$coefficients
  est <- cf[, 1]; se <- if (is.null(vc)) cf[, 2] else sqrt(diag(vc))
  dfr <- model$df.residual; tv <- est / se; p <- 2 * pt(-abs(tv), dfr); h <- qt(.975, dfr) * se
  out <- data.frame(Term = rownames(cf), Estimate = f2(est), SE = f2(se),
                    CI = ci_str(est - h, est + h), t = f2(tv), p = pplain(p), stringsAsFactors = FALSE)
  extra <- data.frame(Term = c("Observations", "$R^2$"),
                      Estimate = c(as.character(nobs(model)), f2(summary(model)$r.squared)),
                      SE = "", CI = "", t = "", p = "", stringsAsFactors = FALSE)
  tab <- rbind(out, extra); names(tab)[4] <- "95% CI"
  list(tab = tab, est = est, se = se, lo = est - h, hi = est + h, p = p, df = dfr)
}
save_diag <- function(model, name) {                      # residual-vs-fitted + normal Q-Q in one PDF
  df <- data.frame(fitted = fitted(model), resid = resid(model), std = rstandard(model))
  p1 <- ggplot(df, aes(fitted, resid)) + geom_point(alpha = .6) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_smooth(method = "loess", formula = y ~ x, se = FALSE, colour = "red", linewidth = .6) +
    labs(x = "Fitted values", y = "Residuals", title = "Residuals vs fitted")
  p2 <- ggplot(df, aes(sample = std)) + stat_qq() + stat_qq_line(colour = "red") +
    labs(x = "Theoretical quantiles", y = "Standardised residuals", title = "Normal Q-Q")
  grDevices::pdf(file.path("../report/figures", paste0(name, ".pdf")), width = 9, height = 4)
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(1, 2)))
  print(p1, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
  print(p2, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 2))
  grDevices::dev.off()
  invisible(NULL)
}

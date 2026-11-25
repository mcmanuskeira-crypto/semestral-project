# ============================================================
# 01_cleaning.R  -  Prague cafe data: inspect, clean, create variables
# Input : raw_data.csv (raw Google-Sheet export, never edited by hand)
# Output: dataset.csv  (cleaned data, no row numbers)
# Base R only - no packages needed.
# ============================================================

# Run from the "analysis" folder (RStudio: Session > Set Working Directory > To Source File Location)
infile <- "raw_data.csv"                       # the raw CSV downloaded from Google Drive
if (!file.exists(infile) && file.exists("2__Data_Collection_Sheet_-_Sheet1.csv"))
  infile <- "2__Data_Collection_Sheet_-_Sheet1.csv"
if (!file.exists(infile)) {
  message("Raw CSV not found in the working folder - select it in the window.")
  infile <- file.choose()
}
outfile <- "dataset.csv"                       # the only file later scripts read

# ---------- 1. INSPECT BEFORE CHANGING ANYTHING ----------
# (a) the types R guesses by default: a numeric column stored as text
#     (chr) means it contains non-numeric values
guess <- read.csv(infile, na.strings = c("", "NA"), check.names = FALSE)
cat("\n=== Structure with R's default types ===\n")
str(guess)

# (b) read everything as text so nothing is changed silently
raw <- read.csv(infile, colClasses = "character", na.strings = c("", "NA"),
                check.names = FALSE, strip.white = TRUE)
cat("\nRows x columns in raw file:", nrow(raw), "x", ncol(raw), "\n")
# Short names, matched by the START OF EACH HEADER (not by position), so the
# script still works if the sheet gains or loses extra columns.
patterns <- c(name = "^Name$", visit_time = "^Date and time", district_num = "^District \\(number\\)",
              district_name = "^District \\(name\\)", cafe = "^Cafe name", address = "^Cafe address",
              coords = "^Coordinates", link = "^Link to GMaps", p_espresso = "^Espresso",
              p_cappuccino = "^Cappuccino", p_flatwhite = "^Flat White", p_filter = "^Filter",
              payment = "^Payment", seats_in = "^Seating Places Inside",
              seats_out = "^Seating Places Outside", rating = "^Rating \\(Google\\)",
              n_ratings = "^Number of ratings", cafe_type = "^Cafe Type", roastery = "^Is Roastery",
              photo_folder = "^Photo Link", note = "^Note", auto_arrow = "^AUTOMATIC",
              dist_orloj = "^Distance from", lat_sheet = "^Latitude", lon_sheet = "^Longitude")
idx <- sapply(patterns, function(pt) {
  hit <- grep(pt, names(raw), ignore.case = TRUE)
  if (length(hit) != 1) stop("Column not found exactly once for pattern: ", pt)
  hit
})
# optional: the sheet's own Adjusted Rating column (used only to compare with ours)
adj_hit <- grep("^Adjusted Rating", names(raw), ignore.case = TRUE)
unused <- setdiff(seq_along(raw), c(idx, adj_hit))
if (length(unused)) cat("Columns not used:", paste(names(raw)[unused], collapse = " | "), "\n")
keep <- c(idx, if (length(adj_hit) == 1) adj_hit)
raw <- raw[, keep]
names(raw) <- c(names(patterns), if (length(adj_hit) == 1) "adj_sheet")

cat("\n=== Missing values per column (raw) ===\n")
print(colSums(is.na(raw)))

# Empty rows: no cafe name, coordinates or link. (Payment and the automatic
# columns are pre-filled in empty rows, so they are not used for this test.)
empty <- is.na(raw$cafe) & is.na(raw$coords) & is.na(raw$link)
cat("\nCompletely empty rows:", sum(empty), "\n")
cat("Exact duplicated rows (all columns):", sum(duplicated(raw)), "\n")

n_before <- nrow(raw)
df <- raw[!empty, ]
cat("Rows after dropping empty rows:", nrow(df), "\n")

# The sheet also holds the professor's example row (collector "Vojtech Zika").
# It was not collected by a student, so it is not part of the data.
is_example <- grepl("^Vojt", df$name)
cat("\nExample row removed:", df$cafe[is_example], "\n")
df <- df[!is_example, ]
cat("Student rows:", nrow(df), "\n")

# Duplicates: the same cafe entered by two students. The spelling differs
# ("Cafe Savoy" / "Cafe Savoy"), so first list all entries whose coordinates
# are within 30 metres of each other and inspect them by eye.
xy  <- strsplit(df$coords, ",")
lat <- as.numeric(sapply(xy, function(z) z[1]))
lon <- as.numeric(sapply(xy, function(z) z[2]))
pr  <- which(upper.tri(diag(nrow(df))), arr.ind = TRUE)
metres <- sqrt(((lat[pr[, 1]] - lat[pr[, 2]]) * 111)^2 + ((lon[pr[, 1]] - lon[pr[, 2]]) * 71)^2) * 1000
w <- which(metres <= 30)
cat("\n=== Entries within 30 m of each other ===\n")
print(data.frame(cafe_1 = df$cafe[pr[w, 1]], by_1 = df$name[pr[w, 1]],
                 cafe_2 = df$cafe[pr[w, 2]], by_2 = df$name[pr[w, 2]], metres = round(metres[w])),
      row.names = FALSE)

# DECISION: 11 of these pairs are the same cafe entered twice (same place, same
# rating and number of ratings, different spelling of the name). Other close
# pairs are different cafes in the same building (e.g. "The Cup" and "Atomic
# matcha") and are kept. EMA espresso bar appears at several different
# addresses, so those are different branches and are kept too.
# For each repeated cafe the first entry in the sheet is kept, the second is dropped.
dup_patterns <- c("louvre", "starbucks reserve", "no.?26", "milani", "republica", "duo cafe",
                  "vescovi", "mondie?u", "kampa cafe", "savoy", "^cafe.?cafe$")
cafe_lc  <- tolower(trimws(df$cafe))
drop_row <- integer(0)
for (pt in dup_patterns) {
  rows <- which(grepl(pt, cafe_lc))
  stopifnot(length(rows) == 2)            # each pattern must match exactly the two entries
  drop_row <- c(drop_row, rows[2])
}
df <- df[-drop_row, ]
cat("Cafes after removing repeated entries:", nrow(df), "\n")

# ---------- 2. CATEGORIES ----------
cat("\n=== Raw categories ===\n")
for (v in c("district_num", "cafe_type", "payment", "roastery")) {
  cat("\n", v, "\n"); print(table(df[[v]], useNA = "ifany"))
}

# District: lower-case/trim, then rebuild as "Prague <number>".
# Fixes "prague 1", "prauge 1", "Prage 7", trailing spaces.
df$district <- ifelse(is.na(df$district_num), NA,
                      paste("Prague", gsub("[^0-9]", "", df$district_num)))
# Cafe type: trim, standardise capitals; blank already read as NA
df$cafe_type_clean <- tools::toTitleCase(tolower(trimws(df$cafe_type)))
df$payment_clean   <- trimws(df$payment)
# Entries combining two categories? (look for separators other than the
# legitimate "Cash & Card")
combo <- grepl("[/,;]", df$cafe_type) | grepl("[/,;]", df$district_num) |
         grepl("[/,;]", df$payment)
cat("\nRows with combined categories:", sum(combo), "\n")

cat("\n=== Cleaned categories (compare with raw above) ===\n")
for (v in c("district", "cafe_type_clean", "payment_clean")) {
  cat("\n", v, "\n"); print(table(df[[v]], useNA = "ifany"))
}

# ---------- 3. PRICES ----------
# Show every price cell that is not a plain number
price_cols <- c("p_espresso", "p_cappuccino", "p_flatwhite", "p_filter")
cat("\n=== Price cells that are not a single number ===\n")
for (v in price_cols) {
  bad <- unique(df[[v]][!is.na(df[[v]]) & !grepl("^[0-9]+(\\.[0-9]+)?$", df[[v]])])
  cat(v, ":", paste(bad, collapse = " | "), "\n")
}
# DECISION: several prices ("85, 95, 105", "65/40", "45,60") -> take the FIRST
# listed value (default rule from the directions). The text "na" -> NA.
first_price <- function(x) {
  x <- trimws(x)
  out <- suppressWarnings(as.numeric(sub("^([0-9]+(\\.[0-9]+)?).*$", "\\1", x)))
  out[!grepl("^[0-9]", x)] <- NA
  out
}
df$espresso   <- first_price(df$p_espresso)
df$cappuccino <- first_price(df$p_cappuccino)
df$flatwhite  <- first_price(df$p_flatwhite)
df$filter     <- first_price(df$p_filter)

cat("\nCheck (original vs converted), rows that had multiple prices:\n")
for (v in price_cols) {
  nm <- sub("p_", "", v); nm <- c(espresso="espresso", cappuccino="cappuccino",
                                  flatwhite="flatwhite", filter="filter")[nm]
  i <- which(!is.na(df[[v]]) & !grepl("^[0-9]+(\\.[0-9]+)?$", df[[v]]))
  if (length(i)) print(head(data.frame(original = df[[v]][i], kept = df[[nm]][i]), 8))
}

# DECISION: an NA filter price means the cafe does not serve filter coffee.
# The price stays NA (never 0); a separate 0/1 indicator records the offer.
df$serves_filter <- ifelse(is.na(df$filter), 0, 1)

# ---------- 4. TEXT -> NUMBERS ----------
num_cols <- c("seats_in", "seats_out", "rating", "n_ratings", "roastery")
cat("\n=== Values containing anything other than digits and a decimal point ===\n")
for (v in num_cols) {
  bad <- unique(df[[v]][!is.na(df[[v]]) & !grepl("^[0-9]+(\\.[0-9]+)?$", df[[v]])])
  cat(v, ":", if (length(bad)) paste(bad, collapse = " | ") else "none", "\n")
}
# Problem confirmed: number of ratings has thousands separators ("1,020").
# Make sure the ONLY issue is a comma used as thousands separator before removing it.
bad_n <- df$n_ratings[!is.na(df$n_ratings) & !grepl("^[0-9]+$", df$n_ratings)]
stopifnot(all(grepl("^[0-9]{1,3}(,[0-9]{3})+$", bad_n)))
df$n_ratings_num <- as.numeric(gsub(",", "", df$n_ratings))
df$rating_num    <- as.numeric(df$rating)
df$seats_in_num  <- as.numeric(df$seats_in)
df$seats_out_num <- as.numeric(df$seats_out)
df$roastery_num  <- as.numeric(df$roastery)

cat("\nVisual check of the conversion (original text vs number):\n")
chk <- which(df$n_ratings %in% bad_n)
print(data.frame(original = df$n_ratings[chk], converted = df$n_ratings_num[chk]))
stopifnot(all(is.na(df$n_ratings) == is.na(df$n_ratings_num)))   # no NA created

# Visit time (mixed formats "9/22/2026" and "9/22/26") - kept for reference
vt <- trimws(df$visit_time)
df$visit_time_clean <- as.POSIXct(rep(NA_character_, length(vt)), tz = "Europe/Prague")
short <- grepl("^[0-9]+/[0-9]+/[0-9]{2} ", vt)
long  <- grepl("^[0-9]+/[0-9]+/[0-9]{4} ", vt)
df$visit_time_clean[long]  <- as.POSIXct(vt[long],  format = "%m/%d/%Y %H:%M:%S", tz = "Europe/Prague")
df$visit_time_clean[short] <- as.POSIXct(vt[short], format = "%m/%d/%y %H:%M:%S", tz = "Europe/Prague")

# ---------- 5. NEW VARIABLES ----------
# Coordinates: split "50.073..., 14.438..." into latitude and longitude
parts <- strsplit(df$coords, ",")
df$latitude  <- as.numeric(sapply(parts, function(p) trimws(p[1])))
df$longitude <- as.numeric(sapply(parts, function(p) trimws(p[2])))
cat("\nMax difference vs the sheet's own Latitude/Longitude columns:",
    max(abs(df$latitude - as.numeric(df$lat_sheet)), na.rm = TRUE), "/",
    max(abs(df$longitude - as.numeric(df$lon_sheet)), na.rm = TRUE), "\n")

# Distances from the Orloj (50.0871 N, 14.4207 E); 1 deg lat = 111 km, 1 deg lon = 71 km
df$ns_km <- (df$latitude  - 50.0871) * 111      # + = north of the Orloj
df$ew_km <- (df$longitude - 14.4207) * 71       # + = east of the Orloj
df$abs_ns_km <- abs(df$ns_km)                   # for question A3
df$abs_ew_km <- abs(df$ew_km)

# Adjusted Rating (shrinkage): (n*R + 50*C) / (n + 50)
# C = mean raw rating of all cafes (computed in code, not typed in)
C <- mean(df$rating_num, na.rm = TRUE)
cat("C (mean rating of all cafes) =", C, "\n")
df$adj_rating <- (df$n_ratings_num * df$rating_num + 50 * C) / (df$n_ratings_num + 50)
if ("adj_sheet" %in% names(df)) {
  cat("Max difference between my Adjusted Rating and the sheet's own column:",
      max(abs(df$adj_rating - suppressWarnings(as.numeric(df$adj_sheet))), na.rm = TRUE), "\n")
}

# Price Level: mean of espresso, cappuccino, flat white - ONLY cafes with all three
all3 <- !is.na(df$espresso) & !is.na(df$cappuccino) & !is.na(df$flatwhite)
df$price_level <- ifelse(all3, (df$espresso + df$cappuccino + df$flatwhite) / 3, NA)

# Indicators (0/1; NA stays NA)
df$prague1       <- ifelse(is.na(df$district), NA, as.numeric(df$district == "Prague 1"))
df$outdoor       <- ifelse(is.na(df$seats_out_num), NA, as.numeric(df$seats_out_num > 0))
df$above_med_adj <- ifelse(is.na(df$adj_rating), NA,
                           as.numeric(df$adj_rating > median(df$adj_rating, na.rm = TRUE)))
df$above_med_price <- ifelse(is.na(df$price_level), NA,
                             as.numeric(df$price_level > median(df$price_level, na.rm = TRUE)))
df$above_med_nrat  <- ifelse(is.na(df$n_ratings_num), NA,
                             as.numeric(df$n_ratings_num > median(df$n_ratings_num, na.rm = TRUE)))
# Q5 / Q10: flat white price above the median flat white price (median computed in code)
med_fw <- median(df$flatwhite, na.rm = TRUE)
df$above_med_flatwhite <- ifelse(is.na(df$flatwhite), NA, as.numeric(df$flatwhite > med_fw))
cat("Median flat white price used for the indicator:", med_fw, "CZK\n")

# Q3 (paired): flat white minus cappuccino, only where BOTH prices exist
df$fw_minus_cap <- ifelse(!is.na(df$flatwhite) & !is.na(df$cappuccino),
                          df$flatwhite - df$cappuccino, NA)

# Q10: Adjusted Rating in steps of 0.1 (so a logit/marginal effect is "per 0.1")
df$adj_rating_tenth <- df$adj_rating / 0.1

cat("Medians used: adj_rating =", median(df$adj_rating, na.rm = TRUE),
    "| price_level =", median(df$price_level, na.rm = TRUE),
    "| n_ratings =", median(df$n_ratings_num, na.rm = TRUE), "\n")

# ---------- 6. FINAL DATA SET (originals kept next to clean versions) ----------
clean <- data.frame(
  name = df$name, visit_time = df$visit_time_clean, cafe = df$cafe,
  address = df$address,
  district_orig = df$district_num, district = df$district, prague1 = df$prague1,
  cafe_type_orig = df$cafe_type, cafe_type = df$cafe_type_clean,
  payment = df$payment_clean,
  espresso_orig = df$p_espresso, espresso = df$espresso,
  cappuccino_orig = df$p_cappuccino, cappuccino = df$cappuccino,
  flatwhite_orig = df$p_flatwhite, flatwhite = df$flatwhite,
  filter_orig = df$p_filter, filter = df$filter, serves_filter = df$serves_filter,
  price_level = df$price_level, above_med_price = df$above_med_price,
  seats_in = df$seats_in_num, seats_out = df$seats_out_num, outdoor = df$outdoor,
  rating = df$rating_num, n_ratings_orig = df$n_ratings,
  n_ratings = df$n_ratings_num, adj_rating = df$adj_rating,
  above_med_flatwhite = df$above_med_flatwhite, fw_minus_cap = df$fw_minus_cap,
  adj_rating_tenth = df$adj_rating_tenth,
  above_med_adj = df$above_med_adj, above_med_nrat = df$above_med_nrat,
  roastery = df$roastery_num,
  dist_orloj_sheet = as.numeric(df$dist_orloj),
  latitude = df$latitude, longitude = df$longitude,
  ns_km = df$ns_km, ew_km = df$ew_km, abs_ns_km = df$abs_ns_km, abs_ew_km = df$abs_ew_km,
  stringsAsFactors = FALSE
)

# ---------- 7. CHECK THE RESULT ----------
cat("\n=== Final checks ===\n")
cat("Rows: raw file", n_before, "-> cleaned", nrow(clean), "\n")
cat("\nMissing values per column (cleaned):\n"); print(colSums(is.na(clean)))
cat("\nRanges of the variables used:\n")
rng <- c("espresso", "cappuccino", "flatwhite", "filter", "price_level", "seats_in",
         "seats_out", "rating", "n_ratings", "adj_rating", "roastery",
         "ns_km", "ew_km")
print(t(sapply(clean[rng], function(x) range(x, na.rm = TRUE))))
stopifnot(all(clean$rating >= 1 & clean$rating <= 5, na.rm = TRUE),
          all(clean$adj_rating >= 1 & clean$adj_rating <= 5, na.rm = TRUE),
          all(clean$price_level > 0, na.rm = TRUE),
          all(clean$n_ratings >= 0, na.rm = TRUE))
cat("\nSanity check, ew_km/ns_km vs the sheet's own distance (should be close):\n")
print(head(data.frame(sheet = clean$dist_orloj_sheet,
                      mine = sqrt(clean$ns_km^2 + clean$ew_km^2)), 5))

write.csv(clean, outfile, row.names = FALSE)
cat("\nSaved", outfile, "with", nrow(clean), "rows and", ncol(clean), "columns\n")

################################################################################
# TAPE Performance Indicators â Step 2 Core Criteria
# Tool for Agroecology Performance Evaluation (FAO)
#
# Column names match the KoboToolbox XLSForm (ajfT4SY4zcJPVqKCJuuQxH.xlsx).
# All -99 values are treated as "Don't know / Refused to answer".
#
# Functions:
#   compute_fies()              â Food Insecurity Experience Scale
#   compute_dietary_diversity() â Women's Dietary Diversity Score
#   compute_soil_health()       â Soil Health Score
#   compute_economic_index()    â Qualitative Economic Perception Index
#   compute_land_tenure()       â Land Tenure Scores (men / women / household)
#   compute_youth_score()       â Youth Employment & Aspiration Score
#   compute_aweai()             â Abbreviated Women's Empowerment in Ag. Index
#   compute_biodiversity_index()â Gini-Simpson Biodiversity Index
#   compute_tape_performance()  â Wrapper: runs all indicators above
#   tape_performance_summary()  â Summary statistics across all indicators
#   validate_performance_data() â Data validation helper
################################################################################

# ââ Utility ââââââââââââââââââââââââââââââââââââââââââââââââââââââââââââââââââ

.na99 <- function(x) ifelse(is.na(x) | x == -99, NA_real_, as.numeric(x))

.coalesce <- function(...) {
  Reduce(function(a, b) ifelse(!is.na(a), a, b), list(...))
}

.score_dec <- function(x, sex = c("men", "women")) {
  # Decision-making score (dec list: 1=man only, 2=man mainly, 3=woman mainly,
  # 4=woman only, 5=both equally, 6=joint, 88=not applicable)
  sex <- match.arg(sex)
  if (sex == "men") {
    dplyr::case_when(
      x == 1          ~ 0,
      x %in% c(2, 6) ~ 1,
      x == 3          ~ 0.25,
      x %in% c(4, 5) ~ 0.5,
      TRUE            ~ NA_real_
    )
  } else {
    dplyr::case_when(
      x == 1          ~ 0,
      x %in% c(3, 6) ~ 1,
      x == 2          ~ 0.25,
      x %in% c(4, 5) ~ 0.5,
      TRUE            ~ NA_real_
    )
  }
}

# ââ 1. FIES â Food Insecurity Experience Scale ââââââââââââââââââââââââââââââââ
#
# Inputs (binary 0/1, select_one fies):
#   worried, healthy, fewfoods, skipped, ateless, ranout, hungry, wholeday
#
# Outputs added to data:
#   fies_sum          â raw item sum (0â8)
#   fies_secure       â 1 if food secure (sum = 0)
#   fies_mild         â 1 if mildly food insecure
#   fies_moderate     â 1 if moderately food insecure
#   fies_severe       â 1 if severely food insecure (wholeday = 1)
#   fies_cat          â category label
#   fies_score        â 0â100, higher = MORE food secure
################################################################################
compute_fies <- function(data) {
  required_cols <- c("worried", "healthy", "fewfoods", "skipped",
                     "ateless", "ranout", "hungry", "wholeday")
  missing <- setdiff(required_cols, names(data))
  if (length(missing) > 0) {
    stop("compute_fies: missing columns: ", paste(missing, collapse = ", "))
  }

  d <- data
  for (col in required_cols) d[[col]] <- .na99(d[[col]])

  d$fies_sum <- with(d, rowSums(cbind(worried, healthy, fewfoods, skipped,
                                      ateless, ranout, hungry, wholeday),
                                na.rm = FALSE))

  d$fies_secure   <- as.integer(!is.na(d$fies_sum) & d$fies_sum == 0)
  d$fies_mild     <- as.integer(
    !is.na(d$fies_sum) &
    (d$worried + d$healthy + d$fewfoods + d$skipped) > 0 &
    (d$ateless + d$ranout + d$hungry + d$wholeday) == 0
  )
  d$fies_moderate <- as.integer(
    !is.na(d$fies_sum) &
    (d$ateless + d$ranout + d$hungry) > 0 &
    d$wholeday == 0
  )
  d$fies_severe   <- as.integer(!is.na(d$wholeday) & d$wholeday == 1)

  d$fies_cat <- dplyr::case_when(
    d$fies_severe   == 1 ~ "Severely food insecure",
    d$fies_moderate == 1 ~ "Moderately food insecure",
    d$fies_mild     == 1 ~ "Mildly food insecure",
    d$fies_sum      == 0 ~ "Food secure",
    TRUE                  ~ NA_character_
  )

  # Inverted: 100 = fully food secure, 0 = all 8 items answered "yes"
  d$fies_score <- round(100 - ((d$fies_sum / 8) * 100), 2)

  return(d)
}

# ââ 2. Dietary Diversity Score ââââââââââââââââââââââââââââââââââââââââââââââââ
#
# Women's Dietary Diversity (relevant when hh_women > 0 OR hh_fyoung > 0).
# 10 food groups aggregated from sub-items; -99 â 0.
#
# Raw inputs (select_one dietyn, 0/1/-99):
#   grains_A, grains_B (â grains_tmp = max of both)
#   pulses, nuts, eggs, darkgreen, otherveg, otherfruit  (single items)
#   dairy_E, dairy_F   (â dairy_tmp = max of both)
#   meat_H, meat_I, meat_J, meat_K, meat_L (â meat_tmp = max of all)
#   darkyellow_N, darkyellow_O (â darkyellow_tmp = max of both)
#
# Outputs:
#   grains_tmp, pulses_tmp, nuts_tmp, dairy_tmp, meat_tmp,
#   eggs_tmp, darkgreen_tmp, darkyellow_tmp, otherveg_tmp, otherfruit_tmp
#   dietary_score  â 0â100
#   minimum_diet   â "Yes" if dietary_score >= 50
################################################################################
compute_dietary_diversity <- function(data) {
  grains_cols     <- c("grains_A", "grains_B")
  dairy_cols      <- c("dairy_E", "dairy_F")
  meat_cols       <- c("meat_H", "meat_I", "meat_J", "meat_K", "meat_L")
  darkyellow_cols <- c("darkyellow_N", "darkyellow_O")
  single_cols     <- c("pulses", "nuts", "eggs", "darkgreen", "otherveg", "otherfruit")

  d <- data

  # Replace -99 with 0, then take max across a set of columns
  .grp_max <- function(df, cols) {
    avail <- intersect(cols, names(df))
    if (length(avail) == 0) return(rep(NA_real_, nrow(df)))
    mat <- sapply(avail, function(c) ifelse(is.na(df[[c]]) | df[[c]] == -99,
                                            0, as.numeric(df[[c]])))
    if (is.null(dim(mat))) mat <- matrix(mat, ncol = 1)
    apply(mat, 1, max, na.rm = TRUE)
  }

  d$grains_tmp     <- .grp_max(d, grains_cols)
  d$dairy_tmp      <- .grp_max(d, dairy_cols)
  d$meat_tmp       <- .grp_max(d, meat_cols)
  d$darkyellow_tmp <- .grp_max(d, darkyellow_cols)

  for (col in single_cols) {
    new_col      <- paste0(col, "_tmp")
    d[[new_col]] <- ifelse(is.na(d[[col]]) | d[[col]] == -99, 0, as.numeric(d[[col]]))
  }

  score_cols <- c("grains_tmp", "pulses_tmp", "nuts_tmp", "dairy_tmp",
                  "meat_tmp", "eggs_tmp", "darkgreen_tmp",
                  "darkyellow_tmp", "otherveg_tmp", "otherfruit_tmp")

  avail_score <- intersect(score_cols, names(d))
  if (length(avail_score) < 10) {
    warning("compute_dietary_diversity: only ", length(avail_score),
            " of 10 food group columns found; score may be incomplete.")
  }

  mat <- sapply(avail_score, function(c) as.numeric(d[[c]]))
  if (is.null(dim(mat))) mat <- matrix(mat, ncol = 1)
  d$dietary_score <- round((rowSums(mat, na.rm = FALSE) / 10) * 100, 2)
  d$minimum_diet  <- ifelse(!is.na(d$dietary_score) & d$dietary_score >= 50,
                            "Yes", "No")

  return(d)
}

# ââ 3. Soil Health Score ââââââââââââââââââââââââââââââââââââââââââââââââââââââ
#
# 10 items on a 1â5 scale (select_one per item):
#   soilstructure, soilcompaction, soildepth, soilresidues, soilcolor,
#   water_ret, cover, erosion, invertebrates, microbio
#
# Formula: round(((sum / 10) - 1) * 25, 2)  â  0 (all 1s) to 100 (all 5s)
#
# Output: soilhealth_score
################################################################################
compute_soil_health <- function(data) {
  soil_cols <- c("soilstructure", "soilcompaction", "soildepth", "soilresidues",
                 "soilcolor", "water_ret", "cover", "erosion",
                 "invertebrates", "microbio")

  missing <- setdiff(soil_cols, names(data))
  if (length(missing) > 0) {
    stop("compute_soil_health: missing columns: ", paste(missing, collapse = ", "))
  }

  d   <- data
  mat <- sapply(soil_cols, function(c) .na99(d[[c]]))
  if (is.null(dim(mat))) mat <- matrix(mat, ncol = 1)
  soil_sum <- rowSums(mat, na.rm = FALSE)
  d$soilhealth_score <- round(((soil_sum / 10) - 1) * 25, 2)

  return(d)
}

# ââ 4. Qualitative Economic Perception Index ââââââââââââââââââââââââââââââââââ
#
# 6 items (select_one, 0â4 scale; profitable is 1â4; -99 = N/A â excluded):
#   stabincome, livcon, envfuture, profitable, productive, valueadd
#
# Formula: round(((sum_of_valid / n_valid) * 25), 2)
#   Converts 0â4 mean to 0â100.
#
# Output: econ_index
################################################################################
compute_economic_index <- function(data) {
  econ_cols <- c("stabincome", "livcon", "envfuture",
                 "profitable", "productive", "valueadd")

  missing <- setdiff(econ_cols, names(data))
  if (length(missing) > 0) {
    stop("compute_economic_index: missing columns: ", paste(missing, collapse = ", "))
  }

  d       <- data
  mat_raw <- sapply(econ_cols, function(c) as.numeric(d[[c]]))
  if (is.null(dim(mat_raw))) mat_raw <- matrix(mat_raw, ncol = 1)

  mat_tmp <- ifelse(is.na(mat_raw) | mat_raw == -99, 0, mat_raw)
  denom   <- rowSums(!is.na(mat_raw) & mat_raw != -99)

  d$econ_denom <- denom
  d$econ_index <- round((rowSums(mat_tmp) / denom) * 25, 2)
  d$econ_index[denom == 0] <- NA_real_

  return(d)
}

# ââ 5. Land Tenure Scores âââââââââââââââââââââââââââââââââââââââââââââââââââââ
#
# Three components per gender and household:
#
#  A) Legal recognition:
#     recland_men/women (yn: 1=Yes/0=No/-99â0) â formal document exists
#     name_men/women    (yn: 1=Yes/0=No/-99â0) â name on document
#     landt_legal = if(recland=1, if(name=1, 1, 0.5), 0)
#
#  B) Perception of tenure security:
#     ltperc_men/women (select_one ltperc; stored as 1/0.75/0.5/0.25/0;
#                        inverted: 1=not likely, 0=extremely likely; -99 â 0.5)
#
#  C) Land rights (sell / bequeath / inherit):
#     sell/beq/inh â yn, -99 â 0
#     sel_beq_inh = if(sell+beq+inh > 0, 1, 0)
#
#  Score (%) = round(((legal + ltperc + rights) / 3) * 100, 2)
#
# Outputs:
#   landt_legal_men, landt_legal_women, landt_legal_household
#   ltperc_men_tmp, ltperc_women_tmp, max_ltperc
#   sel_beq_inh_men, sel_beq_inh_women, max_sel_beq_inh
#   landtenure_score_men, landtenure_score_women, landtenure_score_household  (0â1)
#   landtenure_score_men_pct, landtenure_score_women_pct,
#   landtenure_score_household_pct  (0â100)
################################################################################
compute_land_tenure <- function(data) {
  required <- c("recland_men", "recland_women", "name_men", "name_women",
                "ltperc_men", "ltperc_women",
                "sell_men", "sell_women", "beq_men", "beq_women",
                "inh_men", "inh_women")
  missing <- setdiff(required, names(data))
  if (length(missing) > 0) {
    stop("compute_land_tenure: missing columns: ", paste(missing, collapse = ", "))
  }

  d <- data

  # ââ Component A: legal recognition âââââââââââââââââââââââââââââââââââââââââ
  d$recland_men_tmp   <- ifelse(is.na(d$recland_men)   | d$recland_men   == -99, 0, as.numeric(d$recland_men))
  d$recland_women_tmp <- ifelse(is.na(d$recland_women) | d$recland_women == -99, 0, as.numeric(d$recland_women))
  d$max_recland       <- pmax(d$recland_men_tmp, d$recland_women_tmp, na.rm = TRUE)

  d$name_men_tmp      <- ifelse(is.na(d$name_men)   | d$name_men   == -99, 0, as.numeric(d$name_men))
  d$name_women_tmp    <- ifelse(is.na(d$name_women) | d$name_women == -99, 0, as.numeric(d$name_women))
  d$max_name_doc      <- pmax(d$name_men_tmp, d$name_women_tmp, na.rm = TRUE)

  d$landt_legal_men       <- ifelse(d$recland_men_tmp   == 1,
                                    ifelse(d$name_men_tmp   == 1, 1, 0.5), 0)
  d$landt_legal_women     <- ifelse(d$recland_women_tmp == 1,
                                    ifelse(d$name_women_tmp == 1, 1, 0.5), 0)
  d$landt_legal_household <- ifelse(d$max_recland == 1,
                                    ifelse(d$max_name_doc == 1, 1, 0.5), 0)

  # ââ Component B: perception (ltperc already stored as 0â1; -99 â 0.5) ââââââ
  d$ltperc_men_tmp   <- ifelse(is.na(d$ltperc_men)   | d$ltperc_men   == -99, 0.5, as.numeric(d$ltperc_men))
  d$ltperc_women_tmp <- ifelse(is.na(d$ltperc_women) | d$ltperc_women == -99, 0.5, as.numeric(d$ltperc_women))
  d$max_ltperc       <- pmax(d$ltperc_men_tmp, d$ltperc_women_tmp, na.rm = TRUE)

  # ââ Component C: rights (sell / bequeath / inherit) âââââââââââââââââââââââââ
  .yn0 <- function(x) ifelse(is.na(x) | x == -99, 0, as.numeric(x))

  d$sell_men_tmp   <- .yn0(d$sell_men)
  d$sell_women_tmp <- .yn0(d$sell_women)
  d$beq_men_tmp    <- .yn0(d$beq_men)
  d$beq_women_tmp  <- .yn0(d$beq_women)
  d$inh_men_tmp    <- .yn0(d$inh_men)
  d$inh_women_tmp  <- .yn0(d$inh_women)

  d$max_sell <- pmax(d$sell_men_tmp, d$sell_women_tmp, na.rm = TRUE)
  d$max_beq  <- pmax(d$beq_men_tmp,  d$beq_women_tmp,  na.rm = TRUE)
  d$max_inh  <- pmax(d$inh_men_tmp,  d$inh_women_tmp,  na.rm = TRUE)

  d$sel_beq_inh_men   <- as.integer((d$sell_men_tmp + d$beq_men_tmp + d$inh_men_tmp) > 0)
  d$sel_beq_inh_women <- as.integer((d$sell_women_tmp + d$beq_women_tmp + d$inh_women_tmp) > 0)
  d$max_sel_beq_inh   <- as.integer((d$max_sell + d$max_beq + d$max_inh) > 0)

  # ââ Scores ââââââââââââââââââââââââââââââââââââââââââââââââââââââââââââââââââ
  d$landtenure_score_men       <- (d$landt_legal_men        + d$ltperc_men_tmp    + d$sel_beq_inh_men)   / 3
  d$landtenure_score_women     <- (d$landt_legal_women     + d$ltperc_women_tmp + d$sel_beq_inh_women) / 3
  d$landtenure_score_household <- (d$landt_legal_household + d$max_ltperc        + d$max_sel_beq_inh)   / 3

  d$landtenure_score_men_pct       <- round(d$landtenure_score_men       * 100, 2)
  d$landtenure_score_women_pct     <- round(d$landtenure_score_women     * 100, 2)
  d$landtenure_score_household_pct <- round(d$landtenure_score_household * 100, 2)

  return(d)
}

# ââ 6. Youth Employment & Aspiration Score ââââââââââââââââââââââââââââââââââââ
#
# Repeat group data is typically exported by KoboToolbox in one of two layouts:
#
#  (a) Pre-aggregated columns already present in the main dataset:
#        youth_score, youth_m_score, youth_f_score, or
#        youth_emp_score_avg + youth_emig_score_avg
#
#  (b) A separate repeat-group table. Use aggregate_youth_repeats() first,
#      then join the result back to the main dataset.
#
# Per-individual scores (computed inside the repeat group):
#   youth_emp_score   = if(y_employ=1, 0.5,
#                          if(y_agprod=1 | y_agprod_outside=1 | y_enrol=1, 1, 0))
#   youth_emig_score  = if(future_farm=1 & keen_emigr=0, 1,
#                          if((farm=0 & emigr=0)|(farm=1 & emigr=1), 0.5,
#                             if(farm=0 & emigr=1, 0, NA)))
#
# Household-level aggregation:
#   youth_emp_m_score_avg  = mean(emp_m_scores) * 100
#   youth_emig_m_score_avg = round(mean(emig_m_scores, na.rm=TRUE) * 100, 2)
#   youth_m_score          = round((emp_m_avg + emig_m_avg) / 2, 2)
#   [identical for females with _f suffix]
#
# Combined (all youth, both genders):
#   youth_emp_score_avg  = (sum_f + sum_m) / (n_f + n_m) * 100
#   youth_emig_score_avg = round((sum_f + sum_m) / (n_f + n_m) * 100, 2)
#   youth_score          = round((emp_avg + emig_avg) / 2, 2)
################################################################################

#' Compute per-individual youth sub-scores
.youth_individual_scores <- function(agprod, agprod_out, employ, enrol,
                                     future_farm, keen_emigr) {
  emp_score <- dplyr::case_when(
    employ == 1                                        ~ 0.5,
    agprod == 1 | agprod_out == 1 | enrol == 1        ~ 1,
    TRUE                                               ~ 0
  )
  emig_score <- dplyr::case_when(
    future_farm == 1 & keen_emigr == 0                ~ 1,
    (future_farm == 0 & keen_emigr == 0) |
    (future_farm == 1 & keen_emigr == 1)              ~ 0.5,
    future_farm == 0 & keen_emigr == 1                ~ 0,
    TRUE                                              ~ NA_real_
  )
  list(emp_score = emp_score, emig_score = emig_score)
}

#' Aggregate youth repeat-group rows to household-level scores.
#'
#' @param youth_data Data frame of individual youth rows. Required columns:
#'   household_id, sex (values "male" or "female"),
#'   y_agprod, y_agprod_outside, y_employ, y_enrol,
#'   y_future_farm, y_keen_emigr
#' @return Data frame (one row per household_id) with columns:
#'   youth_m_score, youth_f_score, youth_score (0â100)
aggregate_youth_repeats <- function(youth_data) {
  if (!requireNamespace("dplyr", quietly = TRUE))
    stop("Package 'dplyr' is required.")

  required <- c("household_id", "sex",
                "y_agprod", "y_agprod_outside", "y_employ", "y_enrol",
                "y_future_farm", "y_keen_emigr")
  missing <- setdiff(required, names(youth_data))
  if (length(missing) > 0)
    stop("aggregate_youth_repeats: missing columns: ", paste(missing, collapse = ", "))

  youth_data <- dplyr::mutate(youth_data,
    sc = .youth_individual_scores(
      .na99(y_agprod), .na99(y_agprod_outside),
      .na99(y_employ), .na99(y_enrol),
      .na99(y_future_farm), .na99(y_keen_emigr)
    ),
    emp_score  = sc$emp_score,
    emig_score = sc$emig_score
  )

  agg_m <- dplyr::summarise(
    dplyr::filter(youth_data, sex == "male"),
    youth_emp_m_score_avg  = ifelse(dplyr::n() > 0,
                                    mean(emp_score,  na.rm = TRUE) * 100, NA_real_),
    youth_emig_m_score_avg = round(mean(emig_score, na.rm = TRUE) * 100, 2),
    youth_m_score          = round((youth_emp_m_score_avg + youth_emig_m_score_avg) / 2, 2),
    n_m                    = dplyr::n(),
    sum_emp_m              = sum(emp_score,  na.rm = TRUE),
    sum_emig_m             = sum(emig_score, na.rm = TRUE),
    .by = household_id
  )

  agg_f <- dplyr::summarise(
    dplyr::filter(youth_data, sex == "female"),
    youth_emp_f_score_avg  = ifelse(dplyr::n() > 0,
                                    mean(emp_score,  na.rm = TRUE) * 100, NA_real_),
    youth_emig_f_score_avg = round(mean(emig_score, na.rm = TRUE) * 100, 2),
    youth_f_score          = round((youth_emp_f_score_avg + youth_emig_f_score_avg) / 2, 2),
    n_f                    = dplyr::n(),
    sum_emp_f              = sum(emp_score,  na.rm = TRUE),
    sum_emig_f             = sum(emig_score, na.rm = TRUE),
    .by = household_id
  )

  combined <- dplyr::full_join(agg_m, agg_f, by = "household_id")
  combined  <- dplyr::mutate(combined,
    dplyr::across(c(n_m, n_f, sum_emp_m, sum_emig_m, sum_emp_f, sum_emig_f),
                  ~ tidyr::replace_na(.x, 0)),
    total_n = n_m + n_f,
    youth_emp_score_avg = dplyr::case_when(
      total_n > 0 ~ (sum_emp_m + sum_emp_f) / total_n * 100,
      TRUE        ~ NA_real_
    ),
    youth_emig_score_avg = dplyr::case_when(
      total_n > 0 ~ round((sum_emig_m + sum_emig_f) / total_n * 100, 2),
      TRUE        ~ NA_real_
    ),
    youth_score = round((youth_emp_score_avg + youth_emig_score_avg) / 2, 2)
  )

  dplyr::select(combined,
    household_id,
    youth_emp_m_score_avg, youth_emig_m_score_avg, youth_m_score,
    youth_emp_f_score_avg, youth_emig_f_score_avg, youth_f_score,
    youth_emp_score_avg, youth_emig_score_avg, youth_score
  )
}

#' Add youth_score to the main household dataset.
#' Detects which pre-computed columns are present and combines accordingly.
compute_youth_score <- function(data) {
  d <- data

  if ("youth_score" %in% names(d)) {
    d$youth_score <- as.numeric(d$youth_score)
    return(d)
  }

  if (all(c("youth_emp_score_avg", "youth_emig_score_avg") %in% names(d))) {
    d$youth_score <- round(
      (as.numeric(d$youth_emp_score_avg) + as.numeric(d$youth_emig_score_avg)) / 2, 2
    )
    return(d)
  }

  if (all(c("youth_m_score", "youth_f_score") %in% names(d))) {
    m <- as.numeric(d$youth_m_score)
    f <- as.numeric(d$youth_f_score)
    hm <- if ("hh_myoung" %in% names(d)) as.numeric(d$hh_myoung) else NA_real_
    hf <- if ("hh_fyoung" %in% names(d)) as.numeric(d$hh_fyoung) else NA_real_
    d$youth_score <- dplyr::case_when(
      !is.na(hm) & !is.na(hf) & hm > 0 & hf > 0 ~ round((m + f) / 2, 2),
      !is.na(hm) & hm > 0                          ~ m,
      !is.na(hf) & hf > 0                          ~ f,
      TRUE                                          ~ NA_real_
    )
    return(d)
  }

  warning("compute_youth_score: no youth columns found. ",
          "Run aggregate_youth_repeats() on the repeat-group export first, ",
          "then join the result to the main dataset.")
  d$youth_score <- NA_real_
  return(d)
}

# ââ 7. A-WEAI âââââââââââââââââââââââââââââââââââââââââââââââââââââââââââââââââ
#
# Abbreviated Women's Empowerment in Agriculture Index (A-WEAI)
# Reference: IFPRI / FAO A-WEAI methodology
#
# Five Domains of Empowerment (5DE) computed for women and men:
#
#  1. Time allocation  (wtime_alloc_*_score)
#     total = wtime_ag + wtime_dom + wtime_otgain (hours/day)
#     score = if(total < 10.5, 1, 0.133 * (18 - total))
#     -99 imputed with defaults: ag_men=6, ag_women=4; dom_men=4, dom_women=6; otgain=2
#
#  2. Asset ownership  (asset_ownership_*_score)
#     = average of (minor-asset score, major-asset score, credit score) and land tenure
#     Decision coding (dec list): 1=man onlyâ0 men / 0 women
#                                  2=man mainlyâ1 men / 0.25 women
#                                  3=woman mainlyâ0.25 men / 1 women
#                                  4=woman onlyâ0.5 men / 0.5 women
#                                  5=both equallyâ0.5 men / 0.5 women
#                                  6=jointâ1 men / 1 women
#                                  88=not applicable
#
#  3. Production decisions  (prod_asset_*_score)
#     = weighted mean of dec_cashcrop / dec_foodcrop / dec_biganim /
#       dec_smallanim / dec_otact / dec_expense scores
#
#  4. Revenue decisions  (dec_rev_*_score)
#     = weighted mean of dec_rev_crop / dec_rev_anim / dec_rev_oth scores
#
#  5. Leadership  (leadership_*)
#     Involvement scale (involv): 1â0, 2â0.25, 3â0.5, 4â0.75, 5â1
#     = max(involv_agri_score, involv_oth_score)
#
#  women_5de   = (wtime + asset + prod + rev + leadership) / 5
#  men_5de     = same with men sub-scores (first respondent)
#  men_5de_001 = same with _001 columns (second male respondent, if present)
#  gpi         = round((women_5de / coalesce(men_5de_001, men_5de)) * 100, 2)
#  aweai       = round(gpi * 0.1 + women_5de * 0.9 * 100, 2)
#
# If pre-computed sub-scores already exist in the data, they are used directly.
################################################################################
compute_aweai <- function(data) {
  d <- data

  if ("aweai" %in% names(d) && !all(is.na(d$aweai))) {
    message("compute_aweai: using pre-computed 'aweai' column.")
    return(d)
  }

  # ââ Leadership involvement scoring âââââââââââââââââââââââââââââââââââââââââ
  .involv_score <- function(x) {
    x <- ifelse(is.na(x) | x == -99, 1L, as.integer(x))
    dplyr::case_when(x == 1 ~ 0, x == 2 ~ 0.25, x == 3 ~ 0.5,
                     x == 4 ~ 0.75, x == 5 ~ 1, TRUE ~ 0)
  }

  # ââ Domain 1: Time allocation âââââââââââââââââââââââââââââââââââââââââââââââ
  if (!all(c("wtime_alloc_women_score", "wtime_alloc_men_score") %in% names(d))) {
    ag_m  <- ifelse(is.na(d$wtime_ag_men)       | d$wtime_ag_men       == -99, 6, as.numeric(d$wtime_ag_men))
    ag_w  <- ifelse(is.na(d$wtime_ag_women)      | d$wtime_ag_women     == -99, 4, as.numeric(d$wtime_ag_women))
    dom_m <- ifelse(is.na(d$wtime_dom_men)       | d$wtime_dom_men      == -99, 4, as.numeric(d$wtime_dom_men))
    dom_w <- ifelse(is.na(d$wtime_dom_women)     | d$wtime_dom_women    == -99, 6, as.numeric(d$wtime_dom_women))
    ot_m  <- ifelse(is.na(d$wtime_otgain_men)    | d$wtime_otgain_men   == -99, 2, as.numeric(d$wtime_otgain_men))
    ot_w  <- ifelse(is.na(d$wtime_otgain_women)  | d$wtime_otgain_women == -99, 2, as.numeric(d$wtime_otgain_women))

    tot_m <- ag_m + dom_m + ot_m
    tot_w <- ag_w + dom_w + ot_w
    d$wtime_alloc_men_score   <- ifelse(tot_m < 10.5, 1, 0.133 * (18 - tot_m))
    d$wtime_alloc_women_score <- ifelse(tot_w < 10.5, 1, 0.133 * (18 - tot_w))
  }

  # ââ Domain 2: Asset ownership âââââââââââââââââââââââââââââââââââââââââââââââ
  if (!all(c("asset_ownership_women_score", "asset_ownership_men_score") %in% names(d))) {

    raise_animals    <- if ("raise_animals"    %in% names(d)) as.numeric(d$raise_animals)    else 1
    Access_to_credit <- if ("Access_to_credit" %in% names(d)) as.numeric(d$Access_to_credit) else 1

    .minor <- function(ow_crop_r, ow_anim_r, ow_ot_r, sex) {
      fn  <- function(x) .score_dec(x, sex)
      def <- if (sex == "men") 3 else 3
      ow_crop <- ifelse(is.na(ow_crop_r) | ow_crop_r == -99, def, ow_crop_r)
      ow_anim <- ifelse(is.na(ow_anim_r) | ow_anim_r == -99, 2,   ow_anim_r)
      ow_ot   <- ifelse(is.na(ow_ot_r)   | ow_ot_r   == -99, def, ow_ot_r)

      s_crop <- ifelse(ow_crop != 88, fn(ow_crop), 0)
      s_anim <- ifelse(ow_anim == 88 | raise_animals == 0, 0, fn(ow_anim))
      s_ot   <- ifelse(ow_ot   != 88, fn(ow_ot), 0)
      n_crop <- ifelse(ow_crop != 88, 1, 0)
      n_anim <- ifelse(ow_anim == 88 | raise_animals == 0, 0, 1)
      n_ot   <- ifelse(ow_ot   != 88, 1, 0)
      (s_crop + s_anim + s_ot) / pmax(n_crop + n_anim + n_ot, 1)
    }

    .major <- function(ow_house_r, sex) {
      fn       <- function(x) .score_dec(x, sex)
      ow_house <- ifelse(is.na(ow_house_r) | ow_house_r == -99, 2, ow_house_r)
      ifelse(ow_house == 1 | ow_house == 88, 0, fn(ow_house))
    }

    .credit <- function(cb_r, db_r, ub_r, sex) {
      fn <- function(x) .score_dec(x, sex)
      cb  <- ifelse(is.na(cb_r) | cb_r == -99, 2, cb_r)
      db  <- ifelse(is.na(db_r) | db_r == -99, 6, db_r)
      ub  <- ifelse(is.na(ub_r) | ub_r == -99, 6, ub_r)

      s_cb <- dplyr::case_when(
        cb == 1                        ~ 0.33,
        cb == 2                        ~ 1,
        cb == 4 | Access_to_credit == 0 ~ 0,
        TRUE                           ~ NA_real_
      )
      s_db <- ifelse(db == 1 | Access_to_credit == 0, 0, fn(db))
      s_ub <- ifelse(ub == 1 | Access_to_credit == 0, 0, fn(ub))

      n_cb <- ifelse(!is.na(cb_r) & cb_r != 88, 1, 0)
      n_db <- ifelse(!is.na(db_r) & db_r != 88, 1, 0)
      n_ub <- ifelse(!is.na(ub_r) & ub_r != 88, 1, 0)

      (ifelse(n_cb==1, s_cb, 0) + ifelse(n_db==1, s_db, 0) + ifelse(n_ub==1, s_ub, 0)) /
        pmax(n_cb + n_db + n_ub, 1)
    }

    lt_men   <- if ("landtenure_score_men"   %in% names(d)) d$landtenure_score_men   else 0.5
    lt_women <- if ("landtenure_score_women" %in% names(d)) d$landtenure_score_women else 0.5

    # First respondent
    mi_m  <- .minor(d$owcrop, d$owanim, d$owotact, "men")
    mi_w  <- .minor(d$owcrop, d$owanim, d$owotact, "women")
    ma_m  <- .major(d$owhouse, "men")
    ma_w  <- .major(d$owhouse, "women")
    cr_m  <- .credit(d$credit_canborrow, d$credit_decborrow, d$credit_useborrow, "men")
    cr_w  <- .credit(d$credit_canborrow, d$credit_decborrow, d$credit_useborrow, "women")

    d$asset_ownership_men_score   <- ((mi_m + ma_m) / 2 + cr_m + lt_men)   / 3
    d$asset_ownership_women_score <- ((mi_w + ma_w) / 2 + cr_w + lt_women) / 3

    # Second respondent (_001)
    if (all(c("owcrop_001", "owanim_001", "owhouse_001", "owotact_001") %in% names(d))) {
      mi_m1 <- .minor(d$owcrop_001, d$owanim_001, d$owotact_001, "men")
      mi_w1 <- .minor(d$owcrop_001, d$owanim_001, d$owotact_001, "women")
      ma_m1 <- .major(d$owhouse_001, "men")
      ma_w1 <- .major(d$owhouse_001, "women")
      cb1   <- if ("credit_canborrow_001" %in% names(d)) d$credit_canborrow_001 else NA
      db1   <- if ("credit_decborrow_001" %in% names(d)) d$credit_decborrow_001 else NA
      ub1   <- if ("credit_useborrow_001" %in% names(d)) d$credit_useborrow_001 else NA
      cr_m1 <- .credit(cb1, db1, ub1, "men")
      cr_w1 <- .credit(cb1, db1, ub1, "women")

      d$asset_ownership_men_score_001   <- ((mi_m1 + ma_m1) / 2 + cr_m1 + lt_men)   / 3
      d$asset_ownership_women_score_001 <- ((mi_w1 + ma_w1) / 2 + cr_w1 + lt_women) / 3
    }
  }

  # ââ Domain 3: Production decisions âââââââââââââââââââââââââââââââââââââââââ
  if (!all(c("prod_asset_women_score", "prod_asset_men_score") %in% names(d))) {
    raise_animals <- if ("raise_animals" %in% names(d)) as.numeric(d$raise_animals) else 1

    .prod <- function(cc, fc, ba, sa, ot, ex, sex) {
      fn  <- function(x) .score_dec(x, sex)
      s   <- c(ifelse(!is.na(cc) & cc != 88, fn(cc), 0),
               ifelse(!is.na(fc) & fc != 88, fn(fc), 0),
               ifelse(is.na(ba) | ba == 88 | raise_animals == 0, 0, fn(ba)),
               ifelse(is.na(sa) | sa == 88 | raise_animals == 0, 0, fn(sa)),
               ifelse(!is.na(ot) & ot != 88, fn(ot), 0),
               ifelse(!is.na(ex) & ex != 88, fn(ex), 0))
      n   <- c(ifelse(!is.na(cc) & cc != 88, 1, 0),
               ifelse(!is.na(fc) & fc != 88, 1, 0),
               ifelse(is.na(ba) | ba == 88 | raise_animals == 0, 0, 1),
               ifelse(is.na(sa) | sa == 88 | raise_animals == 0, 0, 1),
               ifelse(!is.na(ot) & ot != 88, 1, 0),
               ifelse(!is.na(ex) & ex != 88, 1, 0))
      # vectorised over rows
      rowSums(do.call(cbind, s)) / pmax(rowSums(do.call(cbind, n)), 1)
    }

    # Vectorised helper for a single decision column
    .vdec <- function(col, sex) {
      x <- if (col %in% names(d)) as.numeric(d[[col]]) else NA_real_
      .score_dec(x, sex)
    }

    .prod_v <- function(suffix, sex) {
      sfx <- if (nchar(suffix) > 0) paste0("_", suffix) else ""
      cc <- if (paste0("dec_cashcrop", sfx) %in% names(d)) as.numeric(d[[paste0("dec_cashcrop", sfx)]]) else NA_real_
      fc <- if (paste0("dec_foodcrop", sfx) %in% names(d)) as.numeric(d[[paste0("dec_foodcrop", sfx)]]) else NA_real_
      ba <- if (paste0("dec_biganim",  sfx) %in% names(d)) as.numeric(d[[paste0("dec_biganim",  sfx)]]) else NA_real_
      sa <- if (paste0("dec_smallanim",sfx) %in% names(d)) as.numeric(d[[paste0("dec_smallanim",sfx)]]) else NA_real_
      ot <- if (paste0("dec_otact",    sfx) %in% names(d)) as.numeric(d[[paste0("dec_otact",    sfx)]]) else NA_real_
      ex <- if (paste0("dec_expense",  sfx) %in% names(d)) as.numeric(d[[paste0("dec_expense",  sfx)]]) else NA_real_
      ra <- if ("raise_animals" %in% names(d)) as.numeric(d$raise_animals) else 1
      fn <- function(x) .score_dec(x, sex)

      s_cc <- ifelse(!is.na(cc) & cc != 88, fn(cc), 0)
      s_fc <- ifelse(!is.na(fc) & fc != 88, fn(fc), 0)
      s_ba <- ifelse(is.na(ba) | ba == 88 | ra == 0, 0, fn(ba))
      s_sa <- ifelse(is.na(sa) | sa == 88 | ra == 0, 0, fn(sa))
      s_ot <- ifelse(!is.na(ot) & ot != 88, fn(ot), 0)
      s_ex <- ifelse(!is.na(ex) & ex != 88, fn(ex), 0)

      n_cc <- as.numeric(!is.na(cc) & cc != 88)
      n_fc <- as.numeric(!is.na(fc) & fc != 88)
      n_ba <- ifelse(is.na(ba) | ba == 88 | ra == 0, 0, 1)
      n_sa <- ifelse(is.na(sa) | sa == 88 | ra == 0, 0, 1)
      n_ot <- as.numeric(!is.na(ot) & ot != 88)
      n_ex <- as.numeric(!is.na(ex) & ex != 88)

      (s_cc+s_fc+s_ba+s_sa+s_ot+s_ex) / pmax(n_cc+n_fc+n_ba+n_sa+n_ot+n_ex, 1)
    }

    d$prod_asset_men_score   <- .prod_v("", "men")
    d$prod_asset_women_score <- .prod_v("", "women")

    if ("dec_cashcrop_001" %in% names(d)) {
      d$prod_asset_men_score_001   <- .prod_v("001", "men")
      d$prod_asset_women_score_001 <- .prod_v("001", "women")
    }
  }

  # ââ Domain 4: Revenue decisions âââââââââââââââââââââââââââââââââââââââââââââ
  if (!all(c("dec_rev_women_score", "dec_rev_men_score") %in% names(d))) {
    raise_animals <- if ("raise_animals" %in% names(d)) as.numeric(d$raise_animals) else 1

    .rev_v <- function(suffix, sex) {
      sfx <- if (nchar(suffix) > 0) paste0("_", suffix) else ""
      rc <- if (paste0("dec_rev_crop", sfx) %in% names(d)) as.numeric(d[[paste0("dec_rev_crop", sfx)]]) else NA_real_
      ra <- if (paste0("dec_rev_anim", sfx) %in% names(d)) as.numeric(d[[paste0("dec_rev_anim", sfx)]]) else NA_real_
      ro <- if (paste0("dec_rev_oth",  sfx) %in% names(d)) as.numeric(d[[paste0("dec_rev_oth",  sfx)]]) else NA_real_
      fn <- function(x) .score_dec(x, sex)
      ras <- if ("raise_animals" %in% names(d)) as.numeric(d$raise_animals) else 1

      s_rc <- ifelse(!is.na(rc) & rc != 88, fn(rc), 0)
      s_ra <- ifelse(is.na(ra) | ra == 88 | ras == 0, 0, fn(ra))
      s_ro <- ifelse(!is.na(ro) & ro != 88, fn(ro), 0)

      n_rc <- as.numeric(!is.na(rc) & rc != 88)
      n_ra <- ifelse(is.na(ra) | ra == 88 | ras == 0, 0, 1)
      n_ro <- as.numeric(!is.na(ro) & ro != 88)

      (s_rc + s_ra + s_ro) / pmax(n_rc + n_ra + n_ro, 1)
    }

    d$dec_rev_men_score   <- .rev_v("", "men")
    d$dec_rev_women_score <- .rev_v("", "women")
    if ("dec_rev_crop_001" %in% names(d)) {
      d$dec_rev_men_score_001   <- .rev_v("001", "men")
      d$dec_rev_women_score_001 <- .rev_v("001", "women")
    }
  }

  # ââ Domain 5: Leadership ââââââââââââââââââââââââââââââââââââââââââââââââââââ
  if (!all(c("leadership_women", "leadership_men") %in% names(d))) {
    d$involv_agri_men_score   <- .involv_score(d$involv_agri_men)
    d$involv_agri_women_score <- .involv_score(d$involv_agri_women)
    d$involv_oth_men_score    <- .involv_score(d$involv_oth_men)
    d$involv_oth_women_score  <- .involv_score(d$involv_oth_women)

    d$leadership_men   <- pmax(d$involv_agri_men_score,   d$involv_oth_men_score,   na.rm = TRUE)
    d$leadership_women <- pmax(d$involv_agri_women_score, d$involv_oth_women_score, na.rm = TRUE)
  }

  # ââ Aggregate 5DE âââââââââââââââââââââââââââââââââââââââââââââââââââââââââââ
  aso_m1 <- if ("asset_ownership_men_score_001"   %in% names(d)) d$asset_ownership_men_score_001   else NA_real_
  pa_m1  <- if ("prod_asset_men_score_001"        %in% names(d)) d$prod_asset_men_score_001        else NA_real_
  dr_m1  <- if ("dec_rev_men_score_001"           %in% names(d)) d$dec_rev_men_score_001           else NA_real_

  d$women_5de <- (d$wtime_alloc_women_score +
                  d$asset_ownership_women_score +
                  d$prod_asset_women_score +
                  d$dec_rev_women_score +
                  d$leadership_women) / 5

  d$men_5de <- (d$wtime_alloc_men_score +
                d$asset_ownership_men_score +
                d$prod_asset_men_score +
                d$dec_rev_men_score +
                d$leadership_men) / 5

  has_001 <- !is.na(aso_m1) & !is.na(pa_m1) & !is.na(dr_m1)
  d$men_5de_001 <- ifelse(
    has_001,
    (d$wtime_alloc_men_score + aso_m1 + pa_m1 + dr_m1 + d$leadership_men) / 5,
    NA_real_
  )

  men_denom <- .coalesce(d$men_5de_001, d$men_5de)
  d$gpi   <- round((d$women_5de / men_denom) * 100, 2)
  d$aweai <- round(d$gpi * 0.1 + d$women_5de * 0.9 * 100, 2)

  return(d)
}

# ââ 8. Biodiversity Index (Gini-Simpson) âââââââââââââââââââââââââââââââââââââ
#
# Crops (from the 'crops' and 'varieties' repeat groups):
#   sum_pi       = Î£ (variety_area_haÂ²) across all varieties of all crops
#   bio_area_sum = Î£ (bio_crop_area_ha) across all crops
#   simpson      = sum_pi / (bio_area_sumÂ²)     [Simpson's dominance index D]
#   GSIndex_crops = round(100 * (1 - simpson), 2)  [Gini-Simpson = 1 - D]
#
# Livestock (from the 'animals_001' repeat group):
#   sum_pi_animals  = Î£ (animal_count_tluÂ²) per species
#   total_tlu       = Î£ (animal_count_tlu)
#   simpson_animals = sum_pi_animals / (total_tluÂ²)
#   GSIndex_animals = round(100 * (1 - simpson_animals), 2)
#
# Overall:
#   GSI_overall = GSIndex_crops   (if no livestock)
#              OR round((GSIndex_crops + GSIndex_animals) / 2, 2)
#
# Usage A â pre-computed KoboToolbox export columns in main dataset:
#   Requires: sum_pi, bio_area_sum   (+ optionally sum_pi_animals, total_tlu)
#
# Usage B â from repeat-group data frames:
#   compute_biodiversity_index(data,
#     crop_data   = df with cols: household_id, variety_area_ha
#     animal_data = df with cols: household_id, animal_count_tlu)
################################################################################
compute_biodiversity_index <- function(data, crop_data = NULL, animal_data = NULL) {
  d <- data

  # Option A: summary columns already exported
  if (all(c("sum_pi", "bio_area_sum") %in% names(d))) {
    d$simpson       <- d$sum_pi / (d$bio_area_sum * d$bio_area_sum)
    d$GSIndex_crops <- round(100 * (1 - d$simpson), 2)

    if (all(c("sum_pi_animals", "total_tlu") %in% names(d))) {
      d$simpson_animals <- d$sum_pi_animals / (d$total_tlu * d$total_tlu)
      d$GSIndex_animals <- round(100 * (1 - d$simpson_animals), 2)

      # animal_species == "-97" means "no animals" in the form
      no_animals <- if ("animal_species" %in% names(d))
        grepl("-97", as.character(d$animal_species)) else FALSE

      d$GSI_overall <- dplyr::case_when(
        no_animals              ~ d$GSIndex_crops,
        !is.na(d$GSIndex_animals) ~
          round((d$GSIndex_crops + d$GSIndex_animals) / 2, 2),
        TRUE                    ~ d$GSIndex_crops
      )
    } else {
      d$GSI_overall <- d$GSIndex_crops
    }
    return(d)
  }

  # Option B: compute from repeat-group exports
  if (!is.null(crop_data)) {
    if (!requireNamespace("dplyr", quietly = TRUE))
      stop("Package 'dplyr' required.")
    crop_agg <- dplyr::summarise(crop_data,
      sum_pi       = sum(variety_area_ha^2, na.rm = TRUE),
      bio_area_sum = sum(variety_area_ha,   na.rm = TRUE),
      .by = household_id)
    d <- dplyr::left_join(d, crop_agg, by = "household_id")
    d$simpson       <- d$sum_pi / (d$bio_area_sum * d$bio_area_sum)
    d$GSIndex_crops <- round(100 * (1 - d$simpson), 2)
  } else {
    warning("compute_biodiversity_index: 'sum_pi'/'bio_area_sum' not found and ",
            "'crop_data' not provided. Biodiversity index not computed.")
    d$GSIndex_crops <- NA_real_
    d$GSI_overall   <- NA_real_
    return(d)
  }

  if (!is.null(animal_data)) {
    anim_agg <- dplyr::summarise(animal_data,
      sum_pi_animals = sum(animal_count_tlu^2, na.rm = TRUE),
      total_tlu      = sum(animal_count_tlu,   na.rm = TRUE),
      .by = household_id)
    d <- dplyr::left_join(d, anim_agg, by = "household_id")
    d$simpson_animals <- d$sum_pi_animals / (d$total_tlu * d$total_tlu)
    d$GSIndex_animals <- round(100 * (1 - d$simpson_animals), 2)
    d$GSI_overall     <- round((d$GSIndex_crops + d$GSIndex_animals) / 2, 2)
  } else {
    d$GSI_overall <- d$GSIndex_crops
  }

  return(d)
}

# ââ 9. Master Wrapper âââââââââââââââââââââââââââââââââââââââââââââââââââââââââ
#
# Runs all performance indicators in sequence.
# Errors in individual indicators produce a warning instead of stopping the
# whole pipeline; set skip_* = TRUE to intentionally bypass an indicator.
################################################################################
compute_tape_performance <- function(
    data,
    crop_data         = NULL,
    animal_data       = NULL,
    skip_fies         = FALSE,
    skip_dietary      = FALSE,
    skip_soil         = FALSE,
    skip_economic     = FALSE,
    skip_land_tenure  = FALSE,
    skip_youth        = FALSE,
    skip_aweai        = FALSE,
    skip_biodiversity = FALSE
) {
  d <- data

  run <- function(fn, label, ...) {
    tryCatch(fn(d, ...), error = function(e) {
      warning(label, " skipped: ", conditionMessage(e))
      d
    })
  }

  if (!skip_fies)         d <- run(compute_fies,              "FIES")
  if (!skip_dietary)      d <- run(compute_dietary_diversity,  "Dietary diversity")
  if (!skip_soil)         d <- run(compute_soil_health,        "Soil health")
  if (!skip_economic)     d <- run(compute_economic_index,     "Economic index")
  if (!skip_land_tenure)  d <- run(compute_land_tenure,        "Land tenure")
  if (!skip_youth)        d <- run(compute_youth_score,        "Youth score")
  if (!skip_aweai)        d <- run(compute_aweai,              "AWEAI")
  if (!skip_biodiversity) {
    d <- tryCatch(
      compute_biodiversity_index(d, crop_data, animal_data),
      error = function(e) {
        warning("Biodiversity index skipped: ", conditionMessage(e))
        d
      }
    )
  }

  return(d)
}

# ââ 10. Summary Statistics âââââââââââââââââââââââââââââââââââââââââââââââââââ
tape_performance_summary <- function(data) {
  score_cols <- intersect(
    c("fies_score",
      "dietary_score",
      "soilhealth_score",
      "econ_index",
      "landtenure_score_men_pct",
      "landtenure_score_women_pct",
      "landtenure_score_household_pct",
      "youth_score",
      "aweai",
      "gpi",
      "GSIndex_crops",
      "GSIndex_animals",
      "GSI_overall"),
    names(data)
  )

  if (length(score_cols) == 0)
    stop("No performance score columns found. Run compute_tape_performance() first.")

  do.call(rbind, lapply(score_cols, function(col) {
    x   <- as.numeric(data[[col]])
    xok <- x[!is.na(x)]
    data.frame(
      indicator   = col,
      n           = length(xok),
      mean        = round(mean(xok),   2),
      sd          = round(sd(xok),     2),
      min         = round(min(xok),    2),
      max         = round(max(xok),    2),
      pct_missing = round(mean(is.na(x)) * 100, 1),
      stringsAsFactors = FALSE
    )
  }))
}

# ââ 11. Validation âââââââââââââââââââââââââââââââââââââââââââââââââââââââââââ
validate_performance_data <- function(data) {
  issues <- list()

  .miss <- function(cols, group) {
    m <- setdiff(cols, names(data))
    if (length(m) > 0)
      issues[[length(issues) + 1]] <<- data.frame(
        group = group, type = "missing_column",
        detail = paste(m, collapse = ", "), stringsAsFactors = FALSE)
  }

  .range <- function(col, lo, hi, group) {
    if (!col %in% names(data)) return()
    x   <- .na99(data[[col]])
    bad <- !is.na(x) & (x < lo | x > hi)
    if (any(bad))
      issues[[length(issues) + 1]] <<- data.frame(
        group  = group,
        type   = paste0("out_of_range [", lo, ",", hi, "]"),
        detail = paste0(col, ": ", sum(bad), " value(s) outside range"),
        stringsAsFactors = FALSE)
  }

  .miss(c("worried","healthy","fewfoods","skipped",
          "ateless","ranout","hungry","wholeday"), "FIES")
  for (col in c("worried","healthy","fewfoods","skipped",
                "ateless","ranout","hungry","wholeday"))
    .range(col, 0, 1, "FIES")

  .miss(c("soilstructure","soilcompaction","soildepth","soilresidues",
          "soilcolor","water_ret","cover","erosion",
          "invertebrates","microbio"), "Soil health")
  for (col in c("soilstructure","soilcompaction","soildepth","soilresidues",
                "soilcolor","water_ret","cover","erosion",
                "invertebrates","microbio"))
    .range(col, 1, 5, "Soil health")

  .miss(c("stabincome","livcon","envfuture",
          "profitable","productive","valueadd"), "Economic index")
  for (col in c("stabincome","livcon","envfuture","productive","valueadd"))
    .range(col, 0, 4, "Economic index")
  .range("profitable", 1, 4, "Economic index")

  .miss(c("recland_men","recland_women","name_men","name_women",
          "ltperc_men","ltperc_women",
          "sell_men","sell_women","beq_men","beq_women",
          "inh_men","inh_women"), "Land tenure")

  .miss(c("wtime_ag_men","wtime_ag_women",
          "wtime_dom_men","wtime_dom_women",
          "wtime_otgain_men","wtime_otgain_women"), "AWEAI â time allocation")

  if (length(issues) == 0) {
    message("validate_performance_data: all checks passed.")
    return(invisible(data.frame()))
  }
  result <- do.call(rbind, issues)
  message("validate_performance_data: ", nrow(result), " issue(s) found.")
  result
}

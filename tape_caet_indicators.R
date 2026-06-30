# =============================================================================
# TAPE CAET Step 2 Indicators Calculator
# =============================================================================
# Tool Assessment for Agroecology Performance Evaluation (TAPE) - FAO
# Computes all Step 2 CAET sub-indicators, principle scores, and CAET total
# from KoboToolbox survey data exported as a flat CSV/data frame.
#
# Author: Generated from TAPE KoboToolbox XLSForm v2025
# Reference: FAO TAPE Manual (agroecology.fao.org)
# =============================================================================
# USAGE:
#   source("tape_caet_indicators.R")
#   results <- compute_caet(df)       # df = exported KoboCollect data frame
#   write.csv(results, "caet_output.csv", row.names = FALSE)
# =============================================================================

library(dplyr)
library(tidyr)

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

#' Safe numeric coercion (returns NA for blank / non-numeric)
as_num <- function(x) suppressWarnings(as.numeric(as.character(x)))

#' Round to 2 decimal places (matches XLSForm round(..., 2))
r2 <- function(x) round(x, 2)

#' Coerce a 0/0.5/1/1.5/2 ordinal score column to numeric
score_col <- function(df, col) {
  if (col %in% names(df)) as_num(df[[col]]) else NA_real_
}

#' Check if a field is non-missing (mirrors XLSForm != "")
has_val <- function(x) !is.na(x)

# =============================================================================
# MAIN FUNCTION
# =============================================================================

#' Compute all TAPE CAET Step 2 indicators
#'
#' @param df  Data frame: one row per farm, column names matching KoboToolbox
#'            variable names (the `name` column of the XLSForm survey sheet).
#'            Ordinal response columns should hold the numeric score values
#'            (0, 0.5, 1, 1.5, 2) — as exported by KoboCollect or after
#'            applying the choice numeric mapping.
#'
#' @return    The input data frame augmented with all intermediate sums,
#'            sub-indicator scores (0–100), principle scores (0–100),
#'            and the CAET total score (0–100).
#'
#' @details
#'   All calculations reproduce the XLSForm `calculation` expressions exactly,
#'   including conditional logic for livestock-optional items and hired-labour
#'   items.  Skip conditions (`relevant`) are respected: when the gating
#'   variable indicates "No", the dependent score is forced to 0 (rather
#'   than NA) so aggregate averages remain comparable across farms.
#'
compute_caet <- function(df) {

  df <- as.data.frame(df)

  # ------------------------------------------------------------------
  # 0. PREREQUISITE: ensure key gating columns exist
  # ------------------------------------------------------------------
  required_gates <- c("raise_animals", "extworker")
  for (col in required_gates) {
    if (!col %in% names(df)) {
      warning(sprintf("Column '%s' not found; defaulting to 0.", col))
      df[[col]] <- 0
    }
  }

  # Gate flags (numeric; yesno list: 1 = Yes, 0 = No)
  raises_animals  <- as_num(df[["raise_animals"]]) == 1   # logical vector
  has_extworker   <- as_num(df[["extworker"]]) == 1

  # ------------------------------------------------------------------
  # HELPER: grab a scored input, zero-out when relevant condition fails
  # ------------------------------------------------------------------
  get <- function(col, gate = NULL) {
    v <- score_col(df, col)
    if (!is.null(gate)) v <- ifelse(gate, v, 0)
    v
  }

  # ==================================================================
  # PRINCIPLE 1: DIVERSITY
  # ==================================================================

  # --- 1a. Plant Diversity (plant_diversity) ---
  # Inputs: num_plant [0–2], plant_gendiv [0–2]
  # sum_plant_diversity = num_plant + plant_gendiv           (max = 4)
  # plant_diversity     = round((sum / 4) * 100, 2)

  num_plant   <- get("num_plant")
  plant_gendiv <- get("plant_gendiv")

  sum_plant_diversity <- num_plant + plant_gendiv
  plant_diversity     <- r2((sum_plant_diversity / 4) * 100)

  # --- 1b. Temporal & Spatial Diversity (temp_spatial_div) ---
  # Inputs: temp_div [0–2], space_div [0–2]
  # sum_temp_spatial_div = temp_div + space_div              (max = 4)
  # temp_spatial_div     = round((sum / 4) * 100, 2)

  temp_div  <- get("temp_div")
  space_div <- get("space_div")

  sum_temp_spatial_div <- temp_div + space_div
  temp_spatial_div     <- r2((sum_temp_spatial_div / 4) * 100)

  # --- 1c. Animal Diversity (anim_diversity) ---
  # SKIP: only relevant when raise_animals = 1
  # Inputs: num_animal [0–2], anim_gendiv [0–2]
  # sum_anim_diversity = num_animal + anim_gendiv            (max = 4)
  # anim_diversity     = round((sum / 4) * 100, 2)
  # When raise_animals = 0, anim_diversity is excluded from div_score avg

  num_animal   <- get("num_animal",  gate = raises_animals)
  anim_gendiv  <- get("anim_gendiv", gate = raises_animals)

  sum_anim_diversity <- ifelse(raises_animals,
                               num_animal + anim_gendiv,
                               NA_real_)
  anim_diversity     <- ifelse(raises_animals,
                               r2((sum_anim_diversity / 4) * 100),
                               NA_real_)

  # --- div_score: average of 2 or 3 sub-indicators ---
  # XLSForm:
  #   if (anim_diversity != "")
  #     round((plant + temp + anim) / 3, 2)
  #   else
  #     round((plant + temp + 0) / 3, 2)
  #
  # Note: when no animals, anim term contributes 0, denominator still 3.

  div_score <- ifelse(
    raises_animals,
    r2((plant_diversity + temp_spatial_div + anim_diversity) / 3),
    r2((plant_diversity + temp_spatial_div + 0)               / 3)
  )

  # ==================================================================
  # PRINCIPLE 2: SYNERGIES
  # ==================================================================

  # --- 2a. Crop–Livestock–Aquaculture Integration (crop_liv_aqua) ---
  # SKIP: only relevant when raise_animals = 1
  # Inputs: feed_prod [0–2], vary_service [0–2]             (max = 4)

  feed_prod    <- get("feed_prod",    gate = raises_animals)
  vary_service <- get("vary_service", gate = raises_animals)

  sum_crop_liv_aqua <- ifelse(raises_animals,
                              feed_prod + vary_service,
                              NA_real_)
  crop_liv_aqua     <- ifelse(raises_animals,
                              r2((sum_crop_liv_aqua / 4) * 100),
                              NA_real_)

  # --- 2b. Integration of Trees (integ_trees) ---
  # Inputs: tree_farm [0–2], tree_divspe [0–2]              (max = 4)

  tree_farm   <- get("tree_farm")
  tree_divspe <- get("tree_divspe")

  integ_trees <- r2(((tree_farm + tree_divspe) / 4) * 100)

  # --- 2c. Connectivity with Natural Ecosystems (connect_ag) ---
  # Inputs: plot_mosaic [0–2], natveg_pct [0–2]             (max = 4)

  plot_mosaic <- get("plot_mosaic")
  natveg_pct  <- get("natveg_pct")

  connect_ag <- r2(((plot_mosaic + natveg_pct) / 4) * 100)

  # --- synergy_score ---
  # XLSForm:
  #   if (raise_animals = 1)
  #     round((crop_liv_aqua + integ_trees + connect_ag) / 3, 2)
  #   else
  #     round((0 + integ_trees + connect_ag) / 3, 2)

  synergy_score <- ifelse(
    raises_animals,
    r2((crop_liv_aqua + integ_trees + connect_ag) / 3),
    r2((0             + integ_trees + connect_ag) / 3)
  )

  # ==================================================================
  # PRINCIPLE 3: RECYCLING
  # ==================================================================

  # --- 3a. Seeds & Breeds (seed_breed) ---
  # Inputs: useloc_seedbreed [0–2], nat_seedbreed [0–2]     (max = 4)

  useloc_seedbreed <- get("useloc_seedbreed")
  nat_seedbreed    <- get("nat_seedbreed")

  sum_seed_breed <- useloc_seedbreed + nat_seedbreed
  seed_breed     <- r2((sum_seed_breed / 4) * 100)

  # --- 3b. Recycling of Biomass (recycling_biomass) ---
  # Inputs: reuse_orgres [0–2], nonorg_waste [0–2]          (max = 4)

  reuse_orgres <- get("reuse_orgres")
  nonorg_waste <- get("nonorg_waste")

  sum_recycling_biomass <- reuse_orgres + nonorg_waste
  recycling_biomass     <- r2((sum_recycling_biomass / 4) * 100)

  # --- 3c. Water & Energy Sources (water_energy) ---
  # Inputs: source_water [0–2], source_energy [0–2]         (max = 4)

  source_water  <- get("source_water")
  source_energy <- get("source_energy")

  sum_water_energy <- source_water + source_energy
  water_energy     <- r2((sum_water_energy / 4) * 100)

  # --- recycling_score ---
  recycling_score <- r2((seed_breed + recycling_biomass + water_energy) / 3)

  # ==================================================================
  # PRINCIPLE 4: EFFICIENCY
  # ==================================================================

  # --- 4a. Soil Fertility Management (mgt_soilfert) ---
  # Inputs: onfarm_nutcycle [0–2], nitrogen_plant [0–2]     (max = 4)

  onfarm_nutcycle <- get("onfarm_nutcycle")
  nitrogen_plant  <- get("nitrogen_plant")

  sum_mgt_soilfert <- onfarm_nutcycle + nitrogen_plant
  mgt_soilfert     <- r2((sum_mgt_soilfert / 4) * 100)

  # --- 4b. Pest & Disease Management (mgt_pestdis) ---
  # Inputs: onf_eco [0–2] always; use_vet [0–2] only when raise_animals = 1
  # XLSForm:
  #   sum = if (use_vet != "") onf_eco + use_vet else onf_eco
  #   score = if (use_vet != "")
  #             round((sum / 4) * 100, 2)   <- denominator 4 when both
  #           else
  #             round((sum / 2) * 100, 2)   <- denominator 2 when only onf_eco

  onf_eco <- get("onf_eco")
  use_vet <- get("use_vet", gate = raises_animals)

  sum_mgt_pestdis <- ifelse(raises_animals,
                            onf_eco + use_vet,
                            onf_eco)
  mgt_pestdis     <- ifelse(raises_animals,
                            r2((sum_mgt_pestdis / 4) * 100),
                            r2((sum_mgt_pestdis / 2) * 100))

  # --- 4c. Water & Energy USE Efficiency (water_and_energy_use) ---
  # Inputs: water_use_save [0–2], energy_use_save [0–2]     (max = 4)

  water_use_save  <- get("water_use_save")
  energy_use_save <- get("energy_use_save")

  sum_water_and_energy_use <- water_use_save + energy_use_save
  water_and_energy_use     <- r2((sum_water_and_energy_use / 4) * 100)

  # --- efficiency_score ---
  sum_efficiency  <- water_and_energy_use + mgt_soilfert + mgt_pestdis
  efficiency_score <- r2(sum_efficiency / 3)

  # ==================================================================
  # PRINCIPLE 5: RESILIENCE
  # ==================================================================

  # --- 5a. Social Resilience (soc_res) ---
  # Inputs: community_coop [0–2], ext_finance [0–2]         (max = 4)

  community_coop <- get("community_coop")
  ext_finance    <- get("ext_finance")

  sum_soc_res <- community_coop + ext_finance
  soc_res     <- r2((sum_soc_res / 4) * 100)

  # --- 5b. Economic Resilience (econ_res) ---
  # Inputs: div_activ [0–2], farminput_auto [0–2]           (max = 4)

  div_activ      <- get("div_activ")
  farminput_auto <- get("farminput_auto")

  econ_res <- r2(((div_activ + farminput_auto) / 4) * 100)

  # --- 5c. Soil Conservation (soil_conserve) ---
  # Inputs: soil_cover [0–2], soil_disturb [0–2]            (max = 4)

  soil_cover   <- get("soil_cover")
  soil_disturb <- get("soil_disturb")

  soil_conserve <- r2(((soil_cover + soil_disturb) / 4) * 100)

  # --- resilience_score ---
  resilience_score <- r2((soc_res + econ_res + soil_conserve) / 3)

  # ==================================================================
  # PRINCIPLE 6: CULTURE & FOOD TRADITIONS
  # ==================================================================

  # --- 6a. Dietary Diversity & Food Self-sufficiency (dietdiv_foodself) ---
  # Inputs: food_div [0–2], farm_ingred [0–2]               (max = 4)

  food_div    <- get("food_div")
  farm_ingred <- get("farm_ingred")

  sum_dietdiv_foodself <- food_div + farm_ingred
  dietdiv_foodself     <- r2((sum_dietdiv_foodself / 4) * 100)

  # --- 6b. Food Heritage (food_heritage) ---
  # Inputs: trad_food [0–2], value_heritage [0–2]           (max = 4)

  trad_food      <- get("trad_food")
  value_heritage <- get("value_heritage")

  sum_food_heritage <- trad_food + value_heritage
  food_heritage     <- r2((sum_food_heritage / 4) * 100)

  # --- cultfood_score ---
  sum_cultfood  <- dietdiv_foodself + food_heritage
  cultfood_score <- r2(sum_cultfood / 2)

  # ==================================================================
  # PRINCIPLE 7: CO-CREATION & SHARING OF KNOWLEDGE
  # ==================================================================

  # --- 7a. Integration into Agro-Ecological Context (int_ageco) ---
  # Inputs: context_know [0–2], share_aginnov [0–2]         (max = 4)

  context_know  <- get("context_know")
  share_aginnov <- get("share_aginnov")

  sum_int_ageco <- context_know + share_aginnov
  int_ageco     <- r2((sum_int_ageco / 4) * 100)

  # --- 7b. Social Knowledge (soc_know) ---
  # Inputs: know_platform [0–2], know_process [0–2]         (max = 4)

  know_platform <- get("know_platform")
  know_process  <- get("know_process")

  sum_soc_know <- know_platform + know_process
  soc_know     <- r2((sum_soc_know / 4) * 100)

  # --- cocrea_score ---
  sum_cocrea  <- int_ageco + soc_know
  cocrea_score <- r2(sum_cocrea / 2)

  # ==================================================================
  # PRINCIPLE 8: HUMAN & SOCIAL VALUES
  # ==================================================================

  # --- 8a. Women's Empowerment in CAET (wom_emp_caet) ---
  # Inputs: women_role [0–2], youth [0–2]                   (max = 4)

  women_role <- get("women_role")
  youth_var  <- get("youth")    # 'youth' is R reserved? Use youth_var

  sum_wom_emp_caet <- women_role + youth_var
  wom_emp_caet     <- r2((sum_wom_emp_caet / 4) * 100)

  # --- 8b. Labour & Social Inequalities (labour_socinq) ---
  # Inputs: workcond_farmer [0–2] always relevant when extworker = 1;
  #         workcond_emp [0–2] always collected (no skip)
  # XLSForm:
  #   sum = if (workcond_emp != "") workcond_farmer + workcond_emp else workcond_farmer
  #   score = if (workcond_emp != "")
  #             round((sum / 4) * 100, 2)
  #           else
  #             round((sum / 2) * 100, 2)
  #
  # Note: workcond_farmer is shown when extworker = 1.
  # workcond_emp has no relevant restriction (always asked).

  workcond_farmer <- get("workcond_farmer", gate = has_extworker)
  workcond_emp    <- score_col(df, "workcond_emp")

  has_emp_score <- has_val(workcond_emp) & !is.na(workcond_emp)

  sum_labour_socinq <- ifelse(has_emp_score,
                              workcond_farmer + workcond_emp,
                              workcond_farmer)
  labour_socinq     <- ifelse(has_emp_score,
                              r2((sum_labour_socinq / 4) * 100),
                              r2((sum_labour_socinq / 2) * 100))

  # --- 8c. Labour Conditions / Farmer Wellbeing (labor_condition) ---
  # Inputs: agro_innov [0–2], activ_outside [0–2]           (max = 4)

  agro_innov    <- get("agro_innov")
  activ_outside <- get("activ_outside")

  sum_labor_condition <- agro_innov + activ_outside
  labor_condition     <- r2((sum_labor_condition / 4) * 100)

  # --- human_score ---
  human_score <- r2((wom_emp_caet + labour_socinq + labor_condition) / 3)

  # ==================================================================
  # PRINCIPLE 9: CIRCULAR & SOLIDARITY ECONOMY
  # ==================================================================

  # --- 9a. Local Market (local_market) ---
  # Inputs: local_market_share [0–2], pgs_cert [0–2]        (max = 4)

  local_market_share <- get("local_market_share")
  pgs_cert           <- get("pgs_cert")

  sum_local_market <- local_market_share + pgs_cert
  local_market     <- r2((sum_local_market / 4) * 100)

  # --- 9b. Local Circulation of Resources (local_circ) ---
  # Inputs: orig_inputs [0–2], resource_share [0–2]         (max = 4)

  orig_inputs    <- get("orig_inputs")
  resource_share <- get("resource_share")

  sum_local_circ <- orig_inputs + resource_share
  local_circ     <- r2((sum_local_circ / 4) * 100)

  # --- circular_score ---
  sum_circular  <- local_market + local_circ
  circular_score <- r2(sum_circular / 2)

  # ==================================================================
  # PRINCIPLE 10: RESPONSIBLE GOVERNANCE
  # ==================================================================

  # --- 10a. Producer Empowerment (prod_empow) ---
  # Inputs: auto_dec [0–2], access_finance [0–2]            (max = 4)

  auto_dec       <- get("auto_dec")
  access_finance <- get("access_finance")

  sum_prod_empow <- auto_dec + access_finance
  prod_empow     <- r2((sum_prod_empow / 4) * 100)

  # --- 10b. Producer Access to Resources (prod_access) ---
  # Inputs: access_land [0–2], access_gene [0–2]            (max = 4)

  access_land <- get("access_land")
  access_gene <- get("access_gene")

  sum_prod_access <- access_land + access_gene
  prod_access     <- r2((sum_prod_access / 4) * 100)

  # --- respgov_score ---
  sum_respgov  <- prod_empow + prod_access
  respgov_score <- r2(sum_respgov / 2)

  # ==================================================================
  # CAET TOTAL SCORE
  # ==================================================================
  # Simple average of all 10 principle scores (each already 0–100)

  caet_score <- r2(
    (div_score       +
     synergy_score   +
     recycling_score +
     efficiency_score+
     resilience_score+
     cultfood_score  +
     cocrea_score    +
     human_score     +
     circular_score  +
     respgov_score) / 10
  )

  # ==================================================================
  # BIND RESULTS BACK TO DATA FRAME
  # ==================================================================

  out <- df
  out[["sum_plant_diversity"]]      <- sum_plant_diversity
  out[["plant_diversity"]]          <- plant_diversity
  out[["sum_temp_spatial_div"]]     <- sum_temp_spatial_div
  out[["temp_spatial_div"]]         <- temp_spatial_div
  out[["sum_anim_diversity"]]       <- sum_anim_diversity
  out[["anim_diversity"]]           <- anim_diversity
  out[["sum_crop_liv_aqua"]]        <- sum_crop_liv_aqua
  out[["crop_liv_aqua"]]            <- crop_liv_aqua
  out[["integ_trees"]]              <- integ_trees
  out[["connect_ag"]]               <- connect_ag
  out[["div_score"]]                <- div_score

  out[["sum_seed_breed"]]           <- sum_seed_breed
  out[["seed_breed"]]               <- seed_breed
  out[["sum_recycling_biomass"]]    <- sum_recycling_biomass
  out[["recycling_biomass"]]        <- recycling_biomass
  out[["sum_water_energy"]]         <- sum_water_energy
  out[["water_energy"]]             <- water_energy
  out[["recycling_score"]]          <- recycling_score

  out[["sum_mgt_soilfert"]]         <- sum_mgt_soilfert
  out[["mgt_soilfert"]]             <- mgt_soilfert
  out[["sum_mgt_pestdis"]]          <- sum_mgt_pestdis
  out[["mgt_pestdis"]]              <- mgt_pestdis
  out[["sum_water_and_energy_use"]] <- sum_water_and_energy_use
  out[["water_and_energy_use"]]     <- water_and_energy_use
  out[["sum_efficiency"]]           <- sum_efficiency
  out[["efficiency_score"]]         <- efficiency_score

  out[["sum_soc_res"]]              <- sum_soc_res
  out[["soc_res"]]                  <- soc_res
  out[["econ_res"]]                 <- econ_res
  out[["soil_conserve"]]            <- soil_conserve
  out[["resilience_score"]]         <- resilience_score

  out[["sum_dietdiv_foodself"]]     <- sum_dietdiv_foodself
  out[["dietdiv_foodself"]]         <- dietdiv_foodself
  out[["sum_food_heritage"]]        <- sum_food_heritage
  out[["food_heritage"]]            <- food_heritage
  out[["sum_cultfood"]]             <- sum_cultfood
  out[["cultfood_score"]]           <- cultfood_score

  out[["sum_int_ageco"]]            <- sum_int_ageco
  out[["int_ageco"]]                <- int_ageco
  out[["sum_soc_know"]]             <- sum_soc_know
  out[["soc_know"]]                 <- soc_know
  out[["sum_cocrea"]]               <- sum_cocrea
  out[["cocrea_score"]]             <- cocrea_score

  out[["sum_wom_emp_caet"]]         <- sum_wom_emp_caet
  out[["wom_emp_caet"]]             <- wom_emp_caet
  out[["sum_labour_socinq"]]        <- sum_labour_socinq
  out[["labour_socinq"]]            <- labour_socinq
  out[["sum_labor_condition"]]      <- sum_labor_condition
  out[["labor_condition"]]          <- labor_condition
  out[["human_score"]]              <- human_score

  out[["sum_local_market"]]         <- sum_local_market
  out[["local_market"]]             <- local_market
  out[["sum_local_circ"]]           <- sum_local_circ
  out[["local_circ"]]               <- local_circ
  out[["sum_circular"]]             <- sum_circular
  out[["circular_score"]]           <- circular_score

  out[["sum_prod_empow"]]           <- sum_prod_empow
  out[["prod_empow"]]               <- prod_empow
  out[["sum_prod_access"]]          <- sum_prod_access
  out[["prod_access"]]              <- prod_access
  out[["sum_respgov"]]              <- sum_respgov
  out[["respgov_score"]]            <- respgov_score

  out[["synergy_score"]]            <- synergy_score
  out[["caet_score"]]               <- caet_score

  return(out)
}

# =============================================================================
# CONVENIENCE: extract only the CAET indicator columns
# =============================================================================

#' Return a tidy summary table of principle scores and CAET total
#'
#' @param results  Data frame returned by compute_caet()
#' @return         Data frame with farm identifier + all principle scores
caet_summary <- function(results, id_col = NULL) {

  score_cols <- c(
    "div_score", "synergy_score", "recycling_score", "efficiency_score",
    "resilience_score", "cultfood_score", "cocrea_score", "human_score",
    "circular_score", "respgov_score", "caet_score"
  )

  if (!is.null(id_col) && id_col %in% names(results)) {
    out <- results[, c(id_col, score_cols), drop = FALSE]
  } else {
    out <!- results[, score_cols, drop = FALSE]�  out <- cbind(farm_id = seq_len(nrow(out)), out)
  }

  names(out) <- c(
    if (!is.null(id_col) && id_col %in% names(results)) id_col else "farm_id",
    "P1_Diversity", "P2_Synergies", "P3_Recycling", "P4_Efficiency",
    "P5_Resilience", "P6_CultureFood", "P7_CoCreation", "P8_HumanValues",
    "P9_CircularEconomy", "P10_RespGov", "CAET_Total"
  )

  return(out)
}

# =============================================================================
# VALIDATION CHECKS
# =============================================================================

#' Check input data for common problems before running compute_caet()
#'
#' @param df  Raw data frame
#' @return    Invisible list of warnings; prints to console
validate_tape_data <- function(df) {

  input_cols <- c(
    "raise_animals", "extworker",
    # P1
    "num_plant", "plant_gendiv", "temp_div", "space_div",
    "num_animal", "anim_gendiv",
    # P2
    "feed_prod", "vary_service", "tree_farm", "tree_divspe",
    "plot_mosaic", "natveg_pct",
    # P3
    "useloc_seedbreed", "nat_seedbreed", "reuse_orgres", "nonorg_waste",
    "source_water", "source_energy",
    # P4
    "onfarm_nutcycle", "nitrogen_plant", "onf_eco", "use_vet",
    "water_use_save", "energy_use_save",
    # P5
    "community_coop", "ext_finance", "div_activ", "farminput_auto",
    "soil_cover", "soil_disturb",
    # P6
    "food_div", "farm_ingred", "trad_food", "value_heritage",
    # P7
    "context_know", "share_aginnov", "know_platform", "know_process",
    # P8
    "women_role", "youth", "workcond_farmer", "workcond_emp",
    "agro_innov", "activ_outside",
    # P9
    "local_market_share", "pgs_cert", "orig_inputs", "resource_share",
    # P10
    "auto_dec", "access_finance", "access_land", "access_gene"
  )

  issues <- list()
  missing_cols <- setdiff(input_cols, names(df))

  if (length(missing_cols) > 0) {
    msg <- paste("Missing columns:", paste(missing_cols, collapse = ", "))
    message("WARNING: ", msg)
    issues$missing_cols <- missing_cols
  }

  valid_scores <- c(0, 0.5, 1, 1.5, 2, NA)
  ordinal_cols <- setdiff(input_cols, c("raise_animals", "extworker"))

  for (col in intersect(ordinal_cols, names(df))) {
    vals <- as_num(df[[col]])
    bad  <- vals[!is.na(vals) & !vals %in% valid_scores]
    if (length(bad) > 0) {
      msg <- sprintf("Column '%s' has unexpected values: %s",
                     col, paste(unique(bad), collapse = ", "))
      message("WARNING: ", msg)
      issues[[paste0("bad_", col)]] <- bad
    }
  }

  if (length(issues) == 0) message("OK: No validation issues found.")
  invisible(issues)
}

# =============================================================================
# EXAMPLE / DEMO
# =============================================================================
if (FALSE) {

  # --- Load exported KoboCollect data ---
  df_raw <- read.csv("tape_export.csv", stringsAsFactors = FALSE)

  # --- Validate ---
  validate_tape_data(df_raw)

  # --- Compute ---
  df_scored <- compute_caet(df_raw)

  # --- Summary table ---
  summary_tbl <- caet_summary(df_scored, id_col = "_uuid")
  print(head(summary_tbl))

  # --- Export ---
  write.csv(df_scored,    "tape_all_indicators.csv",  row.names = FALSE)
  write.csv(summary_tbl,  "tape_caet_summary.csv",    row.names = FALSE)

  # --- Quick visualisation of principle scores ---
  library(ggplot2)
  library(tidyr)

  long <- tidyr::pivot_longer(
    summary_tbl,
    cols = starts_with("P"),
    names_to  = "Principle",
    values_to = "Score"
  )

  ggplot(long, aes(x = Principle, y = Score)) +
    geom_boxplot(fill = "#4C9BE8", alpha = 0.7) +
    geom_jitter(width = 0.15, alpha = 0.4, size = 1) +
    ylim(0, 100) +
    labs(title  = "TAPE CAET Step 2 Principle Scores",
         y      = "Score (0–100)",
         x      = NULL) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 35, hjust = 1))
}

# =============================================================================
# TAPE CAET Step 1 Elements Calculator
# =============================================================================
# Tool Assessment for Agroecology Performance Evaluation (TAPE) - FAO
# Computes all Step 1 CAET sub-indicators, principle scores, and CAET total
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

## -----------------------------------------------------------------------
## 0. SETUP
## -----------------------------------------------------------------------
rm(list = ls())
#setwd("~/Documents/Documents/FAOTAPE/")
#file_path <- "Clone_of_Cabo_Verde_TAPE_Dataset_Blueprint.xlsx"

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

#' Strip KoboToolbox folder-style prefixes from exported column names.
#' make.unique() prevents repeated answer-code names (e.g. 0, 1, dk_refuse)
#' from causing ambiguous column references after stripping.
normalize_kobo_names <- function(df) {
  df <- as.data.frame(df)
  names(df) <- make.unique(sub(".*/", "", names(df)))
  df
}

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
  
  # Cabo Verde export columns contain Kobo group prefixes; normalize once
  # before looking up the CAET variables below.
  df <- normalize_kobo_names(df)
  
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

caet_summary <- function(results, id_col = NULL) {
  
  score_cols <- c(
    "div_score", "synergy_score", "recycling_score", "efficiency_score",
    "resilience_score", "cultfood_score", "cocrea_score", "human_score",
    "circular_score", "respgov_score", "caet_score"
  )
  
  if (!is.null(id_col) && id_col %in% names(results)) { 
    out <- results[, c(id_col, score_cols), drop = FALSE] 
  } else {
    out <- results[, score_cols, drop = FALSE] 
    out <- cbind(farm_id = seq_len(nrow(out)), out) 
  }
  
  names(out) <- c(
    if (!is.null(id_col) && id_col %in% names(results)) id_col else "farm_id",
    "P1_Diversity", "P2_Synergies", "P3_Recycling", "P4_Efficiency",
    "P5_Resilience", "P6_CultureFood", "P7_CoCreation", "P8_HumanValues",
    "P9_CircularEconomy", "P10_RespGov", "CAET_Total"
  )
  
  out
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
  df_raw <- normalize_kobo_names(df_raw)
  
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



################################################################################
# TAPE Step 2 Performance Indicators Step 2 Core Criteria
# Tool for Agroecology Performance Evaluation (FAO)
#
# Column names match the KoboToolbox XLSForm (ajfT4SY4zcJPVqKCJuuQxH.xlsx).
# All -99 values are treated as "Don't know / Refused to answer".
#
# Functions:
#   compute_fies()               Food Insecurity Experience Scale
#   compute_dietary_diversity()  Women's Dietary Diversity Score
#   compute_soil_health()        Soil Health Score
#   compute_economic_index()     Qualitative Economic Perception Index
#   compute_land_tenure()        Land Tenure Scores (men / women / household)
#   compute_youth_score()        Youth Employment & Aspiration Score
#   compute_aweai()              Abbreviated Women's Empowerment in Ag. Index
#   compute_biodiversity_index() Gini-Simpson Biodiversity Index
#   compute_tape_performance()   Wrapper: runs all indicators above
#   tape_performance_summary()   Summary statistics across all indicators
#   validate_performance_data()  Data validation helper
################################################################################

# Utility 

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

# 1. FIES Food Insecurity Experience Scale #
# Inputs (binary 0/1, select_one fies):
#   worried, healthy, fewfoods, skipped, ateless, ranout, hungry, wholeday
#
# Outputs added to data:
#   fies_sum          raw item sum (0.8)
#   fies_secure       1 if food secure (sum = 0)
#   fies_mild         1 if mildly food insecure
#   fies_moderate     1 if moderately food insecure
#   fies_severe       1 if severely food insecure (wholeday = 1)
#   fies_cat          category label
#   fies_score        0-100, higher = MORE food secure
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

#2. Dietary Diversity Score#

# Women's Dietary Diversity (relevant when hh_women > 0 OR hh_fyoung > 0).
# 10 food groups aggregated from sub-items; -99, 0.
#
# Raw inputs (select_one dietyn, 0/1/-99):
#   grains_A, grains_B (grains_tmp = max of both)
#   pulses, nuts, eggs, darkgreen, otherveg, otherfruit  (single items)
#   dairy_E, dairy_F   (dairy_tmp = max of both)
#   meat_H, meat_I, meat_J, meat_K, meat_L (meat_tmp = max of all)
#   darkyellow_N, darkyellow_O (darkyellow_tmp = max of both)
#
# Outputs:
#   grains_tmp, pulses_tmp, nuts_tmp, dairy_tmp, meat_tmp,
#   eggs_tmp, darkgreen_tmp, darkyellow_tmp, otherveg_tmp, otherfruit_tmp
#   dietary_score  0-100
#   minimum_diet   "Yes" if dietary_score >= 50
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

# 3. Soil Health Score #
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

# 4. Qualitative Economic Perception Index #
# 6 items (select_one, 0-4 scale; profitable is 1-4; -99 = N/A excluded):
#   stabincome, livcon, envfuture, profitable, productive, valueadd
#
# Formula: round(((sum_of_valid / n_valid) * 25), 2)
#   Converts 0-4 mean to 0-100.
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

#5. Land Tenure Scores #
# Three components per gender and household:
#
#  A) Legal recognition:
#     recland_men/women (yn: 1=Yes/0=No/-99, 0) formal document exists
#     name_men/women    (yn: 1=Yes/0=No/-99, 0) name on document
#     landt_legal = if(recland=1, if(name=1, 1, 0.5), 0)
#
#  B) Perception of tenure security:
#     ltperc_men/women (select_one ltperc; stored as 1/0.75/0.5/0.25/0;
#                        inverted: 1=not likely, 0=extremely likely; -99, 0.5)
#
#  C) Land rights (sell / bequeath / inherit):
#     sell/beq/inh yn, -99, 0
#     sel_beq_inh = if(sell+beq+inh > 0, 1, 0)
#
#  Score (%) = round(((legal + ltperc + rights) / 3) * 100, 2)
#
# Outputs:
#   landt_legal_men, landt_legal_women, landt_legal_household
#   ltperc_men_tmp, ltperc_women_tmp, max_ltperc
#   sel_beq_inh_men, sel_beq_inh_women, max_sel_beq_inh
#   landtenure_score_men, landtenure_score_women, landtenure_score_household  (0-1)
#   landtenure_score_men_pct, landtenure_score_women_pct,
#   landtenure_score_household_pct  (0-100)
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
  
  # Component A: legal recognition
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
  
  # Component B: perception (ltperc already stored as 0-1; -99, 0.5)
  d$ltperc_men_tmp   <- ifelse(is.na(d$ltperc_men)   | d$ltperc_men   == -99, 0.5, as.numeric(d$ltperc_men))
  d$ltperc_women_tmp <- ifelse(is.na(d$ltperc_women) | d$ltperc_women == -99, 0.5, as.numeric(d$ltperc_women))
  d$max_ltperc       <- pmax(d$ltperc_men_tmp, d$ltperc_women_tmp, na.rm = TRUE)
  
  # Component C: rights (sell / bequeath / inherit)
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
  
  # Scores 
  d$landtenure_score_men       <- (d$landt_legal_men        + d$ltperc_men_tmp    + d$sel_beq_inh_men)   / 3
  d$landtenure_score_women     <- (d$landt_legal_women     + d$ltperc_women_tmp + d$sel_beq_inh_women) / 3
  d$landtenure_score_household <- (d$landt_legal_household + d$max_ltperc        + d$max_sel_beq_inh)   / 3
  
  d$landtenure_score_men_pct       <- round(d$landtenure_score_men       * 100, 2)
  d$landtenure_score_women_pct     <- round(d$landtenure_score_women     * 100, 2)
  d$landtenure_score_household_pct <- round(d$landtenure_score_household * 100, 2)
  
  return(d)
}

# 6. Youth Employment & Aspiration Score #
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
#'   youth_m_score, youth_f_score, youth_score (0-100)
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

# 7. A-WEAI #
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
#     Decision coding (dec list): 1=man only 0 men / 0 women
#                                  2=man mainly 1 men / 0.25 women
#                                  3=woman mainly 0.25 men / 1 women
#                                  4=woman only 0.5 men / 0.5 women
#                                  5=both equally 0.5 men / 0.5 women
#                                  6=joint 1 men / 1 women
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
#     Involvement scale (involv): 1 = 0, 2 = 0.25, 3 = 0.5, 4 = 0.75, 5 = 1
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
  
  # Leadership involvement scoring
  .involv_score <- function(x) {
    x <- ifelse(is.na(x) | x == -99, 1L, as.integer(x))
    dplyr::case_when(x == 1 ~ 0, x == 2 ~ 0.25, x == 3 ~ 0.5,
                     x == 4 ~ 0.75, x == 5 ~ 1, TRUE ~ 0)
  }
  
  # Domain 1: Time allocation
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
  
  # Domain 2: Asset ownership
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
  
  # Domain 3: Production decisions
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
  
  # Domain 4: Revenue decisions 
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
  
  # Domain 5: Leadership
  if (!all(c("leadership_women", "leadership_men") %in% names(d))) {
    d$involv_agri_men_score   <- .involv_score(d$involv_agri_men)
    d$involv_agri_women_score <- .involv_score(d$involv_agri_women)
    d$involv_oth_men_score    <- .involv_score(d$involv_oth_men)
    d$involv_oth_women_score  <- .involv_score(d$involv_oth_women)
    
    d$leadership_men   <- pmax(d$involv_agri_men_score,   d$involv_oth_men_score,   na.rm = TRUE)
    d$leadership_women <- pmax(d$involv_agri_women_score, d$involv_oth_women_score, na.rm = TRUE)
  }
  
  # Aggregate 5DE
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

# 8. Biodiversity Index (Gini-Simpson) #
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
# Usage A pre-computed KoboToolbox export columns in main dataset:
#   Requires: sum_pi, bio_area_sum   (+ optionally sum_pi_animals, total_tlu)
#
# Usage B from repeat-group data frames:
#   compute_biodiversity_index(data,
#     crop_data   = df with cols: household_id, variety_area_ha
#     animal_data = df with cols: household_id, animal_count_tlu)
################################################################################
compute_biodiversity_index <- function(data, crop_data = NULL, animal_data = NULL,
                                       tlu_factors = NULL) {
  d <- normalize_kobo_names(data)

  # The Cabo Verde export does not contain populated pre-computed biodiversity
  # summaries, so calculate them from the repeat sheets when supplied.
  #
  # Crop limitation in this form: there is no variety-level area column.
  # Therefore the Gini-Simpson calculation is performed at the crop level
  # using bio_crop_area_ha.
  if (!is.null(crop_data)) {
    crop_data <- normalize_kobo_names(crop_data)

    crop_id <- if ("_submission__id" %in% names(crop_data)) "_submission__id" else
      if ("household_id" %in% names(crop_data)) "household_id" else NULL
    main_id <- if ("_id" %in% names(d)) "_id" else
      if ("household_id" %in% names(d)) "household_id" else NULL

    if (is.null(crop_id) || is.null(main_id))
      stop("compute_biodiversity_index: could not identify the household/submission ID in main and crop data.")
    if (!"bio_crop_area_ha" %in% names(crop_data))
      stop("compute_biodiversity_index: crop_data must contain 'bio_crop_area_ha'.")

    crop_agg <- dplyr::summarise(
      crop_data,
      sum_pi       = sum(as.numeric(bio_crop_area_ha)^2, na.rm = TRUE),
      bio_area_sum = sum(as.numeric(bio_crop_area_ha), na.rm = TRUE),
      .by = dplyr::all_of(crop_id)
    )
    names(crop_agg)[names(crop_agg) == crop_id] <- main_id

    d <- dplyr::left_join(d, crop_agg, by = main_id)
    d$simpson       <- ifelse(d$bio_area_sum > 0,
                              d$sum_pi / (d$bio_area_sum * d$bio_area_sum),
                              NA_real_)
    d$GSIndex_crops <- round(100 * (1 - d$simpson), 2)
  } else if (all(c("sum_pi", "bio_area_sum") %in% names(d))) {
    # Retain support for genuinely pre-computed exports.
    d$simpson       <- d$sum_pi / (d$bio_area_sum * d$bio_area_sum)
    d$GSIndex_crops <- round(100 * (1 - d$simpson), 2)
  } else {
    warning("compute_biodiversity_index: crop repeat data not provided and no pre-computed crop index is available.")
    d$GSIndex_crops <- NA_real_
    d$GSI_overall   <- NA_real_
    return(d)
  }

  if (!is.null(animal_data)) {
    animal_data <- normalize_kobo_names(animal_data)

    animal_id <- if ("_submission__id" %in% names(animal_data)) "_submission__id" else
      if ("household_id" %in% names(animal_data)) "household_id" else NULL
    main_id <- if ("_id" %in% names(d)) "_id" else
      if ("household_id" %in% names(d)) "household_id" else NULL

    if (is.null(animal_id) || is.null(main_id))
      stop("compute_biodiversity_index: could not identify the household/submission ID in main and animal data.")

    # animal_count_tlu is present in the form but is completely blank in the
    # Cabo Verde export. Use a supplied lookup keyed by count_animal_label.
    if ("animal_count_tlu" %in% names(animal_data) &&
        any(!is.na(animal_data$animal_count_tlu))) {
      animal_data$tlu <- as.numeric(animal_data$animal_count_tlu)
    } else {
      if (is.null(tlu_factors)) {
        warning("compute_biodiversity_index: animal_count_tlu is empty. Supply 'tlu_factors' keyed by count_animal_label to calculate livestock diversity.")
        d$GSIndex_animals <- NA_real_
        d$GSI_overall <- d$GSIndex_crops
        return(d)
      }
      if (!all(c("animal_count", "count_animal_label") %in% names(animal_data)))
        stop("compute_biodiversity_index: animal_data must contain 'animal_count' and 'count_animal_label'.")

      labels <- as.character(animal_data$count_animal_label)
      factors <- as.numeric(tlu_factors[labels])
      missing_factors <- unique(labels[is.na(factors) & !is.na(animal_data$animal_count)])
      if (length(missing_factors) > 0)
        stop("compute_biodiversity_index: missing TLU factors for: ",
             paste(missing_factors, collapse = ", "))
      animal_data$tlu <- as.numeric(animal_data$animal_count) * factors
    }

    anim_agg <- dplyr::summarise(
      animal_data,
      sum_pi_animals = sum(tlu^2, na.rm = TRUE),
      total_tlu      = sum(tlu, na.rm = TRUE),
      .by = dplyr::all_of(animal_id)
    )
    names(anim_agg)[names(anim_agg) == animal_id] <- main_id

    d <- dplyr::left_join(d, anim_agg, by = main_id)
    d$simpson_animals <- ifelse(d$total_tlu > 0,
                                d$sum_pi_animals / (d$total_tlu * d$total_tlu),
                                NA_real_)
    d$GSIndex_animals <- round(100 * (1 - d$simpson_animals), 2)

    # If the household has no livestock rows, retain the crop index alone.
    d$GSI_overall <- ifelse(is.na(d$GSIndex_animals),
                            d$GSIndex_crops,
                            round((d$GSIndex_crops + d$GSIndex_animals) / 2, 2))
  } else {
    d$GSI_overall <- d$GSIndex_crops
  }

  return(d)
}

# 9. Master Wrapper #
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
    skip_soil         = TRUE,   # Cabo Verde form does not contain the soil module
    skip_economic     = FALSE,
    skip_land_tenure  = FALSE,
    skip_youth        = FALSE,
    skip_aweai        = FALSE,
    skip_biodiversity = TRUE,  # requires livestock TLU factors in this export
    tlu_factors       = NULL
) {
  # Normalize the main Kobo export before any indicator accesses its fields.
  d <- normalize_kobo_names(data)
  
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
      compute_biodiversity_index(d, crop_data, animal_data, tlu_factors = tlu_factors),
      error = function(e) {
        warning("Biodiversity index skipped: ", conditionMessage(e))
        d
      }
    )
  }
  
  return(d)
}

# 10. Summary Statistics
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

# 11. Validation 
validate_performance_data <- function(data, skip_soil = TRUE) {
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
  
  if (!skip_soil) {
    .miss(c("soilstructure","soilcompaction","soildepth","soilresidues",
            "soilcolor","water_ret","cover","erosion",
            "invertebrates","microbio"), "Soil health")
    for (col in c("soilstructure","soilcompaction","soildepth","soilresidues",
                  "soilcolor","water_ret","cover","erosion",
                  "invertebrates","microbio"))
      .range(col, 1, 5, "Soil health")
  }
  
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
          "wtime_otgain_men","wtime_otgain_women"), "AWEAI time allocation")
  
  if (length(issues) == 0) {
    message("validate_performance_data: all checks passed.")
    return(invisible(data.frame()))
  }
  result <- do.call(rbind, issues)
  message("validate_performance_data: ", nrow(result), " issue(s) found.")
  result
}

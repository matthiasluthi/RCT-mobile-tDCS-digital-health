# Setup ==================
# Load libraries and data from REDCap

library(tidyverse)
library(readxl)
library(ggmap)

screening <- read.csv(
  "data/ProjetoPsylectTriage-ScreeningDataPsylect_DATA_2022-09-15_1544.csv", 
  sep = ";")

trials <- read.csv(
  "data/2110ProjetoPsylectInterv_DATA_2022-10-21_1748.csv",
  sep = ";")

meds <- read_excel("data/psylect_medications.xlsx")

# For geomapping
register_google(key = "AIzaSyCqtWx9Ld6Jc65ju-bGcN_GaQ0MBQvB-DE")


# Clean screening data =========

screening <- screening %>%
  rename(record_id_sc = record_id) %>%
  mutate(
    current_psychotherapy_y = mini_76==1,
    # Calculate summary data for medication
    antidep_calc_ssri = fluoxetina___5 + paroxetina___5 + sertralina___5 + 
      citalopram___5+escitalopram___5 + fluvoxamina___5, 
    antidep_snri_refract = venlafaxina___5 + desvenlafaxina___5 + 
      duloxetina___5, 
    antidep_tric_refract = amitriptilina___5 + nortriptilina___5 + 
      imipramina___5 + clomipramina___5, 
    antidep_imao_refract = parnate___5, 
    antidep_otherad_refract = bupropiona___5 + mirtazapina___5 + trazodona___5 + 
      agomelatina___5 + vortioxetina___5,
    current_resistance = (fluoxetina___1 & fluoxetina___5) + 
      (paroxetina___1 & paroxetina___5) + (sertralina___1 & sertralina___5) + 
      (citalopram___1 & citalopram___5) + (escitalopram___1 & escitalopram___5) + 
      (fluvoxamina___1 & fluvoxamina___5) + (amitriptilina___1 & amitriptilina___5) + 
      (nortriptilina___1 & nortriptilina___5) + (imipramina___1 & imipramina___5) + 
      (clomipramina___1 & clomipramina___5) + (venlafaxina___1 & venlafaxina___5) + 
      (desvenlafaxina___1 & desvenlafaxina___5) + (duloxetina___1 & duloxetina___5) + 
      (parnate___1 & parnate___5) + (bupropiona___1 & bupropiona___5) + 
      (mirtazapina___1 & mirtazapina___5) + (trazodona___1 & trazodona___5) + 
      (agomelatina___1 & agomelatina___5) + (vortioxetina___1 & vortioxetina___5),
    min_1_failed_AD_y = current_resistance>0,
    final_past_dep_y2 = ifelse(mini_27 > 1, 1, 0),
    final_dyst_current_y2 = ifelse(mini_28 >= 24, 1, 0),
    final_anx_current_y2 = (final_panic_cur_y + final_gad_cur_y + final_social_anx_y) != 0,
    mini_27 = ifelse(mini_27==99, NA, mini_27)
  ) %>%
  select(-c(consent_date, copy_consent)) %>%
  select(record_id_sc, everything()) 


# Clean trial data =========

# Define utility variables: 
# vectors for setting variable types
factors <- names(select(trials, reason_dropout, gender, maritalstatus, 
                        income, ethnicity, religion, occupation, smoking, 
                        alcohol, hypertension, diabetes, hypothyroidism, 
                        hyperthyroidism, 
                        ends_with("_fam"), starts_with("blind_"), 
                        -contains("_vas")))
ordered_factors <- c("schooling", "income", "physicial_activity")

# Side effects: 
# "side_g_12", "mood improvement", is removed. 
severities <- c("side_g_1", "side_g_2", "side_g_3", "side_g_4", "side_g_5", 
                "side_g_6", "side_g_7", "side_g_8", "side_g_9", "side_g_10",
                "side_g_11", "side_g_13", "side_g_14", "side_g_15", 
                "side_g_16")

side_fx = c("no_side_fx", "mild_side_fx", "moderate_side_fx", 
            "severe_side_fx")

trials <- trials %>% 
  # Some record_id2 data was entered with spaces or "-", but mostly "_" was used.
  # Make coherent:
  mutate(
    # remove trailing white spaces
    record_id2 = trimws(record_id2, which = "both"),
    record_id2 = str_replace_all(record_id2, " ", "_"), 
    record_id2 = str_replace_all(record_id2, "-", "_")) %>%
  # turn record_id2 into separate columns
  extract(col = record_id2, 
          into = c("record_id2a", "record_id2b", "record_id2c"),
          regex = "^PSYLECT_([A-Za-z0-9]*)_([A-Za-z0-9]*)_([A-Za-z0-9]*)$",
          remove = FALSE,
          convert = TRUE
  ) %>%
  mutate(redcap_event_name_edit = redcap_event_name,
         redcap_event_name_edit = replace(redcap_event_name_edit, 
                                          redcap_event_name_edit == "avaliao_basal_arm_2", 
                                          "semana_0_arm_2"),
         record_id2 = na_if(record_id2, ""),
         zipcode = na_if(zipcode, "")
  ) %>%
  extract(col = redcap_event_name_edit, 
          into = "week",
          regex = "^semana_([0-9])_arm_2$",
          remove = FALSE,
          convert = TRUE
  ) %>%
  rename(record_id_sc = record_id2b,
         subject = record_id) %>%
  group_by(subject) %>% 
  fill(c(record_id2:record_id2c, arm_belong, record_id2, email, zipcode), .direction = "downup") %>%
  ungroup() %>%
  mutate(
    across(c(subject, all_of(factors)), as.factor),
    across(all_of(ordered_factors), as.ordered),
    condition_alt = factor(arm_belong, 
                           levels = c(1, 2, 3), 
                           labels = c("Double-active", 
                                      "tDCS only", 
                                      "Double sham")),
    condition = relevel(condition_alt, ref = "Double sham"),
    Groups = factor(condition, 
                    levels = c("Double-active", 
                               "tDCS only", 
                               "Double sham")),
    Grupos = factor(arm_belong, 
                    levels = c(1, 2, 3), 
                    labels = c("Duplo-ativo", 
                               "ETCC-apenas", 
                               "Duplo-placebo")),
    condition_tdcs = fct_collapse(condition,
                                  tDCS = c("Double-active", "tDCS only"),
                                  "Double sham" = "Double sham"),
    condition_iBT = as.factor(ifelse(
      condition == "Double-active", 1, 0)),
    benzo_ddd_y = as.logical(as.numeric(benzo_ddd)),
    family_hist = (depression_fam == 1) + (bd_fam == 1) + (schizo_fam == 1) + 
      (alcohol_fam == 1) + (drug_fam == 1) + (anxiety_fam == 1) + 
      (ocd_fam == 1) + (dementia_fam == 1),
    family_hist_y = as.logical(family_hist),
    edin_righthanded_y = calc_edin > 60,
        # Code if participants and raters guessed correctly
    acc_ptes_subs = factor(case_when(
      condition_tdcs == "tDCS" & blind_tdcs == 0 ~ "wrong",
      condition_tdcs == "Double sham" & blind_tdcs == 1 ~ "wrong",
      condition_tdcs == "tDCS" & blind_tdcs == 1 ~ "correct",
      condition_tdcs == "Double sham" & blind_tdcs == 0 ~ "correct"),
      levels = c("wrong", "correct")),
    acc_ptes_rtrs = factor(case_when(
      condition_tdcs == "tDCS" & blind_tdcs_eval == 0 ~ "wrong",
      condition_tdcs == "Double sham" & blind_tdcs_eval == 1 ~ "wrong",
      condition_tdcs == "tDCS" & blind_tdcs_eval == 1 ~ "correct",
      condition_tdcs == "Double sham" & blind_tdcs_eval == 0 ~ "correct")),
    acc_ibt_subs = factor(case_when(
      condition == "Double-active" & blind_cbt == 0 ~ "wrong",
      condition != "Double-active" & blind_cbt == 1 ~ "wrong",
      condition == "Double-active" & blind_cbt == 1 ~ "correct",
      condition != "Double-active" & blind_cbt == 0 ~ "correct")),  
    acc_ibt_rtrs = factor(case_when(
      condition == "Double-active" & blind_cbt_eval == 0 ~ "wrong",
      condition != "Double-active" & blind_cbt_eval == 1 ~ "wrong",
      condition == "Double-active" & blind_cbt_eval == 1 ~ "correct",
      condition != "Double-active" & blind_cbt_eval == 0 ~ "correct")), 
  # Create scale sum scores 
  hamd_total = hamd_1 + hamd_2 + hamd_3 + hamd_4 + 
     hamd_5 + hamd_6 + hamd_7 + hamd_8 + hamd_9 + 
     hamd_10 + hamd_11 + hamd_12 + hamd_13 + hamd_14 + 
     hamd_15 + hamd_16 + hamd_17,
         
   madrs_total = madrs_1 + madrs_2 + madrs_3 + madrs_4 + madrs_5 + 
     madrs_6 + madrs_7 + madrs_8 + madrs_9 + madrs_10,
   
   hama_total = hama_1 + hama_2 + hama_3 + hama_4 + hama_5 + hama_6 + 
     hama_7 + hama_8 + hama_9 + hama_10 + hama_11 + hama_12 + 
     hama_13 + hama_14,
   
   bdi_total = bdi_1 + bdi_2 + bdi_3 + bdi_4 + bdi_5 + bdi_6 + bdi_7 + 
     bdi_8 + bdi_9 + bdi_10 + bdi_11 + bdi_12 + bdi_13 + bdi_14 + 
     bdi_15 + bdi_16_calc + bdi_17 + bdi_18_calc + bdi_19 + bdi_20 + 
     bdi_21,
   
   panas_positive_total = panas_amavel + panas_animado + 
     panas_apaixonado + panas_determinado + panas_dinamico + 
     panas_entusiasmado + panas_forte + panas_inspirado + 
     panas_orgulhoso + panas_vigoroso,
   
   panas_negative_total = panas_aflito + panas_amedrontado + 
     panas_angustiado + panas_humilhado + panas_incomodado + 
     panas_inquieto + panas_irritado + 
     panas_nervoso + panas_perturbado + panas_rancoroso, 
   
   # Portuguese version (slightly different from English):
   stai_s_total = 5-idatee_1 + 5-idatee_2 + idatee_3 + idatee_4 +
     5-idatee_5 + idatee_6 + idatee_7 + 5-idatee_8 + idatee_9 +
     5-idatee_10 + 5-idatee_11 + idatee_12 + idatee_13 + idatee_14 +
     5-idatee_15 + 5-idatee_16 + idatee_17 + idatee_18 + 5-idatee_19 +
     5-idatee_20,
   
   stai_t_total = 5-idatet_1 + idatet_2 + idatet_3 + idatet_4 + 
     idatet_5 + 5-idatet_6 + 5-idatet_7 + idatet_8 + idatet_9 + 
     5-idatet_10 + idatet_11 + idatet_12 + 5-idatet_13 + idatet_14 +
     idatet_15 + 5-idatet_16 + idatet_17 + idatet_18 + 5-idatet_19 +
     idatet_20   
  ) %>%
  group_by(subject) %>%
  # fill in NAs for time-independent information
  fill(c(record_id2:medicaes_ddd_complete, 
         acc_ptes_subs:acc_ibt_rtrs,
         blind_tdcs_eval:blind_cbt_vas_eval,
         blind_tdcs:blind_cbt_vas,
         benzo_ddd_y), 
       .direction = "downup") %>%
  # Create variables for response and remission (only HDRS and MADRS)
  mutate(
    hamd_total_bl = hamd_total[week == 0],
    madrs_total_bl = madrs_total[week == 0],
    madrs_response = as.numeric(madrs_total <= 0.5 * madrs_total_bl),
    hamd_response = as.numeric(hamd_total <= 0.5 * hamd_total_bl),
    ) %>%
  ungroup() %>%
  mutate(
    madrs_remission = as.numeric(madrs_total <= 10),
    hamd_remission = as.numeric(hamd_total <= 7),
    increased_mood_g = side_g_12,
    increased_mood_yn = side_12,
    # side effect variable coding: 
    # - side without suffix: presence of event
    # - "_g": gravidade/severity
    # - "_r": event's relation to brain stimulation (rated by patient)
    # Cleaning "_g" variables: 
    # If the patient indicates there is no relation to stimulation, 
    # replace all  values of severity with 0 (not in scale). 
    side_g_1 = ifelse(is.na(side_g_1), NA, 
                      ifelse(side_r_1 == 1, 0, side_g_1)),
    side_g_2 = ifelse(is.na(side_g_2), NA, 
                      ifelse(side_r_2 == 1, 0, side_g_2)),
    side_g_3 = ifelse(is.na(side_g_3), NA, 
                      ifelse(side_r_3 == 1, 0, side_g_3)),
    side_g_4 = ifelse(is.na(side_g_4), NA, 
                      ifelse(side_r_4 == 1, 0, side_g_4)),
    side_g_5 = ifelse(is.na(side_g_5), NA, 
                      ifelse(side_r_5 == 1, 0, side_g_5)),
    side_g_6 = ifelse(is.na(side_g_6), NA, 
                      ifelse(side_r_6 == 1, 0, side_g_6)),
    side_g_7 = ifelse(is.na(side_g_7), NA, 
                      ifelse(side_r_7 == 1, 0, side_g_7)),
    side_g_8 = ifelse(is.na(side_g_8), NA, 
                      ifelse(side_r_8 == 1, 0, side_g_8)),
    side_g_9 = ifelse(is.na(side_g_9), NA, 
                      ifelse(side_r_9 == 1, 0, side_g_9)),
    side_g_10 = ifelse(is.na(side_g_10), NA, 
                       ifelse(side_r_10 == 1, 0, side_g_10)),
    side_g_11 = ifelse(is.na(side_g_11), NA, 
                       ifelse(side_r_11 == 1, 0, side_g_11)),
    side_g_12 = ifelse(is.na(side_g_12), NA, 
                       ifelse(side_r_12 == 1, 0, side_g_12)),
    side_g_13 = ifelse(is.na(side_g_13), NA, 
                       ifelse(side_r_13 == 1, 0, side_g_13)),
    side_g_14 = ifelse(is.na(side_g_14), NA, 
                       ifelse(side_r_14 == 1, 0, side_g_14)),
    side_g_15 = ifelse(is.na(side_g_15), NA, 
                       ifelse(side_r_15 == 1, 0, side_g_15)),
    side_g_16 = ifelse(is.na(side_g_16), NA, 
                       ifelse(side_r_16 == 1, 0, side_g_16)),
    # Cleaning presence variables: If a side effect is present, but there is no 
    # relation to stimulation ("_r"), replace with 0. A further 
    # complication here is that side_r is NA if side was zero, which needs to be t
    # accounted for, as otherwise it would replace side = NA with side = zero.
    side_1 = as.factor(ifelse(is.na(side_1), NA, 
                              ifelse(side_g_1 == 1 | side_r_1 == 1 | is.na(side_r_1), 0, side_1))),
    side_2 = as.factor(ifelse(is.na(side_2), NA, 
                              ifelse(side_g_2 == 1 | side_r_2 == 1 | is.na(side_r_2), 0, side_2))),
    side_3 = as.factor(ifelse(is.na(side_3), NA, 
                              ifelse(side_g_3 == 1 | side_r_3 == 1 | is.na(side_r_3), 0, side_3))),
    side_4 = as.factor(ifelse(is.na(side_4), NA, 
                              ifelse(side_g_4 == 1 | side_r_4 == 1 | is.na(side_r_4), 0, side_4))),
    side_5 = as.factor(ifelse(is.na(side_5), NA, 
                              ifelse(side_g_5 == 1 | side_r_5 == 1 | is.na(side_r_5), 0, side_5))),
    side_6 = as.factor(ifelse(is.na(side_6), NA, 
                              ifelse(side_g_6 == 1 | side_r_6 == 1 | is.na(side_r_6), 0, side_6))),
    side_7 = as.factor(ifelse(is.na(side_7), NA, 
                              ifelse(side_g_7 == 1 | side_r_7 == 1 | is.na(side_r_7), 0, side_7))),
    side_8 = as.factor(ifelse(is.na(side_8), NA, 
                              ifelse(side_g_8 == 1 | side_r_8 == 1 | is.na(side_r_8), 0, side_8))),
    side_9 = as.factor(ifelse(is.na(side_9), NA, 
                              ifelse(side_g_9 == 1 | side_r_9 == 1 | is.na(side_r_9), 0, side_9))),
    side_10 = as.factor(ifelse(is.na(side_10), NA, 
                               ifelse(side_g_10 == 1 | side_r_10 == 1 | is.na(side_r_10), 0, side_10))),
    side_11 = as.factor(ifelse(is.na(side_11), NA, 
                               ifelse(side_g_11 == 1 | side_r_11 == 1 | is.na(side_r_11), 0, side_11))),
    side_12 = as.factor(ifelse(is.na(side_12), NA, 
                               ifelse(side_g_12 == 1 | side_r_12 == 1 | is.na(side_r_12), 0, side_12))),
    side_13 = as.factor(ifelse(is.na(side_13), NA, 
                               ifelse(side_g_13 == 1 | side_r_13 == 1 | is.na(side_r_13), 0, side_13))),
    side_14 = as.factor(ifelse(is.na(side_14), NA, 
                               ifelse(side_g_14 == 1 | side_r_14 == 1 | is.na(side_r_14), 0, side_14))),
    side_15 = as.factor(ifelse(is.na(side_15), NA, 
                               ifelse(side_g_15 == 1 | side_r_15 == 1 | is.na(side_r_15), 0, side_15))),
    side_16 = as.factor(ifelse(is.na(side_16), NA, 
                               ifelse(side_g_16 == 1 | side_r_16 == 1 | is.na(side_r_16), 0, side_16)))
  ) %>%
  rowwise() %>%
  mutate(
    mild_side_fx = ifelse(if_all(side_1:side_r_16, is.na), NA, 
                          sum(c_across(all_of(severities)) %in% 2)),
    moderate_side_fx = ifelse(if_all(side_1:side_r_16, is.na), NA, 
                              sum(c_across(all_of(severities)) %in% 3)),
    severe_side_fx = ifelse(if_all(side_1:side_r_16, is.na), NA, 
                            sum(c_across(all_of(severities)) %in% 4)),    
    no_side_fx = !any(mild_side_fx, moderate_side_fx, severe_side_fx),
    usability = 100 - mean(c(vas_psylect1, vas_psylect2, vas_psylect3,
                             vas_psylect4, vas_psylect5)),
  ) %>%
  ungroup() %>%
  mutate(across(all_of(side_fx),
                as.logical,
                .names = "{col}_lgcl")) %>%
  relocate(c(all_of(side_fx), ends_with("_lgcl")), .after = side_r_16) 

# Clean medication data ================

meds <- meds %>%
  rename(email = identification,
         atypical_AP = `atypical AP`) %>%
  mutate(email = tolower(email)) %>%
  rowwise() %>%
  mutate(any_AD_y = any(ssri, snri, bupropion, other_ad)) %>%
  ungroup() 


# Merge screening, trial, and medication data ======
psylect <- merge(screening, trials, 
                 by = "record_id_sc", 
                 all = TRUE, 
                 # Add suffix _sc to screening data if var names overlapping
                 suffixes = c("_sc", "")) %>%
  mutate(email = tolower(email)) %>%
  full_join(meds, by = "email") %>%
  # anonymize data
  select(-c(record_id2a, record_id2c, redcap_event_name_edit,
            flow_id, equipment_number, birthplace, email, email_sc,
            ends_with("_complete"))) %>%
  # reorder
  select(subject, Groups, contains("condition"), week, everything())

# Check for columns with all missings, and drop them
drops <- names(which(colSums(is.na(psylect)) == nrow(psylect)))
psylect <- select(psylect, !all_of(drops))

# Create dataframe with endpoint data only ====
# E.g. for analysis of condition blinding, geomapping

endpoint <- filter(trials, week == 6) %>%
  mutate(
    condition_iBT = as.factor(ifelse(
      condition == "Double-active", 1, 0)),
    blind_tdcs_subj_acc = as.factor(case_when(
      condition_tdcs == "tDCS" & blind_tdcs == 1 ~ "correct",
      condition_tdcs == "tDCS" & blind_tdcs == 0 ~ "wrong",
      condition_tdcs == "Double sham" & blind_tdcs == 1 ~ "wrong",
      condition_tdcs == "Double sham" & blind_tdcs == 0 ~ "correct")),
    blind_tdcs_eval_acc = as.factor(case_when(
      condition_tdcs == "tDCS" & blind_tdcs_eval == 1 ~ "correct",
      condition_tdcs == "tDCS" & blind_tdcs_eval == 0 ~ "wrong",
      condition_tdcs == "Double sham" & blind_tdcs_eval == 1 ~ "wrong",
      condition_tdcs == "Double sham" & blind_tdcs_eval == 0 ~ "correct")),
    blind_cbt_subj_acc = as.factor(case_when(
      condition == "Double-active" & blind_cbt == 1 ~ "correct",
      condition == "Double-active" & blind_cbt == 0 ~ "wrong",
      condition != "Double-active" & blind_cbt == 1 ~ "wrong",
      condition != "Double-active" & blind_cbt == 0 ~ "correct")),  
    blind_cbt_eval_acc = as.factor(case_when(
      condition == "Double-active" & blind_cbt_eval == 1 ~ "correct",
      condition == "Double-active" & blind_cbt_eval == 0 ~ "wrong",
      condition != "Double-active" & blind_cbt_eval == 1 ~ "wrong",
      condition != "Double-active" & blind_cbt_eval == 0 ~ "correct")),
    # Do something with certainty
    blind_tdcs_subj_acc3 = factor(case_when(
      blind_etcc_vas <= 10 ~ "don't know",
      condition_tdcs == "tDCS" & blind_tdcs == 1 ~ "correct",
      condition_tdcs == "tDCS" & blind_tdcs == 0 ~ "wrong",
      condition_tdcs == "Double sham" & blind_tdcs == 1 ~ "wrong",
      condition_tdcs == "Double sham" & blind_tdcs == 0 ~ "correct")),
    blind_tdcs_eval_acc3 = factor(case_when(
      blind_etcc_vas_eval <= 10 ~ "don't know",
      condition_tdcs == "tDCS" & blind_tdcs_eval == 1 ~ "correct",
      condition_tdcs == "tDCS" & blind_tdcs_eval == 0 ~ "wrong",
      condition_tdcs == "Double sham" & blind_tdcs_eval == 1 ~ "wrong",
      condition_tdcs == "Double sham" & blind_tdcs_eval == 0 ~ "correct")),
    blind_cbt_subj_acc3 = factor(case_when(
      blind_cbt_vas <= 10 ~ "don't know",
      condition == "Double-active" & blind_cbt == 1 ~ "correct",
      condition == "Double-active" & blind_cbt == 0 ~ "wrong",
      condition != "Double-active" & blind_cbt == 1 ~ "wrong",
      condition != "Double-active" & blind_cbt == 0 ~ "correct")),  
    blind_cbt_eval_acc3 = factor(case_when(
      blind_cbt_vas_eval <= 10 ~ "don't know",
      condition == "Double-active" & blind_cbt_eval == 1 ~ "correct",
      condition == "Double-active" & blind_cbt_eval == 0 ~ "wrong",
      condition != "Double-active" & blind_cbt_eval == 1 ~ "wrong",
      condition != "Double-active" & blind_cbt_eval == 0 ~ "correct")),
    blind_tdcs = fct_rev(
      factor(blind_tdcs, labels = c("Guess sham tDCS", "Guess active tDCS"))),
    condition_tdcs = fct_rev(condition_tdcs),
    blind_DI = fct_rev(factor(blind_cbt, 
                              labels = c("Guess sham DI", "Guess active DI"))),
    condition_DI = fct_rev(factor(condition_iBT, 
                                  labels = c("sham DI", "active DI"))),
    blind_both_eval = as.factor(ifelse(
      blind_tdcs_eval_acc == "correct" & blind_cbt_eval_acc == "correct", 
      "correct", 
      "wrong")),
    blind_both_subj = as.factor(ifelse(
      blind_tdcs_subj_acc == "correct" & blind_cbt_subj_acc == "correct", 
      "correct", 
      "wrong")),
    blind_group = as.factor(case_when(
      blind_tdcs == "Guess sham tDCS" & blind_DI == "Guess sham DI" ~ "Guess double sham",
      blind_tdcs != "Guess sham tDCS" & blind_DI != "Guess sham DI" ~ "Guess double active",
      blind_tdcs != "Guess sham tDCS" & blind_DI == "Guess sham DI" ~ "Guess tDCS only",
      blind_tdcs == "Guess sham tDCS" & blind_DI != "Guess sham DI" ~ "Guess sham tDCS, active DI")),
    blind_group = fct_relevel(blind_group, c(
      "Guess double sham", "Guess double active",
      "Guess tDCS only", "Guess sham tDCS, active DI")),
    zipcode2 = ifelse(zipcode == "2725050", "2725050, Brazil", zipcode),
    zipcode2 = ifelse(zipcode == "6160460", "6160460, Brazil", zipcode2),
    zipcode = zipcode2
  ) %>%
  select(-zipcode2)
  

# get latitude and longitude
geocoded <- geocode(endpoint$zipcode, output = "latlon")

endpoint <- endpoint %>%
  cbind(geocoded) %>%
  # Alternative manual look-ups for failed automatic geocoding
  mutate(lat = case_when(zipcode == "08295-000" ~ -23.5502882,
                         zipcode == "06465-132" ~ -23.5113691,
                         zipcode == "12060-363" ~ -23.0162224,
                         zipcode == "13301-621" ~ -23.2452067,
                         zipcode == "12244-623" ~ -23.1876174,
                         zipcode == "07610-585" ~ -23.3848779,
                         zipcode == "12244-623 " ~ -23.1876174,
                         TRUE ~ lat),
         lon = case_when(zipcode == "08295-000" ~ -46.4622609,
                         zipcode == "06465-132" ~ -46.8729420,
                         zipcode == "12060-363" ~ -45.5594421,
                         zipcode == "13301-621" ~ -47.3113339,
                         zipcode == "12244-623" ~ -45.9597094,
                         zipcode == "07610-585" ~ -46.6650810,
                         zipcode == "12244-623 " ~ -45.9597094,
                         TRUE ~ lon))

# Save cleaned data frames ======
save(psylect, file = "data/psylect_clean.RData")

save(endpoint, file = "data/psylect_endpoint_clean.RData")

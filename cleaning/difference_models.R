# if final dataframes are not already in environment

#df <- read.csv("complete.csv")
#df_all <- read.csv("allcases.csv")

#data cleaning for regular models

tmt_df <- filter(df, df$`Treatment or Waitlist` == "T")

tmt_df <- group_by(tmt_df, record_id) %>%
  mutate(
    diff_EC = ECscore[redcap_event_name =="session_1_arm_1"] - ECscore,
    diff_BN = BNscore[redcap_event_name =="session_1_arm_1"] - BNscore,
    diff_RV = RVscore[redcap_event_name =="session_1_arm_1"] - RVscore,
    diff_AV = AVscore[redcap_event_name =="session_1_arm_1"] - AVscore,
    diff_global = global[redcap_event_name =="session_1_arm_1"] - global,
    diff_suscept = susceptibility[redcap_event_name =="session_1_arm_1"] - susceptibility, 
    diff_sever = severity[redcap_event_name =="session_1_arm_1"] - severity,
    diff_bene = benefits[redcap_event_name =="session_1_arm_1"] - benefits,
    diff_barr = barriers[redcap_event_name =="session_1_arm_1"] - barriers,
    diff_cues = cues_action[redcap_event_name =="session_1_arm_1"] - cues_action,
    diff_eff = efficacy[redcap_event_name =="session_1_arm_1"] - efficacy,
    diff_HHI = HHI[redcap_event_name =="session_1_arm_1"] - HHI
  ) %>% ungroup()

wl_df <- filter(df, df$`Treatment or Waitlist` == "W")

wl_df <- group_by(wl_df, record_id) %>%
  mutate(
    diff_EC = ECscore[redcap_event_name =="session_2_arm_1"] - ECscore,
    diff_BN = BNscore[redcap_event_name =="session_2_arm_1"] - BNscore,
    diff_RV = RVscore[redcap_event_name =="session_2_arm_1"] - RVscore,
    diff_AV = AVscore[redcap_event_name =="session_2_arm_1"] - AVscore,
    diff_global = global[redcap_event_name =="session_2_arm_1"] - global,
    diff_suscept = susceptibility[redcap_event_name =="session_2_arm_1"] - susceptibility, 
    diff_sever = severity[redcap_event_name =="session_2_arm_1"] - severity,
    diff_bene = benefits[redcap_event_name =="session_2_arm_1"] - benefits,
    diff_barr = barriers[redcap_event_name =="session_2_arm_1"] - barriers,
    diff_cues = cues_action[redcap_event_name =="session_2_arm_1"] - cues_action,
    diff_eff = efficacy[redcap_event_name =="session_2_arm_1"] - efficacy,
    diff_HHI = HHI[redcap_event_name =="session_2_arm_1"] - HHI
  ) %>% ungroup()

diff_tmt <- tmt_df %>% select(record_id, diff_EC:diff_HHI)

diff_df <- rbind(filter(tmt_df, event == "session_2_arm_1"),filter(wl_df, event == "session_3_arm_1"))

fit_diff_aphab <- lm(diff_global ~ diff_suscept + diff_sever + diff_bene + diff_barr + diff_cues + diff_eff, data = diff_df)
summary(fit_diff_aphab)

fit_diff_hhi <- lm(diff_HHI ~ diff_suscept + diff_sever + diff_bene + diff_barr + diff_cues + diff_eff, data = diff_df)
summary(fit_diff_hhi)


#filter(df, df$`Treatment or Waitlist` == "T")
diff_tmt <- tmt_df %>% select(record_id, diff_EC:diff_HHI)
base_tmt <- inner_join(filter(df, event == "session_1_arm_1" & `Treatment or Waitlist` == "T"), diff_tmt, by = "record_id")

diff_wl <- wl_df %>% select(record_id, diff_EC:diff_HHI)
base_wl <- inner_join(filter(df, event == "session_2_arm_1" & `Treatment or Waitlist` == "W"), diff_wl, by = "record_id")

base_diff_df <- rbind(base_tmt, base_wl)


fit_base_aphab <- lm(diff_global ~ susceptibility + severity + benefits + barriers + cues_action + efficacy + HHI + global, data = base_diff_df)
summary(fit_base_aphab)

fit_base_aphab_2 <- lm(diff_global ~ susceptibility + severity + benefits + barriers + cues_action + efficacy + HHI, data = base_diff_df)
summary(fit_base_aphab_2)

fit_base_HHI <- lm(diff_HHI ~ susceptibility + severity + benefits + barriers + cues_action + efficacy + HHI + global, data = base_diff_df)
summary(fit_base_HHI)

fit_base_HHI_2 <- lm(diff_HHI ~ susceptibility + severity + benefits + barriers + cues_action + efficacy + global, data = base_diff_df)
summary(fit_base_HHI_2)

wl_control <- filter(df, df$`Treatment or Waitlist` == "W")

wl_control <- group_by(wl_df, record_id) %>%
  mutate(
    diff_EC = ECscore[redcap_event_name =="session_1_arm_1"] - ECscore,
    diff_BN = BNscore[redcap_event_name =="session_1_arm_1"] - BNscore,
    diff_RV = RVscore[redcap_event_name =="session_1_arm_1"] - RVscore,
    diff_AV = AVscore[redcap_event_name =="session_1_arm_1"] - AVscore,
    diff_global = global[redcap_event_name =="session_1_arm_1"] - global,
    diff_suscept = susceptibility[redcap_event_name =="session_1_arm_1"] - susceptibility, 
    diff_sever = severity[redcap_event_name =="session_1_arm_1"] - severity,
    diff_bene = benefits[redcap_event_name =="session_1_arm_1"] - benefits,
    diff_barr = barriers[redcap_event_name =="session_1_arm_1"] - barriers,
    diff_cues = cues_action[redcap_event_name =="session_1_arm_1"] - cues_action,
    diff_eff = efficacy[redcap_event_name =="session_1_arm_1"] - efficacy,
    diff_HHI = HHI[redcap_event_name =="session_1_arm_1"] - HHI
  ) %>% ungroup()

(t_test_HHI <- t.test(filter(tmt_df, redcap_event_name == "session_2_arm_1")$diff_HHI, filter(wl_control, redcap_event_name == "session_2_arm_1")$diff_HHI, alternative = "two.sided"))

(t_test_APHAB <- t.test(filter(tmt_df, redcap_event_name == "session_2_arm_1")$diff_global, filter(wl_control, redcap_event_name == "session_2_arm_1")$diff_global, alternative = "two.sided"))


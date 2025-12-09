# load packages and data

knitr::opts_chunk$set(warning = FALSE, message = FALSE)
library(magrittr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(janitor)
library(stringr)

rm(list = ls())

df <- read.csv("final_data_251023-complete.csv")
df_all <- read.csv("final_data_251023-allcases.csv")

df$HHI <- df$HHI*(-2)+60

aphab_vars <- c("ECscore","BNscore","RVscore","AVscore","global")
hbq_vars   <- c("susceptibility","severity","benefits","barriers","cues_action","efficacy")
hhi_vars   <- c("HHI")

response_vars <- intersect(
  c(aphab_vars, hbq_vars, hhi_vars),
  names(df)
)

# create subgroups

tmt_df <- filter(df, df$`Treatment.or.Waitlist` == "T")
wl_df <- filter(df, df$`Treatment.or.Waitlist` == "W")

# keep only record_ids that have both session_1_arm_1 and session_1_arm_2
tmt_df <- tmt_df %>%
  group_by(record_id) %>%
  filter(all(c("session_1_arm_1", "session_2_arm_1") %in% redcap_event_name)) %>%
  ungroup()

# keep only record_ids that have both session_2_arm_1 and session_2_arm_2
wl_df <- wl_df %>%
  group_by(record_id) %>%
  filter(all(c("session_1_arm_1", "session_2_arm_1", "session_3_arm_1") %in% redcap_event_name)) %>%
  ungroup()

# calculate differences for response variables and create the new datasets for modelling

tmt_base <- tmt_df %>%
  filter(redcap_event_name == "session_1_arm_1") %>%
  select("record_id", response_vars) %>%
  rename_with(~ paste0(.x, "_base"), all_of(response_vars))

tmt_follow <- tmt_df %>%
  filter(redcap_event_name == "session_2_arm_1")

tmt_merged <- tmt_base %>%
  dplyr::left_join(tmt_follow, by = c("record_id"))

for (v in response_vars) {
  tmt_merged[[paste0(v, "_diff")]] <- tmt_merged[[v]] - tmt_merged[[paste0(v, "_base")]]
}

tmt_diff <- tmt_merged %>%
  dplyr::select(record_id, dplyr::ends_with("_diff"))

wl_base <- wl_df %>%
  filter(redcap_event_name == "session_1_arm_1") %>%
  select("record_id", response_vars) %>%
  rename_with(~ paste0(.x, "_base"), all_of(response_vars))

wl_follow <- wl_df %>%
  filter(redcap_event_name == "session_2_arm_1")

wl_merged <- wl_base %>%
  dplyr::left_join(wl_follow, by = c("record_id"))

for (v in response_vars) {
  wl_merged[[paste0(v, "_diff")]] <- wl_merged[[v]] - wl_merged[[paste0(v, "_base")]]
}

wl_diff <- wl_merged %>%
  dplyr::select(record_id, dplyr::ends_with("_diff"))

# pairs plot of the response variables in tmt_diff and wl_diff to check for multicollinearity

if (!requireNamespace("GGally", quietly = TRUE)) install.packages("GGally")
library(GGally)
tmt_pairs_df <- tmt_diff %>% select(-record_id) %>% drop_na()
wl_pairs_df  <- wl_diff  %>% select(-record_id) %>% drop_na()
tmt_pairs_plot <- GGally::ggpairs(
  tmt_pairs_df,
  upper = list(continuous = wrap("cor", size = 4)),
  lower = list(continuous = wrap("smooth", alpha = 0.3, size = 0.2)),
  diag  = list(continuous = "densityDiag")
)
print(tmt_pairs_plot)
wl_pairs_plot <- GGally::ggpairs(
  wl_pairs_df,
  upper = list(continuous = wrap("cor", size = 4)),
  lower = list(continuous = wrap("smooth", alpha = 0.3, size = 0.2)),
  diag  = list(continuous = "densityDiag")
)
print(wl_pairs_plot)

# modelling

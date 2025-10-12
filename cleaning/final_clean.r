# Load libray

library(dplyr)
library(tidyr)
library(tidyverse)
library(janitor)
library(stringr)

# Load data
df <- readr::read_csv("data.csv") %>%
  janitor::clean_names()

# Share demographic informations across rows of the same record_id

demo_cols <- c(
  "id","date","dob","age", "gender",
  "ethnicity_1","ethnicity_2","ethnicity_3","ethnicity_4",
  "ethnicity_5","ethnicity_6","ethnicity_7",
  "other_ethnicities_please_s","language"
)

df_filled <- df %>%
  group_by(record_id) %>%
  fill(all_of(demo_cols), .direction = "downup") %>%
  ungroup()

# split the df into df on each questionnaire

cols_with_prefix <- function(nms, prefix) {
  nms[str_detect(nms, regex(paste0("^", prefix, ""), ignore_case = TRUE))]
}

nms <- names(df_filled)

# Simple prefix groups
aphab_cols <- nms[str_starts(nms, "aphab")]
hbq_cols   <- nms[str_starts(nms, "hbq_")]
hhia_cols  <- nms[str_starts(nms, "hhia")]
hhie_cols  <- nms[str_starts(nms, "s_hhie")]

# aphab sub-groups by start ("aphab") + specific endings
aphab_1_cols  <- nms[str_detect(nms, "^aphab.*eng_aided$")]
aphab_2_cols      <- nms[str_detect(nms, "^aphab.*eng_v2$")]
aphab_3_cols <- nms[str_detect(nms, "^aphab.*aided_v4$")]
aphab_4_cols      <- nms[str_detect(nms, "^aphab.*_eng$")]

# hbq groups
hbq_1_cols <- nms[str_detect(nms, "^hbq_.*eng_v2_v4$")]
hbq_2_cols <- nms[str_detect(nms, "^hbq_.*_eng_v2$")]
hbq_3_cols <- nms[str_detect(nms, "^hbq_.*_eng$")]
hbq_4_cols <- setdiff(hbq_cols, c(hbq_1_cols, hbq_2_cols, hbq_3_cols))

# hhia groups, problems with hhia_3_cols, undistinguishable
hhia_1_cols <- nms[str_detect(nms, "^hhia.*_v2_v4$")]
hhia_2_cols <- nms[str_detect(nms, "^hhia.*_v2$")]
hhia_3_cols <- setdiff(hhia_cols, union(hhia_1_cols, hhia_2_cols))

# hhi4 groups
hhie_1_cols <- nms[str_detect(nms, "^s_hhie.*_v2_v4$")]
hhie_2_cols <- nms[str_detect(nms, "^s_hhie.*_eng$")]
hhie_3_cols <- setdiff(hhie_cols, union(hhie_1_cols, hhie_2_cols))

demo_cols <- intersect(demo_cols, nms)

source("questionnaire_summary_functions.R")

# ---- Apply questionnaire summary functions ----
aphab1_result <- aphab_summary(df[, aphab_1_cols, drop = FALSE])
aphab2_result <- aphab_summary(df[, aphab_2_cols, drop = FALSE])
aphab3_result <- aphab_summary(df[, aphab_3_cols, drop = FALSE])
aphab4_result <- aphab_summary(df[, aphab_4_cols, drop = FALSE])

hbp1_result   <- hbq_summary(df[, hbq_1_cols, drop = FALSE])  
hbp2_result   <- hbq_summary(df[, hbq_2_cols, drop = FALSE]) 
hbp3_result   <- hbq_summary(df[, hbq_3_cols, drop = FALSE]) 
hbp4_result   <- hbq_summary(df[, hbq_4_cols, drop = FALSE]) 

hhia1_result  <- hhia_summary(df[, hhia_1_cols, drop = FALSE])
hhia2_result  <- hhia_summary(df[, hhia_2_cols, drop = FALSE])

hhie1_result  <- hhie_summary(df[, hhie_1_cols, drop = FALSE])
hhie2_result  <- hhie_summary(df[, hhie_2_cols, drop = FALSE])

# ---- Combine everything ----
df_final <- df |>
  select(any_of(demo_cols)) |>
  mutate(
    aphab1_result = aphab1_result,
    aphab2_result = aphab2_result,
    aphab3_result = aphab3_result,
    aphab4_result = aphab4_result,
    hbp1_result   = hbp1_result,
    hbp2_result   = hbp2_result,
    hbp3_result   = hbp3_result,
    hbp4_result   = hbp4_result,
    hhia1_result  = hhia1_result,
    hhia2_result  = hhia2_result,
    hhie1_result  = hhie1_result,
    hhie1_result  = hhie2_result,
  ) |>
  select(any_of(c(
    demo_cols,
    "aphab_result", "hbp_result", "hhia_result", "hhie_result"
  )))




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

# helpers

take_first_n_cols <- function(d, n = 26) {
  d[, seq_len(min(n, ncol(d))), drop = FALSE]
}

safe_select <- function(d, cols) {
  d[, intersect(cols, names(d)), drop = FALSE]
}

# Wrapper so hbq_summary only ever sees the first 26 columns
hbq_summary_26 <- function(d) {
  hbq_summary(take_first_n_cols(d, 26))
}

# Optionally, general safety wrapper to avoid calling on 0-col data
safe_call <- function(fun, d) {
  if (is.null(d) || ncol(d) == 0) return(NULL)
  fun(d)
}



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

# hhia groups
hhia_1_cols <- nms[str_detect(nms, "^hhia.*_v2_v4$")]
hhia_2_cols <- nms[str_detect(nms, "^hhia.*_v2$")]

hhia_3plus_cols <- setdiff(hhia_cols, union(hhia_1_cols, hhia_2_cols))


hhia_3_cols <- hhia_3plus_cols[seq_len(min(10, length(hhia_3plus_cols)))]
hhia_4_cols <- hhia_3plus_cols[-seq_len(min(10, length(hhia_3plus_cols)))]


# hhie groups
hhie_1_cols <- nms[str_detect(nms, "^s_hhie.*_v2_v4$")]
hhie_2_cols <- nms[str_detect(nms, "^s_hhie.*_eng$")]
hhie_3_cols <- nms[str_detect(nms, "^s_hhie.*_eng_v2$")]
hhie_4_cols <- setdiff(hhie_cols, union(union(hhie_1_cols, hhie_2_cols), hhie_3_cols))



demo_cols <- intersect(demo_cols, nms)

source("questionnaire_summary_functions.R")

# ---- Apply questionnaire summary functions ----
aphab1_result <- aphab_summary(df[, aphab_1_cols, drop = FALSE])
aphab2_result <- aphab_summary(df[, aphab_2_cols, drop = FALSE])
aphab3_result <- aphab_summary(df[, aphab_3_cols, drop = FALSE])
aphab4_result <- aphab_summary(df[, aphab_4_cols, drop = FALSE])

hbq1_result <- safe_call(hbq_summary_26, safe_select(df_filled, hbq_1_cols)) %>% mutate_all(~ifelse(is.nan(.), NA, .))
hbq2_result <- safe_call(hbq_summary_26, safe_select(df_filled, hbq_2_cols)) %>% mutate_all(~ifelse(is.nan(.), NA, .))
hbq3_result <- safe_call(hbq_summary_26, safe_select(df_filled, hbq_3_cols)) %>% mutate_all(~ifelse(is.nan(.), NA, .))
hbq4_result <- safe_call(hbq_summary_26, safe_select(df_filled, hbq_4_cols)) %>% mutate_all(~ifelse(is.nan(.), NA, .))

hhia1_result  <- hhia_summary(df[, hhia_1_cols, drop = FALSE])
hhia2_result  <- hhia_summary(df[, hhia_2_cols, drop = FALSE])
hhia3_result  <- hhia_summary(df[, hhia_3_cols, drop = FALSE])
hhia4_result  <- hhia_summary(df[, hhia_4_cols, drop = FALSE])

hhie1_result  <- hhie_summary(df[, hhie_1_cols, drop = FALSE])
hhie2_result  <- hhie_summary(df[, hhie_2_cols, drop = FALSE])
hhie3_result  <- hhie_summary(df[, hhie_3_cols, drop = FALSE])
hhie4_result  <- hhie_summary(df[, hhie_4_cols, drop = FALSE])

# ---- Combine everything ----
# df_final <- df |>
#   select(any_of(demo_cols)) |>
#   mutate(
#     aphab1_result = aphab1_result,
#     aphab2_result = aphab2_result,
#     aphab3_result = aphab3_result,
#     aphab4_result = aphab4_result,
#     hbq1_result   = hbq1_result,
#     hbq2_result   = hbq2_result,
#     hbq3_result   = hbq3_result,
#     hbq4_result   = hbq4_result,
#     hhia1_result  = hhia1_result,
#     hhia2_result  = hhia2_result,
#     hhia3_result  = hhia3_result,
#     hhia4_result  = hhia4_result,
#     hhie1_result  = hhie1_result,
#     hhie1_result  = hhie2_result,
#         hhie3_result  = hhie3_result,
#     hhie4_result  = hhie4_result,
#   ) |>
#   select(any_of(c(
#     demo_cols,
#     "aphab_result", "hbp_result", "hhia_result", "hhie_result"
#   )))


#full_df <- cbind(df_filled[,1:19], aphab1_result, aphab2_result, aphab3_result, aphab4_result, hbq1_result, hbq2_result, hbq3_result, hbq4_result, hhia1_result, hhia2_result, hhia3_result, hhia4_result, hhie1_result, hhie2_result, hhie3_result, hhie4_result)

#full_df %>% mutate_all(~ifelse(is.nan(.), NA, .))

rename_cols <- function(input_data, i=1) {
  temp_names <- names(input_data)
  temp_names <- paste(temp_names, i)
  colnames(input_data) <- temp_names
  return(input_data)
}

(rowSums(!is.na(full_df[,20:71])))

#we have some rows with multiple sets of scores, and it seems the demographic rows don't contain test data

#session 1, no repeat instrument is the bio/demo data
#session 1 repeat instruments: aphab#_eng: aphab_4, hbq_#_eng: hbq_3, hhia_#_eng_v2_hex: hhia_3, s_hhie#_eng: hhie_2
#session 2 either: aphab#eng_aided: aphab_1, hbq_#_eng_v2: hbq_2, (hhia_#_eng_v2_hex_v2: hhia_2 or s_hhie#_eng_v2: hhie_3)
#             or:  aphab#_eng_v2: aphab_2, hbq_#eng_v2_hex: hbq_4, (hhia_#_eng_v2_hex_v2_hex: hhia_4 or s_hhie#_eng_v2_hex: hhie_4 [or both?])
#session 3 contains aphab#_eng_aided_v4: aphab_3, hbq_#_eng_v2_v4: hbq_1, and either hhia_#_eng_v2_hex_v2_v4: hhia_1 or s_hhie#_eng_v2_v4: hhie_1


# tidy data format:

# ID  Session T/W Demo_cols aphab_cols  hbq_cols  hhia hhie

# remove repeat_instance column:  all test types for a single date will be in one row
# hhia or hhie will be blank, other tests should be consistent
# add treatment or waitlist column?

#session 1 combined: use joins?

sum(!is.na(df[,5]))

s1_demo <- cbind(df[,1:2],rep(NA, 221),df[,5:18]) %>%
  filter(!is.na(id))
names(s1_demo)[3] <- "Treatment or Waitlist"

s1_aphab <- cbind(df_filled[,1], aphab4_result) %>%
  filter(!is.na(ECscore))
s1_hbq <- cbind(df_filled[,1], hbq3_result) %>%
  filter(!is.na(severity))
s1_hhia <- cbind(df_filled[,1], hhia3_result) %>%
  filter(!is.na(hhia3_result))
s1_hhie <- cbind(df_filled[,1], hhie2_result) %>%
  filter(!is.na(hhie2_result))

#change to inner_join if we want to drop the blank rows
s1_full <- left_join(s1_demo, s1_aphab, by = "record_id") %>%
  left_join(s1_hbq, by = "record_id")

#do NOT change to inner join!
s1_full <- left_join(s1_full, s1_hhia, by = "record_id") %>%
  left_join(s1_hhie, by = "record_id")

names(s1_full)[29:30] <- c("hhia_sum","hhie_sum")



#session 2: separate by group, add treatment or waitlist col, then recombine

s2_mask_tmt <- !is.na(df_filled[,46])
s2_mask_wl <- !is.na(df_filled[,123])

session2_df_tmt <- cbind(df_filled[,1:2],rep("t", 221),df_filled[,5:18], aphab1_result, hbq2_result, hhia2_result, hhie3_result) %>%
  filter(!is.na(ECscore))

names(session2_df_tmt)[3] <- "Treatment or Waitlist"
names(session2_df_tmt)[29:30] <- c("hhia_sum","hhie_sum")

session2_df_wl <- cbind(df_filled[,1:2],rep("w", 221),df_filled[,5:18], aphab2_result, hbq4_result, hhia4_result, hhie4_result) %>%
  filter(!is.na(ECscore))

names(session2_df_wl)[3] <- "Treatment or Waitlist"
names(session2_df_wl)[29:30] <- c("hhia_sum","hhie_sum")

#session 3: all waitlist

session3_df <- cbind(df_filled[,1:2],rep("w", 221),df_filled[,5:18], aphab3_result, hbq1_result, hhia1_result, hhie1_result) %>%
  filter(redcap_event_name == "session_3_arm_1")

names(session3_df)[3] <- "Treatment or Waitlist"
names(session3_df)[29:30] <- c("hhia_sum","hhie_sum")

#combine all tables

final_tidy_data <- rbind(s1_full, session2_df_tmt, session2_df_wl, session3_df)


write.csv(final_tidy_data, file = "final_data.csv")

#manual edits to final table

final_tidy_data <- add_column(final_tidy_data, aided = rep(NA, 101), .after = "Treatment or Waitlist")

final_tidy_data$aided <- "U"
final_tidy_data$aided[final_tidy_data$`Treatment or Waitlist` == "t"] <- "A"
final_tidy_data$aided[final_tidy_data$redcap_event_name == "session_3_arm_1"] <- "A"

final_tidy_data$`Treatment or Waitlist`[final_tidy_data$id %in% session2_df_tmt$id] <- "T"
final_tidy_data$`Treatment or Waitlist`[final_tidy_data$id %in% session2_df_wl$id] <- "W"


#patient 20 session 1 hhie: row 19 col 31
final_tidy_data[19,31] <- 28

#patient 32 session 1 hhia: row 31 col 30
final_tidy_data[31,30] <- 6

#removing HHI values for those who filled out both
#note: cutoff age is 65

#patient 23 session 2
final_tidy_data[76,30] <- NA
#patient 43 session 2
final_tidy_data[83,30] <- NA
#patient 46 session 2
final_tidy_data[85,30] <- NA

final_tidy_data <- mutate(final_tidy_data, "HHI" = coalesce(final_tidy_data$hhia_sum, final_tidy_data$hhie_sum))

write.csv(final_tidy_data, file = "final_data_allcases.csv")

final_tidy_data_complete <- filter(final_tidy_data, !is.na(final_tidy_data$ECscore))
#remove pts 15, 38 (skipped s2), 25, 28 (missed questions on s2 HBQ), 28, 34, 36 (waitlist for s2, skipped s3)
final_tidy_data_complete <- filter(final_tidy_data_complete, !(record_id %in% c(15, 38, 25, 28, 34, 36)))

write.csv(final_tidy_data_complete, file = "final_data_complete.csv")






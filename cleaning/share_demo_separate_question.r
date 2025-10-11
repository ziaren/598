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

# clean and make compact the ethnicity columns

# eth_cols <- names(df_filled)[str_detect(names(df_filled), "^ethnicity___\\d+$")]
# if (length(eth_cols) == 0) {
#   eth_cols <- names(df)[str_detect(names(df_filled), "^ethnicity_\\d+$")]
# }
# 
# stopifnot("No ethnicity columns found" = length(eth_cols) == 7)
# 
# df_eth <- df_filled |>
#   select(-matches("^other_ethnicities")) |>
#   pivot_longer(
#     cols = all_of(eth_cols),
#     names_to = "ethnicity_name",
#     values_to = "checked",
#     values_transform = list(checked = as.character) # handle 0/1, TRUE/FALSE, etc.
#   ) |>
#   mutate(
#     ethnicity = as.integer(str_extract(ethnicity_name, "\\d+")),
#     checked_num = case_when(
#       checked %in% c("1", "TRUE", "True", "true") ~ 1L,
#       checked %in% c("0", "FALSE", "False", "false", "", NA) ~ 0L,
#       TRUE ~ suppressWarnings(as.integer(checked)) # fallback
#     )
#   ) |>
#   filter(checked_num == 1L) |>
#   select(-ethnicity_name, -checked, -checked_num)
# 
# df_eth

# split the df into df on each questionnaire

cols_with_prefix <- function(nms, prefix) {
  nms[str_detect(nms, regex(paste0("^", prefix, ""), ignore_case = TRUE))]
}

nms <- names(df_filled)
aphab_cols <- cols_with_prefix(nms, "aphab")
hbq_cols   <- cols_with_prefix(nms, "hbq_")
hhia_cols  <- cols_with_prefix(nms, "hhia_")
hhie_cols  <- cols_with_prefix(nms, "s_hhie")

demo_cols <- intersect(demo_cols, nms)

has_any_block <- function(d, cols) {
  if (length(cols) == 0) return(rep(FALSE, nrow(d)))
  rowSums(!is.na(d[, cols, drop = FALSE])) > 0
}

flag_aphab <- has_any_block(df_filled, aphab_cols)
flag_hbq   <- has_any_block(df_filled, hbq_cols)
flag_hhia  <- has_any_block(df_filled, hhia_cols)
flag_hhie  <- has_any_block(df_filled, hhie_cols)

df <- df_filled |>
  mutate(
    did_aphab = flag_aphab,
    did_hbq   = flag_hbq,
    did_hhia  = flag_hhia,
    did_hhie  = flag_hhie
  )

df_aphab <- df |>
  filter(did_aphab) |>
  select(any_of(demo_cols), any_of(aphab_cols))

df_hbq <- df |>
  filter(did_hbq) |>
  select(any_of(demo_cols), any_of(hbq_cols))

df_hhia <- df |>
  filter(did_hhia) |>
  select(any_of(demo_cols), any_of(hhia_cols))

df_hhie <- df |>
  filter(did_hhie) |>
  select(any_of(demo_cols), any_of(hhie_cols))

tibble(
  APHAB = sum(df$did_aphab),
  HBQ   = sum(df$did_hbq),
  HHIA  = sum(df$did_hhia),
  HHIE  = sum(df$did_hhie),
  Both_HHIA_HHIE = sum(df$did_hhia & df$did_hhie)
) |> print()

readr::write_csv(df_aphab, "aphab_only.csv")
readr::write_csv(df_hbq,   "hbq_only.csv")
readr::write_csv(df_hhia,  "hhia_only.csv")
readr::write_csv(df_hhie,  "hhie_only.csv")

# merge back

df_merged <- bind_rows(df_aphab, df_hbq, df_hhia, df_hhie) %>%
  distinct()

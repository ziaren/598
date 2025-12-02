library(dplyr)
df     <- read.csv("final_data_complete.csv", check.names = TRUE)
df_all <- read.csv("final_data_allcases.csv", check.names = TRUE)

aphab_vars <- c("ECscore","BNscore","RVscore","AVscore","global")
hbq_vars   <- c("susceptibility","severity","benefits","barriers","cues_action","efficacy")
hhi_vars   <- c("HHI")

response_vars <- intersect(
  c(aphab_vars, hbq_vars, hhi_vars),
  names(df)
)

tw_col <- if ("Treatment.or.Waitlist" %in% names(df)) {
  "Treatment.or.Waitlist"
} else if ("Treatment or Waitlist" %in% names(df)) {
  "Treatment or Waitlist"
} else {
  stop("Cannot find a 'Treatment or Waitlist' column in df")
}

if (!"redcap_event_name" %in% names(df)) {
  stop("Cannot find 'redcap_event_name' in df")
}

df$group  <- factor(df[[tw_col]], levels = c("T","W"))
df$event  <- factor(df$redcap_event_name)
df$gender <- factor(df$gender)
df$age    <- suppressWarnings(as.numeric(df$age))

# split into treat/wait by sessions

group_T <- df[df[[tw_col]] == "T", ]
group_W <- df[df[[tw_col]] == "W", ]

group_T_1 <- group_T %>% filter(event == "session_1_arm_1")
group_T_2 <- group_T %>% filter(event == "session_2_arm_1")
group_T_3 <- group_T %>% filter(event == "session_3_arm_1")

group_W_1 <- group_W %>% filter(event == "session_1_arm_1")
group_W_2 <- group_W %>% filter(event == "session_2_arm_1")
group_W_3 <- group_W %>% filter(event == "session_3_arm_1")

subgroups <- list(
  T_session_1 = group_T_1,
  T_session_2 = group_T_2,
  T_session_3 = group_T_3,
  W_session_1 = group_W_1,
  W_session_2 = group_W_2,
  W_session_3 = group_W_3
)

## sanity
cat("Subgroup sizes:\n")
print(sapply(subgroups, nrow))

# histogram
vars_to_plot <- unique(response_vars)
vars_to_plot <- vars_to_plot[vars_to_plot %in% names(df)]

cat("Variables to plot:\n")
print(vars_to_plot)

pdf("histogram_summary_by_subgroups.pdf", width = 7, height = 5)

if (length(vars_to_plot) == 0) {
  par(mfrow = c(1, 1), mar = c(4, 4, 3, 1))
  plot.new()
  title("No response variables found in df")
} else {
  
  for (v in vars_to_plot) {
    cat("Plotting variable:", v, "\n")
    x_all <- df[[v]]
    if (!is.numeric(x_all)) {
      cat("  -> skipped (not numeric)\n")
      next
    }

    all_x <- unlist(lapply(subgroups, function(g) g[[v]]))
    all_x <- all_x[is.finite(all_x)]
    
    if (length(all_x) == 0) {
      par(mfrow = c(1, 1), mar = c(4, 4, 3, 1))
      plot.new()
      title(main = paste("Histogram:", v))
      mtext("No finite numeric values in any subgroup", side = 3, line = 0.5, cex = 0.9)
      next
    }
    
    x_limits <- range(all_x)

    par(mfrow = c(2, 3), mar = c(4, 4, 3, 1))
    
    for (nm in names(subgroups)) {
      g <- subgroups[[nm]]
      x <- g[[v]]
      x_ok <- x[is.finite(x)]
      
      if (length(x_ok) > 0) {
        hist(
          x_ok,
          main = paste("Histogram:", v, "-", nm),
          cex.main = 0.8,
          xlab = v,
          xlim = x_limits,
          col = "grey",
          border = "white"
        )
      } else {
        plot.new()
        title(main = paste("Histogram:", v, "-", nm),cex.main = 0.8)
        mtext("No finite numeric values", side = 3, line = 0.5, cex = 0.9)
      }
    }
  }
}

dev.off()

cat("Done. PDF written to:", file.path(getwd(), "histogram_summary_by_subgroups.pdf"), "\n")

# Boxplots of various measures by time/condition

df <- df %>%
  mutate(
    redcap_event_name     = as.factor(redcap_event_name),
    Treatment.or.Waitlist = as.factor(Treatment.or.Waitlist),
    aided                 = as.factor(aided),
    subgroup = case_when(
      Treatment.or.Waitlist == "T" & redcap_event_name == "session_1_arm_1" ~ "T_session_1",
      Treatment.or.Waitlist == "T" & redcap_event_name == "session_2_arm_1" ~ "T_session_2",
      Treatment.or.Waitlist == "T" & redcap_event_name == "session_3_arm_1" ~ "T_session_3",
      Treatment.or.Waitlist == "W" & redcap_event_name == "session_1_arm_1" ~ "W_session_1",
      Treatment.or.Waitlist == "W" & redcap_event_name == "session_2_arm_1" ~ "W_session_2",
      Treatment.or.Waitlist == "W" & redcap_event_name == "session_3_arm_1" ~ "W_session_3",
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(subgroup = factor(
    subgroup,
    levels = c("T_session_1","T_session_2","T_session_3",
               "W_session_1","W_session_2","W_session_3")
  ))


df_long <- df %>%
  select(all_of(c(num_keep, "redcap_event_name",
                  "Treatment.or.Waitlist", "aided", "subgroup"))) %>%
  pivot_longer(cols = all_of(num_keep),
               names_to = "variable",
               values_to = "value")

plot_boxes_by <- function(group_var, ncol = 3) {
  ggplot(df_long,
         aes(x = .data[[group_var]], y = value,
             fill = .data[[group_var]])) +
    geom_boxplot(width = 0.7, outlier.alpha = 0.6, na.rm = TRUE) +
    facet_wrap(~ variable, scales = "free_y", ncol = ncol) +
    labs(title = paste("Response variables by", group_var),
         x = group_var, y = "Value") +
    theme_bw(base_size = 11) +
    theme(
      legend.position = "none",
      strip.text      = element_text(size = 9),
      axis.text.x     = element_text(angle = 25, hjust = 1)
    )
}

plot_boxes_subgroups <- function(ncol = 3) {
  ggplot(df_long %>% filter(!is.na(subgroup)),
         aes(x = subgroup, y = value, fill = subgroup)) +
    geom_boxplot(width = 0.7, outlier.alpha = 0.6, na.rm = TRUE) +
    facet_wrap(~ variable, scales = "free_y", ncol = ncol) +
    labs(title = "Response variables by subgroup (T/W × session)",
         x = "Subgroup", y = "Value") +
    theme_bw(base_size = 11) +
    theme(
      legend.position = "none",
      strip.text      = element_text(size = 9),
      axis.text.x     = element_text(angle = 25, hjust = 1)
    )
}

pdf("boxplots_response_vars_by_groups.pdf", width = 11, height = 8.5)
print(plot_boxes_subgroups())    
dev.off()


# Correlation matrix of all response variables
keep <- intersect(response_vars, names(df))
num_df <- df[keep]
num_df <- num_df[, sapply(num_df, is.numeric), drop = FALSE]

if (ncol(num_df) < 2) stop("Not enough numeric variables for correlation.")

C <- cor(num_df, use = "pairwise.complete.obs", method = "pearson")

corr_long <- as.data.frame(C) %>%
  tibble::rownames_to_column("Var1") %>%
  pivot_longer(-Var1, names_to = "Var2", values_to = "value")

pdf("correlation_matrix_response_vars.pdf", width = 8, height = 8)

ggplot(corr_long, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                       midpoint = 0, limit = c(-1, 1), name = "r") +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 9),
    panel.grid = element_blank()
  ) +
  labs(title = "Correlation Matrix of Response Variables",
       x = "", y = "")

dev.off()

# Pairs plot, within each scale , perhaps major responses?

keep <- intersect(response_vars, names(df))
num  <- df[keep]
num  <- num[, sapply(num, is.numeric), drop = FALSE]

stopifnot(ncol(num) >= 2)

pdf("pairs_simple.pdf", width = 9, height = 9)
pairs(num, main = "Pairs plot of response variables",
      pch = 19, cex = 0.5)  # small solid points
dev.off()

aphab_vars <- c("ECscore", "BNscore", "RVscore", "AVscore", "global")
hbq_vars   <- c("susceptibility", "severity", "benefits", "barriers", 
                "cues_action", "efficacy")

make_pairs_simple <- function(dat, vars, title) {
  keep <- intersect(vars, names(dat))
  num  <- dat[keep]
  num  <- num[, sapply(num, is.numeric), drop = FALSE]
  if (ncol(num) < 2) {
    message("Skipping ", title, ": not enough numeric variables.")
    return(invisible(NULL))
  }
  pairs(num, main = title, pch = 19, cex = 0.6)
}

pdf("pairs_aphab_hbq.pdf", width = 9, height = 9)
make_pairs_simple(df, aphab_vars, "APHAB Variables")
make_pairs_simple(df, hbq_vars, "HBQ Variables")
dev.off()

# Pre-post scatterplots, (or S1/S2 scatterplots), with colors for the treatment groups pre/post treatment, colors for treatment groups (Tx:  S1/S2, Wa: S2/S3 change) [more coding]

df <- df %>%
  mutate(session = str_extract(redcap_event_name, "session_\\d") %>%
           str_replace("session_", "S"))

response_vars <- c("ECscore","BNscore","RVscore","AVscore","global",
                   "susceptibility","severity","benefits","barriers",
                   "cues_action","efficacy","hhia_sum","hhie_sum","HHI")

wide_df <- df %>%
  select(id, Treatment.or.Waitlist, session, all_of(response_vars)) %>%
  pivot_wider(names_from = session, values_from = all_of(response_vars),
              names_sep = "_")

plot_prepost <- function(var, data) {
  tx_df <- data %>% filter(Treatment.or.Waitlist == "T")
  wa_df <- data %>% filter(Treatment.or.Waitlist == "W")
  
  # safely extract variable names for sessions
  s1 <- paste0(var, "_S1")
  s2 <- paste0(var, "_S2")
  s3 <- paste0(var, "_S3")
  
  # check existence
  cols_exist <- c(s1, s2, s3) %in% names(data)
  if (!any(cols_exist)) return(NULL)
  
  # combine both subplots in one frame for consistent scales
  range_all <- range(c(data[[s1]], data[[s2]], data[[s3]]), na.rm = TRUE)
  
  ggplot() +
    geom_point(data = tx_df, aes(x = .data[[s1]], y = .data[[s2]], color = "Tx (S1→S2)"),
               size = 2, alpha = 0.8) +
    geom_point(data = wa_df, aes(x = .data[[s2]], y = .data[[s3]], color = "Wa (S2→S3)"),
               size = 2, alpha = 0.8) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40") +
    scale_color_manual(values = c("Tx (S1→S2)" = "#1b9e77", "Wa (S2→S3)" = "#d95f02")) +
    coord_equal(xlim = range_all, ylim = range_all) +
    theme_bw(base_size = 12) +
    labs(title = paste("Pre-Post Scatter:", var),
         x = "Pre Score", y = "Post Score", color = "Group") +
    theme(legend.position = "top")
}

pdf("pre_post_scatterplots.pdf", width = 7, height = 6)
for (v in response_vars) {
  p <- plot_prepost(v, wide_df)
  if (!is.null(p)) print(p)
}
dev.off()



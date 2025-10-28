df <- read.csv("complete.csv")
df_all <- read.csv("allcases.csv")


aphab_vars <- c("ECscore","BNscore","RVscore","AVscore","global")
hbq_vars   <- c("susceptibility","severity","benefits","barriers","cues_action","efficacy")
hhi_vars   <- c("HHI")
response_vars <- intersect(
  c(aphab_vars, hbq_vars, hhi_vars),
  names(df)
)
df$group  <- factor(df$Treatment.or.Waitlist, levels = c("T","W"))
df$event  <- factor(df$redcap_event_name)
df$gender <- factor(df$gender)
df$age    <- suppressWarnings(as.numeric(df$age))

# histograms of various measures
vars_to_plot <- unique(c("age", "gender", response_vars))
vars_to_plot <- vars_to_plot[vars_to_plot %in% names(df)]

pdf("histogram_summary.pdf", width = 7, height = 5)
op <- par(mfrow = c(2, 2), mar = c(4, 4, 3, 1)) 

for (v in vars_to_plot) {
  x <- df[[v]]
  # numeric -> histogram
  if (is.numeric(x)) {
    x_ok <- x[is.finite(x)]
    if (length(x_ok)) {
      hist(x_ok,
           main = paste("Histogram:", v),
           xlab = v, col = "grey", border = "white")
    } else {
      plot.new(); title(main = paste("Histogram:", v))
      mtext("No finite numeric values", side = 3, line = 0.5, cex = 0.9)
    }
    next
  }
  # factors/characters -> bar plot of counts
  if (is.factor(x) || is.character(x)) {
    tab <- table(x, useNA = "ifany")
    if (length(tab)) {
      barplot(tab,
              main = paste("Counts:", v),
              xlab = v, ylab = "Frequency",
              col = "grey", border = "white", las = 2)
    } else {
      plot.new(); title(main = paste("Counts:", v))
      mtext("No values", side = 3, line = 0.5, cex = 0.9)
    }
    next
  }
  # fallback for unsupported types
  plot.new(); title(main = paste("Skip (type):", v))
}

par(op); dev.off()

# Boxplots of various measures by time/condition

df <- df %>%
  mutate(
    redcap_event_name = as.factor(redcap_event_name),
    Treatment.or.Waitlist = as.factor(Treatment.or.Waitlist),
    aided = as.factor(aided)
  )

keep <- intersect(response_vars, names(df))
if (length(keep) == 0) stop("None of response_vars are present in df.")
num_keep <- keep[sapply(df[keep], is.numeric)]
if (length(num_keep) == 0) stop("None of response_vars are numeric.")

df_long <- df %>%
  select(all_of(c(num_keep, "redcap_event_name", "Treatment.or.Waitlist", "aided"))) %>%
  pivot_longer(cols = all_of(num_keep), names_to = "variable", values_to = "value")

plot_boxes_by <- function(group_var, ncol = 3) {
  ggplot(df_long, aes(x = .data[[group_var]], y = value, fill = .data[[group_var]])) +
    geom_boxplot(width = 0.7, outlier.alpha = 0.6, na.rm = TRUE) +
    facet_wrap(~ variable, scales = "free_y", ncol = ncol) +
    labs(title = paste("Response variables by", group_var),
         x = group_var, y = "Value") +
    theme_bw(base_size = 11) +
    theme(
      legend.position = "none",
      strip.text = element_text(size = 9),
      axis.text.x = element_text(angle = 25, hjust = 1)
    )
}

pdf("boxplots_response_vars_by_groups.pdf", width = 11, height = 8.5)
print(plot_boxes_by("redcap_event_name"))
print(plot_boxes_by("Treatment.or.Waitlist"))
print(plot_boxes_by("aided"))
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


# Look at relationship between Age and response variables


# ============================================
# Shared helper functions
# ============================================

# Create the output folder tree for one task and return the paths.
create_output_dirs <- function(base_dir, subdirs = c("tables", "anova", "posthoc", "figures")) {
  dirs <- setNames(lapply(subdirs, function(s) file.path(base_dir, s)), subdirs)
  for (d in dirs) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  dirs
}

# --- Factor coding used throughout the analysis ------------------------------
# Locations are stored in metres in data/. The analysis uses short codes:
#   target / cue  : "t0.3m", "t1.3m", "t2.3m"
#   standard      : "near", "mid", "far"
#   distance pair : "near_mid", "near_far", "mid_far"
loc_levels_m <- c(0.3, 1.3, 2.3)
loc_code <- function(x) factor(sprintf("t%.1fm", x), levels = c("t0.3m", "t1.3m", "t2.3m"))
std_code <- function(x) factor(c("near", "mid", "far")[match(x, loc_levels_m)], levels = c("near", "mid", "far"))

localization_pair_levels <- c("near_mid", "near_far", "mid_far")
pair_code <- function(first_location, second_location) {
  a <- pmin(first_location, second_location); b <- pmax(first_location, second_location)
  factor(case_when(a == 0.3 & b == 1.3 ~ "near_mid",
                   a == 0.3 & b == 2.3 ~ "near_far",
                   TRUE                ~ "mid_far"),
         levels = localization_pair_levels)
}

# Cousineau (2005) / Morey (2008) within-participant 95% confidence interval.
# Used for the error bars/CI columns of every figure and table in the manuscript.
cm_ci <- function(data, dv, within_vars, id_var = "participant", level = 0.95) {
  n_subj     <- length(unique(data[[id_var]]))
  J          <- data %>% distinct(across(all_of(within_vars))) %>% nrow()
  t_crit     <- qt((1 + level) / 2, df = n_subj - 1)
  morey      <- sqrt(J / (J - 1))
  grand_mean <- mean(data[[dv]], na.rm = TRUE)

  data %>%
    group_by(across(all_of(id_var))) %>%
    mutate(.norm = .data[[dv]] - mean(.data[[dv]], na.rm = TRUE) + grand_mean) %>%
    ungroup() %>%
    group_by(across(all_of(within_vars))) %>%
    summarise(
      emmean   = mean(.data[[dv]], na.rm = TRUE),
      lower.CL = emmean - t_crit * morey * sd(.norm, na.rm = TRUE) / sqrt(n()),
      upper.CL = emmean + t_crit * morey * sd(.norm, na.rm = TRUE) / sqrt(n()),
      .groups  = "drop"
    )
}

# Convert an error-rate summary (0-1) into a correct-rate summary in percent,
# swapping the CI bounds accordingly.
to_correct_ratio <- function(df) {
  df %>%
    mutate(new_emmean = (1 - emmean) * 100, new_lower = (1 - upper.CL) * 100, new_upper = (1 - lower.CL) * 100) %>%
    select(-emmean, -lower.CL, -upper.CL) %>%
    rename(emmean = new_emmean, lower.CL = new_lower, upper.CL = new_upper)
}

# Format bayestestR::bayesfactor_inclusion() output as a plain data frame
# with both BF10 (evidence for inclusion) and BF01 = 1 / BF10.
make_bf_table <- function(bf_incl) {
  as.data.frame(bf_incl) %>%
    rownames_to_column("Effect") %>%
    mutate(BF10 = exp(log_BF),
           BF01 = round(1 / exp(log_BF), 3))
}

# Mauchly's sphericity test with Greenhouse-Geisser / Huynh-Feldt epsilons and
# corrected p values for every effect of an afex model. The manuscript applies
# the Greenhouse-Geisser correction only to effects for which Mauchly's test is
# significant (p < .05); this table documents which effects those are.
make_sphericity_table <- function(fit) {
  s <- summary(fit)
  m <- s$sphericity.tests   # matrix of class "anova"; index it as a matrix
  e <- s$pval.adjustments
  eff <- rownames(m)
  idx <- match(eff, rownames(e))
  data.frame(
    Effect              = eff,
    Mauchly_W           = round(as.numeric(m[, 1]), 4),
    Mauchly_p           = round(as.numeric(m[, 2]), 4),
    sphericity_violated = as.numeric(m[, 2]) < .05,
    GG_eps              = round(as.numeric(e[idx, "GG eps"]), 4),
    HF_eps              = round(as.numeric(e[idx, "HF eps"]), 4),
    p_GG                = round(as.numeric(e[idx, "Pr(>F[GG])"]), 4),
    p_HF                = round(as.numeric(e[idx, "Pr(>F[HF])"]), 4),
    row.names           = NULL
  )
}

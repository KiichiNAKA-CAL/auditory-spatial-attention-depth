# ============================================
# Experiment 1: Sustained attention task (Tables 1-2, Figures 3-4)
# ============================================
# Per participant x condition: median response time of correct trials and
# error rate over all trials. 3-way RM-ANOVA (sound pressure level condition x
# standard location x target location), Holm-adjusted post hoc comparisons on
# the estimated marginal means, inclusion Bayes factors across matched models,
# Cousineau-Morey within-participant CIs.

dirs1 <- create_output_dirs(file.path(dir_output_root, "exp1_sustainedAttention"))

# --- 1. Trial-level data -----------------------------------------------------
data_all1 <- read.csv("data/exp1_sustained_attention.csv") %>%
  mutate(
    loudness_condition = factor(loudness_condition, levels = c("Unmatched", "Matched")),
    standard_distance  = std_code(standard_location),
    target             = loc_code(target_location),
    rt_ms              = rt_s * 1000
  )

# The 10 warm-up trials of each block (trial <= 0) are excluded from all analyses
data_filtered1 <- data_all1 %>% filter(trial >= 1)

cat("[Exp1 Sustained] participants:", n_distinct(data_filtered1$participant),
    "  trials:", nrow(data_filtered1), "\n")

# --- 2. Cell values: error rate (all trials; no response = error) and median RT (correct trials)
error_rate1 <- data_filtered1 %>%
  group_by(participant, loudness_condition, standard_distance, target) %>%
  summarise(total_trials = n(),
            error_trials = sum(correct == 0),
            error_rate   = mean(correct == 0),
            .groups = "drop")

median_rt1 <- data_filtered1 %>%
  filter(correct == 1) %>%
  group_by(participant, loudness_condition, standard_distance, target) %>%
  summarise(median_rt = median(rt_ms), n_trials = n(), .groups = "drop")

cat("[Exp1 Sustained] correct trials analysed:", sum(median_rt1$n_trials), "\n")

write.csv(error_rate1, file.path(dirs1$tables, "error_rate_by_target.csv"), row.names = FALSE)
write.csv(median_rt1,  file.path(dirs1$tables, "median_rt_by_target.csv"),  row.names = FALSE)

# --- 3. Summary tables and within-participant CIs (Table 1) ------------------
summary_median_rt1 <- median_rt1 %>%
  group_by(loudness_condition, standard_distance, target) %>%
  summarise(mean_median_rt = mean(median_rt), sd_median_rt = sd(median_rt),
            min_median_rt = min(median_rt), max_median_rt = max(median_rt), .groups = "drop")

summary_error_rate1 <- error_rate1 %>%
  group_by(loudness_condition, standard_distance, target) %>%
  summarise(mean_error_rate = mean(error_rate), sd_error_rate = sd(error_rate), .groups = "drop")

write.csv(summary_median_rt1,  file.path(dirs1$tables, "summary_median_rt.csv"),  row.names = FALSE)
write.csv(summary_error_rate1, file.path(dirs1$tables, "summary_error_rate.csv"), row.names = FALSE)
write.csv(cm_ci(median_rt1, "median_rt", c("loudness_condition", "standard_distance", "target")),
          file.path(dirs1$tables, "cm_ci_median_rt.csv"), row.names = FALSE)
write.csv(cm_ci(error_rate1, "error_rate", c("loudness_condition", "standard_distance", "target")),
          file.path(dirs1$tables, "cm_ci_error_rate.csv"), row.names = FALSE)

# --- 4. Repeated-measures ANOVA (Table 2) ------------------------------------
# anova_*.csv applies the Greenhouse-Geisser correction to every effect;
# anova_*_uncorrected.csv applies none. The manuscript reports the corrected
# values only for effects whose Mauchly test is significant (sphericity_*.csv).
anova_median_rt1 <- aov_ez(id = "participant", dv = "median_rt", data = median_rt1,
                            within = c("loudness_condition", "standard_distance", "target"))
anova_error_rate1 <- aov_ez(id = "participant", dv = "error_rate", data = error_rate1,
                             within = c("loudness_condition", "standard_distance", "target"))

cat("[Exp1 Sustained] === ANOVA: median RT ===\n"); print(nice(anova_median_rt1, es = c("pes", "ges")))
cat("\n[Exp1 Sustained] === ANOVA: error rate ===\n"); print(nice(anova_error_rate1, es = c("pes", "ges")))

write.csv(nice(anova_median_rt1, es = c("pes", "ges")), file.path(dirs1$anova, "anova_median_rt.csv"), row.names = FALSE)
write.csv(nice(anova_median_rt1, correction = "none", es = c("pes", "ges")), file.path(dirs1$anova, "anova_median_rt_uncorrected.csv"), row.names = FALSE)
write.csv(make_sphericity_table(anova_median_rt1), file.path(dirs1$anova, "sphericity_median_rt.csv"), row.names = FALSE)
write.csv(nice(anova_error_rate1, es = c("pes", "ges")), file.path(dirs1$anova, "anova_error_rate.csv"), row.names = FALSE)
write.csv(nice(anova_error_rate1, correction = "none", es = c("pes", "ges")), file.path(dirs1$anova, "anova_error_rate_uncorrected.csv"), row.names = FALSE)
write.csv(make_sphericity_table(anova_error_rate1), file.path(dirs1$anova, "sphericity_error_rate.csv"), row.names = FALSE)

# --- 5. Post hoc comparisons (Holm-adjusted paired t tests on the EMMs) ------
posthoc_loudness_median_rt1 <- pairs(emmeans(anova_median_rt1, ~ loudness_condition), adjust = "holm")
posthoc_target_median_rt1 <- pairs(emmeans(anova_median_rt1, ~ target), adjust = "holm")
posthoc_target_by_distance_median_rt1 <- pairs(emmeans(anova_median_rt1, ~ target | standard_distance), adjust = "holm")

write.csv(as.data.frame(posthoc_loudness_median_rt1), file.path(dirs1$posthoc, "posthoc_loudness_median_rt.csv"), row.names = FALSE)
write.csv(as.data.frame(posthoc_target_median_rt1), file.path(dirs1$posthoc, "posthoc_target_median_rt.csv"), row.names = FALSE)
write.csv(as.data.frame(posthoc_target_by_distance_median_rt1), file.path(dirs1$posthoc, "posthoc_target_by_distance_median_rt.csv"), row.names = FALSE)

# --- 6. Bayes factors (inclusion BF across matched models; Table 2) ----------
# anovaBF enumerates all models by MCMC; the seed is fixed for reproducibility.
# match_models = TRUE restricts each comparison to models that differ only by
# the effect in question (JASP's "across matched models").
cat("[Exp1 Sustained] Computing Bayes factors (median RT)...\n")
set.seed(42)
bf_median_rt_models1 <- anovaBF(
  median_rt ~ loudness_condition * standard_distance * target + participant,
  data = as.data.frame(median_rt1) %>% mutate(participant = factor(participant)),
  whichRandom = "participant", progress = FALSE
)
write.csv(make_bf_table(bayesfactor_inclusion(bf_median_rt_models1, match_models = TRUE)),
          file.path(dirs1$anova, "bf_inclusion_median_rt.csv"), row.names = FALSE)

cat("[Exp1 Sustained] Computing Bayes factors (error rate)...\n")
set.seed(42)
bf_error_rate_models1 <- anovaBF(
  error_rate ~ loudness_condition * standard_distance * target + participant,
  data = as.data.frame(error_rate1) %>% mutate(participant = factor(participant)),
  whichRandom = "participant", progress = FALSE
)
write.csv(make_bf_table(bayesfactor_inclusion(bf_error_rate_models1, match_models = TRUE)),
          file.path(dirs1$anova, "bf_inclusion_error_rate.csv"), row.names = FALSE)

# --- 7. Figures 3 and 4 (x = target, colour/shape = standard, panels = SPL condition)
cr_seg1 <- c(5, 86); cr_tick1 <- c(50, 2)

df_cm_median_rt_split1 <- median_rt1 %>%
  cm_ci("median_rt", c("loudness_condition", "standard_distance", "target")) %>%
  relabel_distance_cols(c("target", "standard_distance"))
g_cm_median_rt1_unmatched <- make_distance_plot(filter(df_cm_median_rt_split1, loudness_condition == "Unmatched"), "target", "standard_distance",
                                                 "Response time [ms]", plot_title = "Loudness-unmatched", legend_title = "Standard",
                                                 x_axis_text_color = plot_colors)
g_cm_median_rt1_matched <- make_distance_plot(filter(df_cm_median_rt_split1, loudness_condition == "Matched"), "target", "standard_distance",
                                               "Response time [ms]", plot_title = "Loudness-matched", legend_title = "Standard",
                                               x_axis_text_color = plot_colors)
save_gap_figure_split(g_cm_median_rt1_unmatched, g_cm_median_rt1_matched, dirs1, "plot_median_rt_cm",
                       c(10, 480), c(0, 720), c(100, 20), width = 12, height = 5,
                       legend_margin = c(top = 0, right = 0, bottom = -315, left = 0))

df_cm_correct_ratio_split1 <- error_rate1 %>%
  cm_ci("error_rate", c("loudness_condition", "standard_distance", "target")) %>%
  to_correct_ratio() %>%
  relabel_distance_cols(c("target", "standard_distance"))
g_cm_correct_ratio1_unmatched <- make_distance_plot(filter(df_cm_correct_ratio_split1, loudness_condition == "Unmatched"), "target", "standard_distance",
                                                     "Correct rate [%]", plot_title = "Loudness-unmatched", legend_title = "Standard",
                                                     x_axis_text_color = plot_colors)
g_cm_correct_ratio1_matched <- make_distance_plot(filter(df_cm_correct_ratio_split1, loudness_condition == "Matched"), "target", "standard_distance",
                                                   "Correct rate [%]", plot_title = "Loudness-matched", legend_title = "Standard",
                                                   x_axis_text_color = plot_colors)
save_gap_figure_split(g_cm_correct_ratio1_unmatched, g_cm_correct_ratio1_matched, dirs1, "plot_correct_ratio_cm",
                       cr_seg1, c(0, 101), cr_tick1, width = 12, height = 5,
                       legend_margin = c(top = 0, right = 0, bottom = -315, left = 0))

cat("[Exp1 Sustained] done\n")

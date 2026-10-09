# ============================================
# Experiment 2: Spatial cueing task (Tables 3-4, Figures 8-9)
# ============================================
# Cued trials: 3-way RM-ANOVA (SOA x cue location x target location) on the
# per-participant median RT of correct trials and on the error rate; Holm-
# adjusted post hoc comparisons; inclusion Bayes factors across matched models.
# No-cue trials: separate 1-way RM-ANOVA (target location). Cousineau-Morey
# within-participant CIs. Finally, the descriptive comparison of the first and
# second halves of the session reported in the Results.

dirs2 <- create_output_dirs(file.path(dir_output_root, "exp2_spatialCueing"))

# --- 1. Trial-level data -----------------------------------------------------
data_all2 <- read.csv("data/exp2_spatial_cueing.csv") %>%
  mutate(
    SOA    = factor(if_else(is.na(soa_ms), "nocue", paste0(soa_ms, "ms")),
                    levels = c("100ms", "400ms", "nocue")),
    cue    = factor(if_else(is.na(cue_location), "nocue", sprintf("t%.1fm", cue_location)),
                    levels = c("t0.3m", "t1.3m", "t2.3m", "nocue")),
    target = loc_code(target_location),
    participant = factor(participant),
    rt_ms  = rt_s * 1000,
    # trials without a response within 2 s (correct = NA) count as neither
    # correct nor incorrect; they stay in the denominator of the error rate
    is_error = !is.na(correct) & correct == 0
  )

cat("[Exp2 SpatialCueing] participants:", n_distinct(data_all2$participant),
    "  trials:", nrow(data_all2), "\n")

data_cue2   <- data_all2 %>% filter(SOA != "nocue")
data_nocue2 <- data_all2 %>% filter(SOA == "nocue")

# --- 2. Cell values ----------------------------------------------------------
error_rate_cue2 <- data_cue2 %>%
  group_by(participant, SOA, cue, target) %>%
  summarise(total_trials = n(), error_trials = sum(is_error), error_rate = mean(is_error), .groups = "drop") %>%
  mutate(SOA = factor(SOA, levels = c("100ms", "400ms")),
         cue = factor(cue, levels = c("t0.3m", "t1.3m", "t2.3m")),
         target = factor(target, levels = c("t0.3m", "t1.3m", "t2.3m")))

error_rate_nocue2 <- data_nocue2 %>%
  group_by(participant, target) %>%
  summarise(total_trials = n(), error_trials = sum(is_error), error_rate = mean(is_error), .groups = "drop") %>%
  mutate(target = factor(target, levels = c("t0.3m", "t1.3m", "t2.3m")))

correct_cue2   <- data_cue2   %>% filter(!is.na(correct) & correct == 1)
correct_nocue2 <- data_nocue2 %>% filter(!is.na(correct) & correct == 1)

median_rt_cue2 <- correct_cue2 %>%
  group_by(participant, SOA, cue, target) %>%
  summarise(median_rt = median(rt_ms), n_trials = n(), .groups = "drop") %>%
  mutate(SOA = factor(SOA, levels = c("100ms", "400ms")),
         cue = factor(cue, levels = c("t0.3m", "t1.3m", "t2.3m")),
         target = factor(target, levels = c("t0.3m", "t1.3m", "t2.3m")))

median_rt_nocue2 <- correct_nocue2 %>%
  group_by(participant, target) %>%
  summarise(median_rt = median(rt_ms), n_trials = n(), .groups = "drop") %>%
  mutate(target = factor(target, levels = c("t0.3m", "t1.3m", "t2.3m")))

cat("[Exp2 SpatialCueing] correct trials analysed: cued", nrow(correct_cue2), " no-cue", nrow(correct_nocue2), "\n")

write.csv(median_rt_cue2,    file.path(dirs2$tables, "median_rt_by_condition_cue.csv"),   row.names = FALSE)
write.csv(median_rt_nocue2,  file.path(dirs2$tables, "median_rt_by_target_nocue.csv"),    row.names = FALSE)
write.csv(error_rate_cue2,   file.path(dirs2$tables, "error_rate_by_condition_cue.csv"),  row.names = FALSE)
write.csv(error_rate_nocue2, file.path(dirs2$tables, "error_rate_by_target_nocue.csv"),   row.names = FALSE)

# --- 3. Summary tables and within-participant CIs (Table 3) ------------------
summary_median_rt_cue2 <- median_rt_cue2 %>% group_by(SOA, cue, target) %>%
  summarise(M_ms = round(mean(median_rt), 1), SD = round(sd(median_rt), 1), .groups = "drop")
summary_median_rt_nocue2 <- median_rt_nocue2 %>% group_by(target) %>%
  summarise(M_ms = round(mean(median_rt), 1), SD = round(sd(median_rt), 1), .groups = "drop")
summary_error_rate_cue2 <- error_rate_cue2 %>% group_by(SOA, cue, target) %>%
  summarise(M_pct = round(mean(error_rate) * 100, 1), SD = round(sd(error_rate) * 100, 1), .groups = "drop")
summary_error_rate_nocue2 <- error_rate_nocue2 %>% group_by(target) %>%
  summarise(M_pct = round(mean(error_rate) * 100, 1), SD = round(sd(error_rate) * 100, 1), .groups = "drop")

write.csv(summary_median_rt_cue2,    file.path(dirs2$tables, "summary_median_rt_cue.csv"),    row.names = FALSE)
write.csv(summary_median_rt_nocue2,  file.path(dirs2$tables, "summary_median_rt_nocue.csv"),  row.names = FALSE)
write.csv(summary_error_rate_cue2,   file.path(dirs2$tables, "summary_error_rate_cue.csv"),   row.names = FALSE)
write.csv(summary_error_rate_nocue2, file.path(dirs2$tables, "summary_error_rate_nocue.csv"), row.names = FALSE)

write.csv(cm_ci(median_rt_cue2, "median_rt", c("SOA", "cue", "target")),
          file.path(dirs2$tables, "cm_ci_median_rt_cue.csv"), row.names = FALSE)
write.csv(cm_ci(error_rate_cue2, "error_rate", c("SOA", "cue", "target")),
          file.path(dirs2$tables, "cm_ci_error_rate_cue.csv"), row.names = FALSE)
write.csv(cm_ci(median_rt_nocue2, "median_rt", "target"),
          file.path(dirs2$tables, "cm_ci_median_rt_nocue.csv"), row.names = FALSE)
write.csv(cm_ci(error_rate_nocue2, "error_rate", "target"),
          file.path(dirs2$tables, "cm_ci_error_rate_nocue.csv"), row.names = FALSE)

# --- 4. RM-ANOVAs (Table 4) --------------------------------------------------
# anova_*.csv: Greenhouse-Geisser corrected; anova_*_uncorrected.csv: no
# correction. The manuscript reports corrected values only for effects whose
# Mauchly test is significant (sphericity_*.csv).
anova_median_rt_cue2   <- aov_ez(id = "participant", dv = "median_rt",  data = median_rt_cue2,  within = c("SOA", "cue", "target"))
anova_error_rate_cue2  <- aov_ez(id = "participant", dv = "error_rate", data = error_rate_cue2, within = c("SOA", "cue", "target"))
anova_median_rt_nocue2  <- aov_ez(id = "participant", dv = "median_rt",  data = median_rt_nocue2,  within = "target")
anova_error_rate_nocue2 <- aov_ez(id = "participant", dv = "error_rate", data = error_rate_nocue2, within = "target")

cat("[Exp2 SpatialCueing] === ANOVA: median RT (cued) ===\n");   print(nice(anova_median_rt_cue2, es = c("pes", "ges")))
cat("\n[Exp2 SpatialCueing] === ANOVA: error rate (cued) ===\n"); print(nice(anova_error_rate_cue2, es = c("pes", "ges")))
cat("\n[Exp2 SpatialCueing] === ANOVA: median RT (no cue) ===\n");  print(nice(anova_median_rt_nocue2, es = c("pes", "ges")))
cat("\n[Exp2 SpatialCueing] === ANOVA: error rate (no cue) ===\n"); print(nice(anova_error_rate_nocue2, es = c("pes", "ges")))

write_anova_trio <- function(fit, tag) {
  write.csv(nice(fit, es = c("pes", "ges")), file.path(dirs2$anova, paste0("anova_", tag, ".csv")), row.names = FALSE)
  write.csv(nice(fit, correction = "none", es = c("pes", "ges")), file.path(dirs2$anova, paste0("anova_", tag, "_uncorrected.csv")), row.names = FALSE)
  write.csv(make_sphericity_table(fit), file.path(dirs2$anova, paste0("sphericity_", tag, ".csv")), row.names = FALSE)
}
write_anova_trio(anova_median_rt_cue2,    "median_rt_cue")
write_anova_trio(anova_error_rate_cue2,   "error_rate_cue")
write_anova_trio(anova_median_rt_nocue2,  "median_rt_nocue")
write_anova_trio(anova_error_rate_nocue2, "error_rate_nocue")

# --- 5. Post hoc comparisons (Holm-adjusted paired t tests on the EMMs) ------
posthoc_target_median_rt2        <- pairs(emmeans(anova_median_rt_cue2, ~ target), adjust = "holm")
posthoc_soa_median_rt2           <- pairs(emmeans(anova_median_rt_cue2, ~ SOA), adjust = "holm")
posthoc_target_by_soa_median_rt2 <- pairs(emmeans(anova_median_rt_cue2, ~ target | SOA), adjust = "holm")   # simple effects of target at each SOA
posthoc_soa_by_target_median_rt2 <- pairs(emmeans(anova_median_rt_cue2, ~ SOA | target), adjust = "holm")   # SOA difference for each target
posthoc_soa_error_rate2          <- pairs(emmeans(anova_error_rate_cue2, ~ SOA), adjust = "holm")

write.csv(as.data.frame(posthoc_target_median_rt2),        file.path(dirs2$posthoc, "posthoc_target_median_rt.csv"), row.names = FALSE)
write.csv(as.data.frame(posthoc_soa_median_rt2),           file.path(dirs2$posthoc, "posthoc_soa_median_rt.csv"), row.names = FALSE)
write.csv(as.data.frame(posthoc_target_by_soa_median_rt2), file.path(dirs2$posthoc, "posthoc_target_by_soa_median_rt.csv"), row.names = FALSE)
write.csv(as.data.frame(posthoc_soa_by_target_median_rt2), file.path(dirs2$posthoc, "posthoc_soa_by_target_median_rt.csv"), row.names = FALSE)
write.csv(as.data.frame(posthoc_soa_error_rate2),          file.path(dirs2$posthoc, "posthoc_soa_error_rate.csv"), row.names = FALSE)

# --- 6. Bayes factors (Table 4) ----------------------------------------------
cat("[Exp2 SpatialCueing] Computing Bayes factors (median RT, cued)...\n")
set.seed(42)
bf_median_rt_cue_models2 <- anovaBF(median_rt ~ SOA * cue * target + participant,
                                     data = as.data.frame(median_rt_cue2), whichRandom = "participant", progress = FALSE)
write.csv(make_bf_table(bayesfactor_inclusion(bf_median_rt_cue_models2, match_models = TRUE)),
          file.path(dirs2$anova, "bf_inclusion_median_rt_cue.csv"), row.names = FALSE)

cat("[Exp2 SpatialCueing] Computing Bayes factor (median RT, no cue)...\n")
set.seed(42)
bf_median_rt_nocue_model2 <- anovaBF(median_rt ~ target + participant,
                                      data = as.data.frame(median_rt_nocue2), whichRandom = "participant", progress = FALSE)
bf_nocue_median_rt_val2 <- extractBF(bf_median_rt_nocue_model2)$bf
write.csv(data.frame(effect = "target", BF10 = bf_nocue_median_rt_val2, BF01 = 1 / bf_nocue_median_rt_val2),
          file.path(dirs2$anova, "bf_median_rt_nocue.csv"), row.names = FALSE)

cat("[Exp2 SpatialCueing] Computing Bayes factors (error rate, cued)...\n")
set.seed(42)
bf_error_rate_cue_models2 <- anovaBF(error_rate ~ SOA * cue * target + participant,
                                      data = as.data.frame(error_rate_cue2), whichRandom = "participant", progress = FALSE)
write.csv(make_bf_table(bayesfactor_inclusion(bf_error_rate_cue_models2, match_models = TRUE)),
          file.path(dirs2$anova, "bf_inclusion_error_rate_cue.csv"), row.names = FALSE)

cat("[Exp2 SpatialCueing] Computing Bayes factor (error rate, no cue)...\n")
set.seed(42)
bf_error_rate_nocue_model2 <- anovaBF(error_rate ~ target + participant,
                                       data = as.data.frame(error_rate_nocue2), whichRandom = "participant", progress = FALSE)
bf_nocue_er_val2 <- extractBF(bf_error_rate_nocue_model2)$bf
write.csv(data.frame(effect = "target", BF10 = bf_nocue_er_val2, BF01 = 1 / bf_nocue_er_val2),
          file.path(dirs2$anova, "bf_error_rate_nocue.csv"), row.names = FALSE)

# --- 7. Figures 8 and 9 (x = target, colour/shape = cue, panels = SOA) -------
rt_seg_cm2 <- c(10, 560); rt_tick_cm2 <- c(20, 20)
cr_seg2 <- c(5, 86); cr_tick2 <- c(50, 2)

df_cm_median_rt_nocue2 <- correct_nocue2 %>%
  group_by(participant, target) %>% summarise(median_rt = median(rt_ms), .groups = "drop") %>%
  cm_ci("median_rt", "target") %>% relabel_distance_cols("target")

# Figure 8: cued series per SOA panel with the no-cue condition overlaid in
# gray (dashed line, open circles); the no-cue values are the same in both panels.
df_cm_median_rt_split2 <- median_rt_cue2 %>%
  cm_ci("median_rt", c("SOA", "cue", "target")) %>% relabel_distance_cols(c("target", "cue"))
g_cm_median_rt_100ms2 <- make_distance_plot_with_nocue(filter(df_cm_median_rt_split2, SOA == "100ms"), df_cm_median_rt_nocue2, plot_title = "SOA 100 ms")
g_cm_median_rt_400ms2 <- make_distance_plot_with_nocue(filter(df_cm_median_rt_split2, SOA == "400ms"), df_cm_median_rt_nocue2, plot_title = "SOA 400 ms")
save_gap_figure_split(g_cm_median_rt_100ms2, g_cm_median_rt_400ms2, dirs2, "plot_median_rt_split_soa_cm", rt_seg_cm2, c(0, 665), rt_tick_cm2, width = 12, height = 5,
                       legend_margin = c(top = 0, right = 0, bottom = -315, left = 0))

# Figure 9: correct rates per SOA panel
df_cm_correct_ratio_split2 <- error_rate_cue2 %>%
  cm_ci("error_rate", c("SOA", "cue", "target")) %>% to_correct_ratio() %>% relabel_distance_cols(c("target", "cue"))
g_cm_correct_ratio_100ms2 <- make_distance_plot(filter(df_cm_correct_ratio_split2, SOA == "100ms"), "target", "cue", "Correct rate [%]", plot_title = "SOA 100 ms", legend_title = "Cue location")
g_cm_correct_ratio_400ms2 <- make_distance_plot(filter(df_cm_correct_ratio_split2, SOA == "400ms"), "target", "cue", "Correct rate [%]", plot_title = "SOA 400 ms", legend_title = "Cue location")
save_gap_figure_split(g_cm_correct_ratio_100ms2, g_cm_correct_ratio_400ms2, dirs2, "plot_correct_ratio_split_soa_cm", cr_seg2, c(0, 101), cr_tick2, width = 12, height = 5,
                       legend_margin = c(top = 0, right = 0, bottom = -315, left = 0))

# --- 8. First versus second half of the session (descriptive; Results 3.2.1)
# Per participant and half (trials 1-315 vs 316-630, presentation order):
#   nearest advantage   = mean median RT (targets 1.3 and 2.3 m) - median RT (target 0.3 m)
#   cue validity effect = mean median RT (cue != target) - mean median RT (cue == target)
# Cells = SOA x cue x target medians of correct trials, equally weighted.
cells_half2 <- correct_cue2 %>%
  mutate(half = factor(if_else(trial <= 315, "first", "second"), levels = c("first", "second"))) %>%
  group_by(participant, half, SOA, cue, target) %>%
  summarise(median_rt = median(rt_ms), .groups = "drop")

effects_half2 <- cells_half2 %>%
  group_by(participant, half) %>%
  summarise(
    nearest_advantage   = mean(median_rt[target != "t0.3m"]) - mean(median_rt[target == "t0.3m"]),
    cue_validity_effect = mean(median_rt[as.character(cue) != as.character(target)]) -
                          mean(median_rt[as.character(cue) == as.character(target)]),
    .groups = "drop"
  )

session_halves2 <- effects_half2 %>%
  pivot_longer(c(nearest_advantage, cue_validity_effect), names_to = "effect", values_to = "value_ms") %>%
  mutate(effect = factor(effect, levels = c("nearest_advantage", "cue_validity_effect"))) %>%
  group_by(effect, half) %>%
  summarise(n = n(), mean_ms = mean(value_ms), sd_ms = sd(value_ms), .groups = "drop")
write.csv(session_halves2, file.path(dirs2$tables, "session_halves.csv"), row.names = FALSE)
print(session_halves2)

cat("[Exp2 SpatialCueing] done\n")

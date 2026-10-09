# ============================================
# Experiment 2: Distance discrimination task (Figure 10)
# ============================================
# Same task and coding as R/exp1_localization.R, run once (loudness-matched
# sound pressure levels), so there is a single within-participant factor (pair).
# Participant p03 responded with the keys reversed; data/ already contains the
# corrected `correct` column for this participant (see README).

dirs2L <- create_output_dirs(file.path(dir_output_root, "exp2_localization"))

loc_data_all2 <- read.csv("data/exp2_distance_discrimination.csv") %>%
  mutate(pair = pair_code(first_location, second_location))

cat("[Exp2 Localization] participants:", n_distinct(loc_data_all2$participant),
    "  trials:", nrow(loc_data_all2), "\n")

loc_by_pair2 <- loc_data_all2 %>%
  group_by(participant, pair) %>%
  summarise(total_trials = n(), correct_trials = sum(correct == 1),
            correct_ratio = mean(correct == 1), .groups = "drop")

summary_loc2 <- loc_by_pair2 %>%
  group_by(pair) %>%
  summarise(mean_correct_ratio = mean(correct_ratio) * 100,
            sd_correct_ratio = sd(correct_ratio) * 100, .groups = "drop")

write.csv(loc_by_pair2, file.path(dirs2L$tables, "correct_ratio_by_pair.csv"), row.names = FALSE)
write.csv(summary_loc2, file.path(dirs2L$tables, "summary_by_pair.csv"), row.names = FALSE)
print(summary_loc2)

df_cm_loc2 <- loc_by_pair2 %>% cm_ci("correct_ratio", "pair") %>%
  mutate(across(c(emmean, lower.CL, upper.CL), ~ .x * 100)) %>% relabel_pair_col()
write.csv(df_cm_loc2, file.path(dirs2L$tables, "cm_ci_correct_ratio.csv"), row.names = FALSE)

# --- RM-ANOVA ---------------------------------------------------------------
anova_loc2 <- aov_ez(id = "participant", dv = "correct_ratio", data = loc_by_pair2, within = "pair")
cat("[Exp2 Localization] === ANOVA: correct rate ===\n"); print(nice(anova_loc2, es = c("pes", "ges")))
write.csv(nice(anova_loc2, es = c("pes", "ges")), file.path(dirs2L$anova, "anova_correct_ratio.csv"), row.names = FALSE)
write.csv(nice(anova_loc2, correction = "none", es = c("pes", "ges")), file.path(dirs2L$anova, "anova_correct_ratio_uncorrected.csv"), row.names = FALSE)
write.csv(make_sphericity_table(anova_loc2), file.path(dirs2L$anova, "sphericity_correct_ratio.csv"), row.names = FALSE)

# --- Post hoc comparisons between distance pairs (Holm) ---------------------
posthoc_pair_loc2 <- pairs(emmeans(anova_loc2, ~ pair), adjust = "holm")
write.csv(as.data.frame(posthoc_pair_loc2), file.path(dirs2L$posthoc, "posthoc_pair_correct_ratio.csv"), row.names = FALSE)

# --- Chance-level test (one-sample t vs 50%, per pair) ----------------------
chance_test2 <- loc_by_pair2 %>%
  group_by(pair) %>%
  summarise(mean_pct = mean(correct_ratio) * 100,
            t = unname(t.test(correct_ratio, mu = 0.5)$statistic),
            df = unname(t.test(correct_ratio, mu = 0.5)$parameter),
            p = t.test(correct_ratio, mu = 0.5)$p.value,
            n_at_or_below_chance = sum(correct_ratio <= 0.5),
            .groups = "drop")
write.csv(chance_test2, file.path(dirs2L$anova, "chance_test_correct_ratio.csv"), row.names = FALSE)

# --- Figure 10: raincloud per pair --------------------------------------------
raincloud_loc2 <- loc_by_pair2 %>% mutate(correct_ratio = correct_ratio * 100) %>% relabel_pair_col()
g_raincloud_loc2 <- make_raincloud(raincloud_loc2, "pair", "correct_ratio", "Correct rate [%]",
                                    dots_binwidth = 1, x_label = "Distance pair",
                                    fill_values = pair_gray_values, x_axis_text_color = "black")
save_gap_figure(g_raincloud_loc2, dirs2L, "raincloud_correct_ratio",
                segments = c(5, 30), ylim = c(0, 101), tick_width = c(50, 10), width = 6, height = 5)

cat("[Exp2 Localization] done\n")

# ============================================
# Experiment 1: Distance discrimination task (Figure 5)
# ============================================
# Participants heard two sounds from different distances and judged which was
# closer. Correct rate per participant x sound pressure level condition x
# distance pair (the two presentation orders of a pair are pooled: 60 trials).
# 2-way RM-ANOVA (sound pressure level condition x pair), Holm-adjusted post
# hoc comparisons between pairs, one-sample t tests against chance (50%).

dirs1L <- create_output_dirs(file.path(dir_output_root, "exp1_localization"))

loc_data_all1 <- read.csv("data/exp1_distance_discrimination.csv") %>%
  mutate(loudness_condition = factor(loudness_condition, levels = c("Unmatched", "Matched")),
         pair = pair_code(first_location, second_location))

cat("[Exp1 Localization] participants:", n_distinct(loc_data_all1$participant),
    "  trials:", nrow(loc_data_all1), "\n")

loc_by_pair1 <- loc_data_all1 %>%
  group_by(participant, loudness_condition, pair) %>%
  summarise(total_trials = n(), correct_trials = sum(correct == 1),
            correct_ratio = mean(correct == 1), .groups = "drop")

summary_loc1 <- loc_by_pair1 %>%
  group_by(loudness_condition, pair) %>%
  summarise(mean_correct_ratio = mean(correct_ratio) * 100,
            sd_correct_ratio = sd(correct_ratio) * 100, .groups = "drop")

write.csv(loc_by_pair1, file.path(dirs1L$tables, "correct_ratio_by_pair.csv"), row.names = FALSE)
write.csv(summary_loc1, file.path(dirs1L$tables, "summary_by_pair.csv"), row.names = FALSE)
print(summary_loc1)

# Within-participant CIs reported in the text
df_cm_loc_split1 <- loc_by_pair1 %>%
  cm_ci("correct_ratio", c("loudness_condition", "pair")) %>%
  mutate(across(c(emmean, lower.CL, upper.CL), ~ .x * 100)) %>%
  relabel_pair_col()
write.csv(df_cm_loc_split1, file.path(dirs1L$tables, "cm_ci_correct_ratio.csv"), row.names = FALSE)

# --- RM-ANOVA ---------------------------------------------------------------
anova_loc1 <- aov_ez(id = "participant", dv = "correct_ratio", data = loc_by_pair1,
                      within = c("loudness_condition", "pair"))
cat("[Exp1 Localization] === ANOVA: correct rate ===\n"); print(nice(anova_loc1, es = c("pes", "ges")))
write.csv(nice(anova_loc1, es = c("pes", "ges")), file.path(dirs1L$anova, "anova_correct_ratio.csv"), row.names = FALSE)
write.csv(nice(anova_loc1, correction = "none", es = c("pes", "ges")), file.path(dirs1L$anova, "anova_correct_ratio_uncorrected.csv"), row.names = FALSE)
write.csv(make_sphericity_table(anova_loc1), file.path(dirs1L$anova, "sphericity_correct_ratio.csv"), row.names = FALSE)

# --- Post hoc comparisons between distance pairs (Holm) ---------------------
posthoc_pair_loc1 <- pairs(emmeans(anova_loc1, ~ pair), adjust = "holm")
write.csv(as.data.frame(posthoc_pair_loc1), file.path(dirs1L$posthoc, "posthoc_pair_correct_ratio.csv"), row.names = FALSE)

# --- Chance-level test (one-sample t vs 50%, per condition) -----------------
chance_test1 <- loc_by_pair1 %>%
  group_by(loudness_condition, pair) %>%
  summarise(mean_pct = mean(correct_ratio) * 100,
            t = unname(t.test(correct_ratio, mu = 0.5)$statistic),
            df = unname(t.test(correct_ratio, mu = 0.5)$parameter),
            p = t.test(correct_ratio, mu = 0.5)$p.value,
            n_at_or_below_chance = sum(correct_ratio <= 0.5),
            .groups = "drop")
write.csv(chance_test1, file.path(dirs1L$anova, "chance_test_correct_ratio.csv"), row.names = FALSE)

# --- Figure 5: raincloud per pair, one panel per sound pressure level condition
rc_seg1 <- c(5, 30); rc_ylim1 <- c(0, 101); rc_tick1 <- c(50, 10)
raincloud_loc_split1 <- loc_by_pair1 %>% mutate(correct_ratio = correct_ratio * 100) %>% relabel_pair_col()
make_loc_raincloud1 <- function(cond, title) {
  make_raincloud(filter(raincloud_loc_split1, loudness_condition == cond),
                 "pair", "correct_ratio", "Correct rate [%]",
                 dots_binwidth = 1, x_label = "Distance pair",
                 fill_values = pair_gray_values, x_axis_text_color = "black") +
    labs(title = title) +
    theme(plot.title = element_text(size = 12, hjust = .5))
}
g_raincloud_loc_unmatched1 <- make_loc_raincloud1("Unmatched", "Loudness-unmatched")
g_raincloud_loc_matched1   <- make_loc_raincloud1("Matched", "Loudness-matched")
save_gap_figure_split(g_raincloud_loc_unmatched1, g_raincloud_loc_matched1, dirs1L,
                      "raincloud_correct_ratio_split", rc_seg1, rc_ylim1, rc_tick1, width = 12, height = 5)

cat("[Exp1 Localization] done\n")

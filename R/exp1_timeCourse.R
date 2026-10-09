# ============================================
# Experiment 1: Time course of response times within blocks (Figure 6)
# ============================================
# Descriptive only. Median RT of correct trials per participant, in bins of 10
# trials by position within the 100-trial block, separately for stimuli at the
# standard location and stimuli shifted to either other location (pooled over
# shift distance, standard location and sound pressure level condition). The
# 10 warm-up trials at the standard location are shown as a detached gray bin.

dirsTC <- create_output_dirs(file.path(dir_output_root, "exp1_timeCourse"), c("tables", "figures"))

data_tc <- read.csv("data/exp1_sustained_attention.csv") %>%
  mutate(rt_ms      = rt_s * 1000,
         is_warmup  = trial < 1,
         trial_type = if_else(target_location == standard_location, "standard", "shifted"))

# warm-up trials are always at the standard location (task code); verify
stopifnot(all(data_tc$trial_type[data_tc$is_warmup] == "standard"))

# bins: 0 = warm-up (10 trials), 1..10 = main trials in tens
data_bins <- data_tc %>%
  filter(correct == 1) %>%
  mutate(bin = if_else(is_warmup, 0, ceiling(trial / 10)))

cells_tc <- data_bins %>%
  group_by(participant, bin, trial_type) %>%
  summarise(median_rt = median(rt_ms), n = n(), .groups = "drop")
# no shifted trials exist in the warm-up bin, so the grid is 21 * (11 + 10)
stopifnot(nrow(cells_tc) == 21 * 21)

write.csv(cells_tc %>% group_by(bin, trial_type) %>%
            summarise(mean_of_medians = mean(median_rt),
                      min_n = min(n), median_n = median(n), .groups = "drop"),
          file.path(dirsTC$tables, "descriptive_rt_by_trialbin.csv"), row.names = FALSE)

ci_tc <- cm_ci(cells_tc, "median_rt", c("bin", "trial_type")) %>%
  mutate(trial_type = factor(trial_type, levels = c("standard", "shifted")))
ci_tc_main <- ci_tc %>% filter(bin >= 1)
ci_tc_wu   <- ci_tc %>% filter(bin == 0)   # warm-up (standard location only)

# --- Figure ----------------------------------------------------------------
bin_breaks <- 0:10
warmup_label_html <- "<span style='color:#8C8C8C'>Warm-up</span>"
bin_labels <- c(warmup_label_html, paste0(seq(1, 91, 10), "-", seq(10, 100, 10)))

theme_ms <- theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_text(size = 12),
        axis.text  = element_text(size = 12, color = "black"),
        axis.text.x = ggtext::element_markdown(size = 10, color = "black",
                                               angle = 45, hjust = 1),
        axis.ticks = element_line(color = "black"),
        legend.position = "bottom")

warmup_point_col <- "#8C8C8C"
add_warmup_layers <- function(df_wu) {
  list(
    geom_errorbar(data = df_wu, aes(x = bin, ymin = lower.CL, ymax = upper.CL),
                  width = 0.15, linewidth = 0.35, color = warmup_point_col,
                  inherit.aes = FALSE),
    geom_point(data = df_wu, aes(x = bin, y = emmean), shape = 21, size = 1.6,
               color = warmup_point_col, fill = "white", stroke = 0.6,
               inherit.aes = FALSE)
  )
}
warmup_sep <- geom_vline(xintercept = 0.5, linetype = "dotted", color = "gray50")

col_tc <- c(standard = "#000000", shifted = "#E69F00")
lab_tc <- c(standard = "Standard location", shifted = "Shifted locations")

g_tc <- ggplot(ci_tc_main, aes(x = bin, y = emmean, color = trial_type,
                               fill = trial_type, shape = trial_type,
                               group = trial_type)) +
  warmup_sep +
  add_warmup_layers(ci_tc_wu) +
  geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL), alpha = .2, color = NA) +
  geom_line(linewidth = .4) +
  geom_point(size = 2, fill = "white") +
  scale_x_continuous(breaks = bin_breaks, labels = bin_labels) +
  scale_color_manual(values = col_tc, labels = lab_tc, name = NULL) +
  scale_fill_manual(values = col_tc, labels = lab_tc, name = NULL) +
  scale_shape_manual(values = c(standard = 21, shifted = 16),
                     labels = lab_tc, name = NULL) +
  labs(x = "Trial position within block", y = "Response time [ms]") +
  theme_ms

# Same axis break and panel size as the other data figures
g_tc_ms <- g_tc +
  theme(axis.text.x = ggtext::element_markdown(size = 9, color = "black",
                                               angle = 0, hjust = 0.5, vjust = 1),
        legend.text = element_text(size = 12),
        legend.direction = "horizontal", legend.position = "bottom")
save_gap_figure(g_tc_ms, dirsTC, "descriptive_rt_by_trialbin_ms",
                c(10, 480), c(0, 720), c(100, 20), width = 6, height = 5,
                legend_margin = c(top = 0, right = 0, bottom = -315, left = 0))

cat("[Exp1 Time course] done\n")

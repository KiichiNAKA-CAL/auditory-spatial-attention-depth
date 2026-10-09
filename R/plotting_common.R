# ============================================
# Shared plotting functions
# ============================================
# All figures use the Cousineau-Morey within-participant CI (cm_ci() in
# utils_common.R) and share one style: theme_minimal, 12-pt black axis text,
# no minor grid lines, y axis anchored at 0 with an axis break (gg.gap).

# Display labels for location codes ("t0.3m"... and "near"/"mid"/"far").
distance_label_map <- c(
  "t0.3m" = "0.3 m", "t1.3m" = "1.3 m", "t2.3m" = "2.3 m",
  "near"  = "0.3 m", "mid"   = "1.3 m", "far"   = "2.3 m"
)

relabel_distance_cols <- function(df, cols) {
  df %>%
    mutate(across(all_of(cols), ~ factor(unname(distance_label_map[as.character(.x)]),
                                          levels = c("0.3 m", "1.3 m", "2.3 m"))))
}

# Display labels for the distance pairs of the distance discrimination task.
pair_label_map <- c(
  "near_mid" = "0.3 & 1.3 m", "near_far" = "0.3 & 2.3 m", "mid_far" = "1.3 & 2.3 m"
)

relabel_pair_col <- function(df, col = "pair") {
  df %>%
    mutate(across(all_of(col), ~ factor(unname(pair_label_map[as.character(.x)]),
                                         levels = c("0.3 & 1.3 m", "0.3 & 2.3 m", "1.3 & 2.3 m"))))
}

# Point + error bar (+ line) plot of emmean / CI by x_var, dodged and
# coloured/shaped by group_var. With group_var = NULL, a single black series.
make_distance_plot <- function(df, x_var, group_var = NULL, y_label,
                                x_label = "Target location", plot_title = NULL,
                                legend_title = NULL,
                                color_values = plot_colors, color_labels = plot_labels_html,
                                shape_values = c(16, 17, 15),
                                x_axis_text_color = "black") {
  if (is.null(group_var)) {
    ggplot(df, aes(x = .data[[x_var]], y = emmean, group = 1)) +
      geom_line(linewidth = .25, color = "black") +
      geom_point(size = 3, color = "black") +
      geom_errorbar(aes(ymax = upper.CL, ymin = lower.CL), width = .2, linewidth = .25, color = "black") +
      theme_minimal() +
    theme(panel.grid.minor = element_blank()) +
      theme(plot.title  = element_text(size = 12, hjust = .5),
            axis.title  = element_text(size = 12),
            axis.text   = element_text(size = 12, color = "black"),
            axis.ticks  = element_line(color = "black")) +
      labs(title = plot_title, x = x_label, y = y_label)
  } else {
    ggplot(df, aes(x = .data[[x_var]], y = emmean, shape = .data[[group_var]], color = .data[[group_var]])) +
      geom_point(position = position_dodge(.3), size = 3) +
      geom_errorbar(aes(ymax = upper.CL, ymin = lower.CL),
                    position = position_dodge(.3), width = .2, linewidth = .25) +
      geom_line(aes(group = .data[[group_var]]), position = position_dodge(.3), linewidth = .25) +
      scale_color_manual(values = color_values, labels = color_labels) +
      scale_shape_manual(values = shape_values, labels = color_labels) +
      theme_minimal() +
    theme(panel.grid.minor = element_blank()) +
      theme(plot.title       = element_text(size = 12, hjust = .5),
            axis.title       = element_text(size = 12),
            axis.text.y      = element_text(size = 12, color = "black"),
            axis.text.x      = element_text(size = 12, color = x_axis_text_color),
            axis.ticks       = element_line(color = "black"),
            legend.text      = element_markdown(size = 12),
            legend.direction = "horizontal",
            legend.position  = "bottom") +
      labs(title = plot_title, x = x_label, y = y_label, shape = legend_title, color = legend_title)
  }
}

# Variant for Figure 8: the three cued series (coloured, solid) plus the
# no-cue condition overlaid as a 4th series (gray, dashed, open circles).
# nocue_df has the same emmean/lower.CL/upper.CL columns as cued_df but no
# cue column (the no-cue values are identical in every SOA panel).
make_distance_plot_with_nocue <- function(cued_df, nocue_df, x_var = "target", group_var = "cue",
                                           y_label = "Response time [ms]",
                                           x_label = "Target location", plot_title = NULL,
                                           legend_title = "Cue location",
                                           color_values = plot_colors, color_labels = plot_labels_html,
                                           nocue_color = "#7F7F7F", nocue_label = "No cue") {
  nocue_html <- sprintf("<span style='color:%s;'>%s</span>", nocue_color, nocue_label)
  lev <- c(levels(cued_df[[group_var]]), nocue_label)
  d <- cued_df %>% mutate(series = as.character(.data[[group_var]])) %>%
    bind_rows(nocue_df %>% mutate(series = nocue_label)) %>%
    mutate(series = factor(series, levels = lev))
  ggplot(d, aes(x = .data[[x_var]], y = emmean, shape = series, color = series, linetype = series)) +
    geom_line(aes(group = series), position = position_dodge(.3), linewidth = .25) +
    geom_errorbar(aes(ymax = upper.CL, ymin = lower.CL), position = position_dodge(.3), width = .2, linewidth = .25) +
    geom_point(position = position_dodge(.3), size = 3, fill = "white") +
    scale_color_manual(values = c(color_values, nocue_color), labels = c(color_labels, nocue_html)) +
    scale_shape_manual(values = c(16, 17, 15, 21), labels = c(color_labels, nocue_html)) +
    scale_linetype_manual(values = c("solid", "solid", "solid", "dashed"), labels = c(color_labels, nocue_html)) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank()) +
    theme(plot.title       = element_text(size = 12, hjust = .5),
          axis.title       = element_text(size = 12),
          axis.text        = element_text(size = 12, color = "black"),
          axis.ticks       = element_line(color = "black"),
          legend.text      = element_markdown(size = 12),
          legend.direction = "horizontal",
          legend.position  = "bottom") +
    labs(title = plot_title, x = x_label, y = y_label,
         shape = legend_title, color = legend_title, linetype = legend_title)
}

# Save one pre-built ggplot as a gg.gap axis-break figure (PNG, 150 dpi).
# For print-resolution output change `res` (e.g. 600) or the device.
save_gap_figure <- function(g, dirs, file_name, segments, ylim, tick_width,
                             width = 6, height = 5, rel_heights = c(0, 0.2, 0, 1),
                             legend_margin = NULL, res = 150) {
  gapped <- gg.gap(plot = g, segments = segments, ylim = ylim,
                    tick_width = tick_width, rel_heights = rel_heights)
  png(filename = file.path(dirs$figures, paste0(file_name, ".png")),
      width = width, height = height, units = "in", res = res)
  print(gapped)
  if (!is.null(legend_margin)) suppressWarnings(add.legend(plot = g, margin = legend_margin))
  dev.off()
}

# Save two pre-built ggplots side by side (e.g. the two sound pressure level
# conditions or the two SOAs), each independently gg.gap'd, combined via cowplot.
save_gap_figure_split <- function(g1, g2, dirs, file_name, segments, ylim, tick_width,
                                   width = 9, height = 5, rel_heights = c(0, 0.2, 0, 1),
                                   legend_margin = NULL, res = 150) {
  p1 <- gg.gap(plot = g1, segments = segments, ylim = ylim, tick_width = tick_width, rel_heights = rel_heights)
  p2 <- gg.gap(plot = g2, segments = segments, ylim = ylim, tick_width = tick_width, rel_heights = rel_heights)
  combined <- plot_grid(p1, p2, ncol = 2)
  png(filename = file.path(dirs$figures, paste0(file_name, ".png")),
      width = width, height = height, units = "in", res = res)
  print(combined)
  if (!is.null(legend_margin)) suppressWarnings(add.legend(plot = g1, margin = legend_margin))
  dev.off()
}

# Raincloud (half-eye + boxplot + dots) of individual data, one distribution
# per level of x_var (Figures 5 and 10).
make_raincloud <- function(df, x_var, y_var, y_label, dots_binwidth,
                            fill_values = plot_colors,
                            x_label = "Target location",
                            x_axis_text_color = fill_values) {
  ggplot(df, aes(x = .data[[x_var]], y = .data[[y_var]], fill = .data[[x_var]], color = .data[[x_var]])) +
    stat_halfeye(adjust = .5, width = .6, justification = -.2,
                 .width = c(.5, .95), point_color = "black", alpha = .6) +
    geom_boxplot(width = .12, outlier.shape = NA, alpha = .5, color = "black") +
    stat_dots(side = "left", justification = 1.1, dotsize = 1, alpha = .5,
              binwidth = dots_binwidth, overflow = "compress") +
    scale_fill_manual(values = fill_values, guide = "none") +
    scale_color_manual(values = fill_values, guide = "none") +
    theme_minimal() +
    theme(panel.grid.minor = element_blank()) +
    theme(axis.title   = element_text(size = 12),
          axis.text.x  = element_text(size = 12, color = x_axis_text_color),
          axis.text.y  = element_text(size = 12, color = "black"),
          strip.text   = element_text(size = 12),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)) +
    labs(x = x_label, y = y_label)
}

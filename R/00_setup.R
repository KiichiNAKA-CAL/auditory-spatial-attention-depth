# ============================================
# Global setup: packages, shared constants
# ============================================
# Sourced once from AllExperimentsAnalysis.R before any task script runs.

library(tidyverse)
library(afex)
library(emmeans)
library(BayesFactor)
library(bayestestR)
library(ggtext)
library(gg.gap)
library(cowplot)
library(ggdist)

# Report generalized eta squared (ges) in addition to partial eta squared
afex_options(es_aov = "ges")

# Colour scheme for the three depth locations (0.3 / 1.3 / 2.3 m), used
# whenever a plotted variable is a loudspeaker location (target, standard, cue).
plot_colors <- c("#F8766D", "#00BA38", "#619CFF")
plot_labels_html <- c(
  "<span style='color:#F8766D;'>0.3 m</span>",
  "<span style='color:#00BA38;'>1.3 m</span>",
  "<span style='color:#619CFF;'>2.3 m</span>"
)

# Flat gray fill for the distance discrimination raincloud plots (the "pair"
# variable is not a single location, so it gets no location colour).
pair_gray_values <- rep("gray50", 3)

# Root output directory (relative to the repository root)
dir_output_root <- "output"

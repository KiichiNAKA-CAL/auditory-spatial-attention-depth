# ============================================
# Auditory spatial attention in the depth dimension -- analysis pipeline
# ============================================
# Reproduces every table, figure and statistic reported in the manuscript
# from the trial-level data in data/. Run from the repository root:
#   Rscript AllExperimentsAnalysis.R
# Outputs are written to output/<task>/{tables,anova,posthoc,figures}.

source("R/00_setup.R")
source("R/utils_common.R")
source("R/plotting_common.R")

cat("\n==== Experiment 1: sustained attention task (Tables 1-2, Figures 3-4) ====\n")
source("R/exp1_sustainedAttention.R")

cat("\n==== Experiment 1: time course within blocks (Figure 6) ====\n")
source("R/exp1_timeCourse.R")

cat("\n==== Experiment 1: distance discrimination task (Figure 5) ====\n")
source("R/exp1_localization.R")

cat("\n==== Experiment 2: spatial cueing task (Tables 3-4, Figures 8-9) ====\n")
source("R/exp2_spatialCueing.R")

cat("\n==== Experiment 2: distance discrimination task (Figure 10) ====\n")
source("R/exp2_localization.R")

cat("\nAll analyses completed.\n")

# Auditory spatial attention in the depth dimension — data and analysis code

Trial-level data, analysis scripts (R) and experiment scripts (MATLAB) for

> Naka, K., Yamasaki, D., Morita, M. M., Sun, Y., Sakamoto, S., Nagata, N., & Kitagawa, N. (2026).
> *Auditory spatial attention in the depth dimension: Gradients around the attended distance and a bias toward the listener.*
> Manuscript submitted for publication.

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.23265042.svg)](https://doi.org/10.5281/zenodo.23265042)

## Contents

```
.
├── AllExperimentsAnalysis.R       runs the whole pipeline
├── R/
│   ├── 00_setup.R                 packages, colours
│   ├── utils_common.R             factor coding, within-participant CIs, Bayes factor and sphericity tables
│   ├── plotting_common.R          figure functions
│   ├── exp1_sustainedAttention.R  Exp. 1 sustained attention task: Tables 1-2, Figures 3-4
│   ├── exp1_timeCourse.R          Exp. 1 time course within blocks: Figure 6
│   ├── exp1_localization.R        Exp. 1 distance discrimination task: Figure 5
│   ├── exp2_spatialCueing.R       Exp. 2 spatial cueing task: Tables 3-4, Figures 8-9, session halves
│   └── exp2_localization.R        Exp. 2 distance discrimination task: Figure 10
├── data/                          trial-level data, one CSV file per task
├── output/                        tables (CSV) and figures (PNG) written by the scripts
├── experiment_scripts/            MATLAB / Psychtoolbox scripts of the four tasks
├── CITATION.cff
├── LICENSE                        MIT (code)
└── DATA_LICENSE.md                CC BY 4.0 (data)
```

## The experiments in brief

Three loudspeakers were placed in a straight line in front of a blindfolded, chin-rested participant at 0.3, 1.3 and 2.3 m from the ears. All stimuli were 200-ms amplitude-modulated (25 or 75 Hz) white-noise bursts, and the participants discriminated the modulation rate with two keys.

- **Experiment 1 (N = 21), sustained attention task.** In each block, 90 % of the stimuli came from a standard location and 10 % from one of the other two locations (5 % each). Twelve blocks: 3 standard locations × 2 sound pressure level conditions (loudness-unmatched: equal sound power at the loudspeakers, i.e. 60/47/41 dB(A) at the ears; loudness-matched: 60 dB(A) at the ears for all loudspeakers) × 2 repetitions. Each block had 10 warm-up trials at the standard location followed by 100 analysed trials. Inter-trial interval 1.8–2.8 s.
- **Experiment 2 (N = 23), spatial cueing task.** A spatially uninformative 20-ms cue preceded the target at an SOA of 100 or 400 ms; cue and target locations varied randomly and independently. 630 trials per participant: 540 cued trials (3 cue locations × 3 target locations × 2 SOAs × 30) and 90 no-cue trials (3 target locations × 30), in which the target was presented at the start of the trial without a cue. Inter-trial interval 1.8–2.8 s.
- **Distance discrimination task (both experiments).** Two sounds were presented 500 ms apart from two different distances and the participant indicated which was closer. 180 trials: 3 distance pairs × 2 presentation orders × 30. In Experiment 1 the task was run once per sound pressure level condition.

Full details are given in the Method sections of the manuscript.

## Data

All files in `data/` are comma-separated with a header row; `NA` marks a missing value. They contain only trial-level numeric codes; no personal information is stored. Participants are labelled `p01`, `p02`, … within each experiment. Locations are given in metres (0.3, 1.3, 2.3), amplitude modulation rates in Hz (25, 75), and response times in seconds exactly as recorded by the task script.

### `exp1_sustained_attention.csv` — 27,720 rows (21 participants × 12 blocks × 110 trials)

| Column | Meaning |
|---|---|
| `participant` | p01 … p21 |
| `loudness_condition` | `Unmatched` (equal sound power at the loudspeakers) or `Matched` (equal sound pressure level at the ears) |
| `standard_location` | location of the standard stimulus of the block (m) |
| `block_order` | chronological position of the block in the session (1 … 12) |
| `block_rep` | repetition of the same standard × loudness condition (1 or 2) |
| `trial` | position within the block: −9 … 0 = the 10 warm-up trials at the standard location (excluded from all analyses), 1 … 100 = analysed trials |
| `target_location` | location of the stimulus (m) |
| `am_rate` | amplitude modulation rate of the stimulus (Hz) |
| `rt_s` | response time from stimulus onset (s); `NA` when no response was made before the next stimulus |
| `correct` | 1 = correct, 0 = incorrect or no response |
| `no_response` | `TRUE` for the trials without a response (0.3 % of the analysed trials) |

### `exp1_distance_discrimination.csv` — 7,560 rows (21 participants × 2 conditions × 180 trials)

| Column | Meaning |
|---|---|
| `participant`, `loudness_condition` | as above |
| `trial` | 1 … 180 |
| `first_location`, `second_location` | distances of the two sounds (m), in presentation order |
| `correct` | 1 = the closer sound was identified correctly, 0 = incorrect |

### `exp2_spatial_cueing.csv` — 14,490 rows (23 participants × 630 trials, in presentation order)

| Column | Meaning |
|---|---|
| `participant` | p01 … p23 |
| `trial` | 1 … 630 (presentation order) |
| `soa_ms` | cue–target stimulus onset asynchrony, 100 or 400; `NA` for no-cue trials |
| `cue_location` | location of the cue (m); `NA` for no-cue trials |
| `target_location` | location of the target (m) |
| `am_rate` | amplitude modulation rate of the target (Hz) |
| `rt_s` | response time from target onset (s); `NA` when no response was made within 2 s. Slightly negative values are key presses that preceded the target |
| `correct` | 1 = correct, 0 = incorrect, `NA` = no response within 2 s (0.4 % of trials; counted as neither correct nor incorrect) |
| `no_response` | `TRUE` for the trials without a response |

### `exp2_distance_discrimination.csv` — 4,140 rows (23 participants × 180 trials)

| Column | Meaning |
|---|---|
| `participant`, `trial`, `first_location`, `second_location`, `correct` | as in Experiment 1 |
| `response_mapping_corrected` | `TRUE` for participant p03, whose two response keys were reversed throughout this task (the raw correct rates were the mirror image of the group: 3–5 % where the group scored 90–100 %). `correct` has been flipped (0 ↔ 1) for this participant only |

## Running the analysis

Requirements: R ≥ 4.5 and the packages `tidyverse`, `afex`, `emmeans`, `BayesFactor`, `bayestestR`, `ggtext`, `gg.gap`, `cowplot`, `ggdist`.
The files in `output/` were produced with R 4.5.0, tidyverse 2.0.0, afex 1.5.0, emmeans 1.11.2-8, BayesFactor 0.9.12-4.7, bayestestR 0.17.0, ggtext 0.1.2, gg.gap 1.3, cowplot 1.2.0 and ggdist 3.3.3 on macOS.

From the repository root:

```sh
Rscript AllExperimentsAnalysis.R      # about one minute
```

The script writes to `output/<task>/{tables,anova,posthoc,figures}` and overwrites the committed files. Bayes factors are computed by MCMC with fixed seeds (`set.seed(42)`), so repeated runs give identical values with the same package versions. Figures are written as 150-dpi PNG files; the print versions submitted with the manuscript were rasterized from the same plots at 600 dpi.

Conventions inside the scripts and output tables: the distance discrimination task is called `localization`; locations are coded `t0.3m` / `t1.3m` / `t2.3m` (target, cue) and `near` / `mid` / `far` (standard location); distance pairs are coded `near_mid` / `near_far` / `mid_far`; accuracy is stored as an error rate (0–1) for the two discrimination tasks (correct rate = 1 − error rate) and as `correct_ratio` (0–1) for the distance discrimination task. `anova_*.csv` applies the Greenhouse–Geisser correction to every effect and `anova_*_uncorrected.csv` applies none; the manuscript reports the corrected values only for effects whose Mauchly test is significant (`sphericity_*.csv`).

## Output files and the manuscript

| Manuscript | Files under `output/` |
|---|---|
| Table 1 (Exp. 1 response times and correct rates) | `exp1_sustainedAttention/tables/summary_median_rt.csv`, `summary_error_rate.csv`, `cm_ci_median_rt.csv`, `cm_ci_error_rate.csv` |
| Table 2 (Exp. 1 ANOVA and Bayes factors) | `exp1_sustainedAttention/anova/anova_median_rt.csv`, `anova_error_rate.csv`, `*_uncorrected.csv`, `sphericity_*.csv`, `bf_inclusion_median_rt.csv`, `bf_inclusion_error_rate.csv` |
| Exp. 1 Results text, pairwise comparisons | `exp1_sustainedAttention/posthoc/posthoc_loudness_median_rt.csv`, `posthoc_target_median_rt.csv`, `posthoc_target_by_distance_median_rt.csv` |
| Figures 3 and 4 | `exp1_sustainedAttention/figures/plot_median_rt_cm.png`, `plot_correct_ratio_cm.png` |
| Figure 5 and the Exp. 1 distance discrimination results | `exp1_localization/figures/raincloud_correct_ratio_split.png`, `tables/summary_by_pair.csv`, `cm_ci_correct_ratio.csv`, `anova/anova_correct_ratio*.csv`, `sphericity_correct_ratio.csv`, `chance_test_correct_ratio.csv`, `posthoc/posthoc_pair_correct_ratio.csv` |
| Figure 6 (time course within blocks) | `exp1_timeCourse/figures/descriptive_rt_by_trialbin_ms.png`, `tables/descriptive_rt_by_trialbin.csv` |
| Table 3 (Exp. 2 response times and correct rates) | `exp2_spatialCueing/tables/summary_median_rt_cue.csv`, `summary_median_rt_nocue.csv`, `summary_error_rate_cue.csv`, `summary_error_rate_nocue.csv`, `cm_ci_*.csv` |
| Table 4 (Exp. 2 ANOVA and Bayes factors) | `exp2_spatialCueing/anova/anova_median_rt_cue.csv`, `anova_median_rt_nocue.csv`, `anova_error_rate_cue.csv`, `anova_error_rate_nocue.csv`, `*_uncorrected.csv`, `sphericity_*.csv`, `bf_*.csv` |
| Exp. 2 Results text, pairwise comparisons and simple effects | `exp2_spatialCueing/posthoc/posthoc_target_median_rt.csv`, `posthoc_soa_median_rt.csv`, `posthoc_target_by_soa_median_rt.csv`, `posthoc_soa_by_target_median_rt.csv`, `posthoc_soa_error_rate.csv` |
| Exp. 2 Results text, first versus second half of the session | `exp2_spatialCueing/tables/session_halves.csv` |
| Figures 8 and 9 | `exp2_spatialCueing/figures/plot_median_rt_split_soa_cm.png`, `plot_correct_ratio_split_soa_cm.png` |
| Figure 10 and the Exp. 2 distance discrimination results | `exp2_localization/figures/raincloud_correct_ratio.png`, `tables/summary_by_pair.csv`, `cm_ci_correct_ratio.csv`, `anova/*.csv`, `posthoc/posthoc_pair_correct_ratio.csv` |

The participant-level cell values that enter the ANOVAs are in `tables/median_rt_by_*.csv`, `error_rate_by_*.csv` and `correct_ratio_by_pair.csv`. Figures 1, 2 and 7 are schematic drawings and are not produced by the scripts.

## Contact

Kiichi Naka, University of Nagasaki (kiichinaka@sun.ac.jp).

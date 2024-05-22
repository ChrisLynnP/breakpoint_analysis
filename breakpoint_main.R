# This is the main code of the project, using the functions from 
# breakpoint_functions.R to evaluate the performance of the algorithm on real and
# simulated data.

if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}

library(tidyverse)
library(readxl)

source("breakpoint_functions.R")

# SIMULATED DATA EVALUATION #

result_df_plateau <- save_100_results(data_points = 10)
boxplot_breakpoints(result_df_plateau)

result_df_peak <- save_100_results(plateau = F, data_points = 10, BP_1 = 50, BP_2 = 75)
boxplot_breakpoints(result_df_peak, plateau = F)

# REAL DATA EVALUATION #

# PART 1: categorizing

# Load in the data

real_data <- read_excel("psilo.xlsx")
real_data_by_study <- split(real_data, real_data$Study)

lpm <- real_data_by_study[[1]][, -1]
lps <- real_data_by_study[[2]][, -1]
lps_l <- real_data_by_study[[3]][, -1]
sps <- real_data_by_study[[4]][, -1]

# Characterization

# LPM_character <- c(0, 0, 0, 1,
#                    1, 0, 0, 1,
#                    0, 1, 1, 1,
#                    0, 0, 1, 1,
#                    0, 0, 0, 0,
#                    1, 1, 0, 1,
#                    1, 1, 0, 1,
#                    1, 0, 1, 1)

# LPS_character <- c(0, 1, 0, 0,
#                    1, 0, 1, 1,
#                    0, 1, 0, 0,
#                    0, 0, 0, 0,
#                    1, 1, 0, 0,
#                    1, 1, 0, 1,
#                    1, 1, 1, 0)

# LPS_L_character <- c(0, 1, 1, 1,
#                      1, 1, 0, 1,
#                      0, 1, 1, 1,
#                      0, 0, 0, 0,
#                      1, 0, 0, 1,
#                      1, 1, 0, 1,
#                      1, 1, 1, 1)

SPS_character <- c(1, 0, 0, 1,
                   2, 1, 0, 0,
                   2, 1, 2, 2,
                   0, 1, 0, 0,
                   0, 0, 0, 0,
                   0)

plot_real_data(sps, SPS_character)
sps_results <- estimate_bp_real_data(sps, SPS_character)
full_plot_breakpoint(sps_results, "SPS Dataset")
simplified_plot_breakpoint(sps_results, "SPS Dataset")

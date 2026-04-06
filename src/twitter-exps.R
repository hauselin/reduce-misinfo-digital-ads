# analysis for individual twitter experiments, R1, R2, R3, NR

# %% set up =================================================

rm(list = ls())
library(data.table)
library(glue)
library(here)
library(fixest)

# %% domain-level analyses for all studies

#' Analyze a Twitter experiment for a given experiment and domain quality threshold. Reads data from 'data/twitter-exps' and fits a fixed-effects GLM model.
#'
#' This function loads the corresponding preprocessed data for a specified
#' experiment (e.g., "R1", "R2", "R3", or "NR") and threshold value,
#' fits a fixed-effects generalized linear model (GLM) for the outcome 't1'
#' as a function of treatment assignment ('conditionC'), pre-treatment score ('t0SC'),
#' with fixed effects for 'block' and 'day', and clustering on 'block'.
#' The model uses a quasi-Poisson family appropriate for count data.
#'
#' @param experiment Character. Experiment identifier, e.g., "R1", "R2", "R3", or "NR". Default is "R1".
#' @param threshold Numeric. Quality threshold value to select the input data file. Default is 70 (result/value reported in main text). Ranges from 40 to 80, in steps of 5.
#' @return The fitted fixest GLM model object.
#' @examples
#' analyze_experiment("R1", 70)
#' analyze_experiment("NR", 60)
analyze_experiment <- function(experiment = "R1", threshold = 70) {
    datafile <- here("data", "twitter-exps", glue("exp-{experiment}_thres-{threshold}.csv"))
    stopifnot(file.exists(datafile))
    print(glue("\n\nAnalyzing experiment {experiment} with domain quality threshold {threshold}...\n- data file: {datafile})\n\n"))
    mod <- feglm(t1 ~ conditionC * t0SC | block + day,
        data = fread(datafile),
        family = "quasipoisson",
        cluster = "block"
    )
    return(mod)
}

analyze_experiment("R1", 70)


# %%

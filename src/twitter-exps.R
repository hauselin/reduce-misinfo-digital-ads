#' analysis for individual twitter experiments, R1, R2, R3, NR
#' loads data from 'data/twitter-exps' and fits a fixed-effects GLM model.
#' see SI section S2.2.1 for disaggregated results and S2.2.3 for multiverse analysis.

# %% set up =================================================

rm(list = ls())
library(data.table)
library(glue)
library(here)
library(fixest)

# %% domain-level analyses for experiments R1, R2, R3, NR

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

# read data and fit model to each experiment, specifying the experiment and threshold
analyze_experiment("R1", 70)
analyze_experiment("R2", 70) # low-quality domain count, not hashtag (note that main text uses hashtag count, see section below)
analyze_experiment("R3", 70)
analyze_experiment("NR", 70)


# %% hashtag analysis for experiment R2

#' Experiment R2 uses hashtag data, not domain data, so we need to use a different data file.

d1 <- fread(here("data", "twitter-exps", "exp-R2_hashtag.csv"))
mod <- feglm(t1 ~ conditionC * t0SC | block + day, d1, family = "quasipoisson", cluster = "block")
mod

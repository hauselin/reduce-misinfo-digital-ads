#' analysis for main result (quality threshold 70, plus results for all thresholds from 40 to 80, in steps of 5)
#' loads data from 'data/twitter-meta' and fits a meta-analysis model.

# %% set up =================================================

rm(list = ls())
library(tidyverse)
library(data.table)
library(meta)
library(here)
source(here("src", "utils.R"))

# get file paths - one per threshold
files <- list.files(here("data", "twitter-meta"), full.names = TRUE, pattern = "meta-treatment-threshold-")
length(files)

# %%

get_result <- function(model, threshold) {
    return(
        data.table(
            threshold = threshold,
            estimate = model$TE.fixed,
            se = model$seTE.fixed,
            low = model$lower.fixed,
            high = model$upper.fixed,
            pval = model$pval.fixed
        )
    )
}

models_itt <- list()
models_att <- list()
meta_itt_results <- list()
meta_att_results <- list()
thresholds <- c()

# loop through each file/threshold
for (i in 1:length(files)) {
    d2 <- fread(files[i])
    threshold <- as.character(d2$threshold[1])
    thresholds <- c(thresholds, threshold)

    # fit ITT model
    m0 <- metagen(TE = raw_estimate, seTE = raw_estimate_se, data = d2, method.tau = "REML", common = TRUE)
    print(m0)
    models_itt[[i]] <- m0

    # fit ATT model
    m1 <- metagen(TE = raw_estimate_att, seTE = se_att, data = d2, method.tau = "REML", common = TRUE)
    print(m1)
    models_att[[i]] <- m1

    d0 <- get_result(m0, threshold)
    meta_itt_results[[i]] <- d0

    d1 <- get_result(m1, threshold)
    meta_att_results[[i]] <- d1
}
names(models_itt) <- thresholds
names(models_att) <- thresholds
names(meta_itt_results) <- thresholds
names(meta_att_results) <- thresholds

# %% main result: quality threshold 70

thres <- "70"
meta_itt_results[[thres]]
summ_metagen(models_itt[[thres]])

meta_att_results[[thres]]
summ_metagen(models_att[[thres]])

# %% results for all thresholds from 40 to 80, in steps of 5 (S2.2.3 multiverse analysis)

rbindlist(meta_itt_results)
models_itt

rbindlist(meta_att_results)
models_att

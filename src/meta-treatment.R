rm(list = ls())
library(tidyverse); library(data.table); library(glue); library(meta)
source("utils.R")
theme_set(theme_minimal())

# parameters
files <- list.files("../data/clean", full.names = TRUE, pattern = "meta-effect-threshold-")
length(files)

models_itt <- list()
models_att <- list()
meta_itt_results <- list()
meta_att_results <- list()
thresholds <- c()

for (i in 1:length(files)) {
    d2 <- fread(files[i])
    threshold <- as.character(d2$threshold[1])
    thresholds <- c(thresholds, threshold)
    
    # fit ITT model
    m0 <- metagen(TE = raw_estimate, seTE = raw_estimate_se, data = d2, method.tau = "REML", fixed = TRUE)
    print(m0)
    models_itt[[i]] <- m0
    
    # fit ATT model
    m1 <- metagen(TE = raw_estimate_att, seTE = se_att, data = d2, method.tau = "REML", fixed = TRUE)
    print(m1)
    models_att[[i]] <- m1
    
    # save ITT results
    d0 <- data.table(
        threshold = threshold,
        estimate = m0$TE.fixed,
        se = m0$seTE.fixed, 
        low = m0$lower.fixed, 
        high = m0$upper.fixed, 
        pval = m0$pval.fixed)
    meta_itt_results[[i]] <- d0
    
    # save ATT results
    d1 <- data.table(
        threshold = threshold,
        estimate = m1$TE.fixed,
        se = m1$seTE.fixed, 
        low = m1$lower.fixed, 
        high = m1$upper.fixed, 
        pval = m1$pval.fixed)
    meta_att_results[[i]] <- d1
}
names(models_itt) <- thresholds
names(models_att) <- thresholds
names(meta_itt_results) <- thresholds
names(meta_att_results) <- thresholds

# main result: threshold 70
meta_itt_results[["70"]]  # ITT
summ_metagen(models_itt$`70`)

meta_att_results[["70"]]  # ATT
summ_metagen(models_att$`70`)

# results for all thresholds (supplement)
meta_itt_results
models_itt

meta_att_results
models_att




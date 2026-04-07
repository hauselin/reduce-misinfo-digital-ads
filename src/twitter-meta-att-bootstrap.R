# bootstrap analysis for ATT effect at quality threshold 70 for experiments R1 and R3, and hashtag count analysis for experiment R2

# %% set up =================================================

rm(list = ls())
library(tidyverse)
library(data.table)
library(glue)
library(here)
library(broom)
library(parallel)
library(fixest)

# %% define bootstrap functions

#' create a bootstrap sample and fit model to it.
#' See methods for details: To estimate the ATT effects, separately for each experiment, we divide the ITT estimates by the proportion of users that saw the ads (compliance rates of 0.513, 0.643, and 0.625 for the three experiments). The standard errors for the ATT effects were computed via bootstrapping. For each model, we create 5,000 bootstrap samples by sampling with replacement at the level of the randomization block, and the bootstrapped standard errors are used as inputs to the inverse-variance weighted meta-analysis.
bootstrap_func <- function(i, data) {
    # create bootstrap sample - cluster on block
    blocks <- data[, unique(block)]
    boot_blocks <- sample(blocks, size = length(blocks), replace = TRUE)

    # determine how many times to sample each user (bootstrap clustered on block)
    dt_block <- data.table(block = boot_blocks)
    dt_block_n <- dt_block[, .N, block][order(-N)]

    # blocks that were sampled once
    once <- dt_block_n[N == 1, block]
    d11 <- data[block %in% once]

    # blocks that were sampled multiple times
    multiple <- dt_block_n[N > 1, ][order(-N, block)]
    start_block_idx <- max(blocks) * 2
    dt_list <- list()
    rep_idx <- 0
    for (b in 1:nrow(multiple)) {
        block_idx <- multiple[b, block]
        block_rep_n <- multiple[b, N]
        temp_block <- data[block == block_idx]
        for (r in 1:block_rep_n) {
            rep_idx <- rep_idx + 1
            start_block_idx <- start_block_idx + 1
            temp_block$block <- start_block_idx
            dt_list[[rep_idx]] <- temp_block
        }
    }
    d12 <- rbindlist(dt_list)
    d4 <- data.table(bind_rows(d11, d12))
    boot_user_ids <- unique(d4$user_id)

    # check that the sampling was done correctly
    stopifnot(n_distinct(d4$block) == n_distinct(data$block))

    mod <- feglm(t1 ~ conditionC * t0SC | block + day,
        data = d4,
        family = "quasipoisson", cluster = "block",
        only.coef = TRUE, notes = FALSE, warn = FALSE
    )
    results <- data.table(term = names(mod), estimate = mod)
    results$unique_users <- n_distinct(boot_user_ids)
    results$unique_blocks <- n_distinct(boot_blocks)
    results$idx <- i
    return(results)
}

# compute SEs for ITT and ATT effects from bootstrap samples
get_boot_results <- function(data, coefficient, compliance, digits = 10) {
    dt_results <- data.table(bind_rows(data))
    n_boots <- dt_results[, n_distinct(idx)]

    # itt
    b <- round(dt_results[term == coefficient, mean(estimate)], digits)
    cis <- round(dt_results[term == coefficient, quantile(estimate, c(0.025, 0.975))], digits)
    se <- round(dt_results[term == coefficient, sd(estimate)], digits)

    # att
    dt_results[, estimate := estimate / compliance]
    att_cis <- round(dt_results[term == coefficient, quantile(estimate, c(0.025, 0.975))], digits)
    att_se <- round(dt_results[term == coefficient, sd(estimate)], digits)

    results <- data.table(bind_cols(
        data.table(
            bootstrap_itt_se = c(se),
            bootstrap_itt_lb = c(cis[1]),
            bootstrap_itt_ub = c(cis[2])
        ),
        data.table(
            bootstrap_att_se = c(att_se),
            bootstrap_att_lb = c(att_cis[1]),
            bootstrap_att_ub = c(att_cis[2])
        )
    ))

    results$term <- coefficient
    results$n_boots <- n_boots

    return(results)
}

# perform bootstrap analysis for a particular experiment/datafile
perform_bootstrap_analysis <- function(file, n_bootstrap_samples = 5000) {
    filepath <- here("data", "twitter-exps", file)
    stopifnot(file.exists(filepath))
    d1 <- fread(filepath)

    PROP_REACHED <- c(R1 = 0.5132, R2 = 0.6429, R3 = 0.6254, NR = 0.6180)
    exp_id <- sub("exp-([^_]+)_.*", "\\1", file)
    stopifnot(exp_id %in% names(PROP_REACHED))
    prop_reached <- PROP_REACHED[exp_id]
    print(glue("exp_id {exp_id} proportion reached (compliance rate): {prop_reached}"))

    # fit ITT model
    mod <- feglm(t1 ~ conditionC * t0SC | block + day, d1, family = "quasipoisson", cluster = "block")
    tidy_model <- data.table(tidy(mod))

    # parallel bootstrap
    n_cores <- detectCores() - 1 # use all but one core
    results <- mclapply(1:n_bootstrap_samples, function(x) bootstrap_func(x, d1), mc.cores = n_cores)

    # process results for each term in the model
    terms <- results[[1]][, unique(term)]
    dt_bootstrap_results <- rbindlist(lapply(terms, function(term) {
        get_boot_results(results, term, prop_reached, 10)
    }))

    dt_itt_results <- tidy_model[, .(term, itt_estimate = estimate, itt_raw_se = std.error)]
    dt_raw_att_se <- dt_itt_results[, .(term,
        att_raw_se = itt_raw_se / prop_reached
    )]
    merged <- merge(dt_itt_results, dt_raw_att_se, by = "term")
    bootstrap_results <- merge(merged, dt_bootstrap_results, by = "term")
    return(bootstrap_results)
}


# %%

#' NOTE
#' This script will take a while to run if you use the default number of bootstrap samples (5000)!
#' bootstrapped ATT SEs are already saved in the data in data/twitter-meta

exp_r1 <- perform_bootstrap_analysis("exp-R1_thres-70.csv")
exp_r2 <- perform_bootstrap_analysis("exp-R2_hashtag.csv")
exp_r3 <- perform_bootstrap_analysis("exp-R3_thres-70.csv")

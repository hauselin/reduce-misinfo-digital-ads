# SI section S2.2.3 multiverse analysis

# %% set up =================================================

rm(list = ls())
library(tidyverse)
library(data.table)
library(glue)
library(patchwork)
library(here)
theme_set(theme_minimal())

# %% load data and prepare for plotting

d1 <- fread(here("data", "twitter-meta", "multiverse-meta_itt_results_heterogeneity.csv"))
d1[, .N, keyby = n_exps]
d1[, I2 := I2 * 100]
d1[, winsorize := factor(winsorize)]

# reorder outcome levels for plotting
d1[, outcome := factor(outcome, levels = c("count", "sum", "frac"), labels = c("Count", "Sum", "Fraction"))]
# rename trucker levels
d1[, trucker := factor(trucker, levels = c("domain", "hashtag"), labels = c("Experiment R2 outcome: Domain", "Experiment R2 outcome: Hashtag"))]

# %% I^2 heterogeneity plot (s2.2.3 figure s13)

d1[, I2_gt_40 := ifelse(I2 > 40, 1, 0)]
d1[, .(I2_gt_40 = round(mean(I2_gt_40) * 100, 1)), keyby = .(outcome)][order(-I2_gt_40)]
d1[, txt := glue_data(.SD, "{round(I2,2)}%")]

ggplot(d1, aes(winsorize, threshold, fill = I2)) +
    facet_grid(trucker ~ outcome, labeller = labeller(outcome = function(x) paste0("Outcome: ", x))) +
    geom_tile() +
    geom_text(aes(label = txt), col = "white", size = 5) +
    scale_y_continuous(breaks = seq(40, 80, 5)) +
    scale_fill_viridis_c(begin = 0, end = 0.8) +
    labs(x = "Winsorization", y = "Domain quality threshold") +
    theme(
        legend.position = "none",
        # plot.title = element_text(hjust = 0.5, size = 15),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 15),
        strip.text = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18)
    )

ggsave(here("figures", "fig-s13-I2-heterogeneity.png"), dpi = 300, bg = "white", width = 15, height = 12)

# %% Q-statistic comparing experiments R1-3 versus NR (meta-regression) (s2.2.3 figure s14)

d2 <- copy(d1)
d2[, txt := glue_data(.SD, "{round(Qm_pval,3)}")]
d2[, Qm_pval_sig := ifelse(Qm_pval <= 0.05, 1, 0)]
d2[, .(Qm_pval_sig = round(mean(Qm_pval_sig) * 100, 1)), keyby = .(outcome)][order(-Qm_pval_sig)]

ggplot(d2, aes(winsorize, threshold, fill = Qm_pval)) +
    facet_grid(trucker ~ outcome, labeller = labeller(outcome = function(x) paste0("Outcome: ", x))) +
    geom_tile() +
    geom_tile(
        data = d2[Qm_pval_sig == 1],
        aes(winsorize, threshold),
        fill = NA, color = "red", size = 1.2, linewidth = 0.8
    ) +
    geom_text(aes(label = txt), col = "white", size = 5) +
    scale_y_continuous(breaks = seq(40, 80, 5)) +
    scale_fill_viridis_c(begin = 0.1, end = 0.7) +
    labs(x = "Winsorization", y = "Domain quality threshold") +
    theme(
        legend.position = "none",
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 15),
        strip.text = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18)
    )

ggsave(here("figures", "fig-s14-Qm-pval.png"), dpi = 300, bg = "white", width = 15, height = 12)


# %% I^2 residual (s2.2.3 figure s15)

d3 <- copy(d1)
d3[, txt := glue_data(.SD, "{round(I_resid,2)}%")]
d3[, I_resid_eq_0 := ifelse(I_resid == 0, 1, 0)]
d3[, .(I_resid_eq_0 = round(mean(I_resid_eq_0) * 100, 1)), keyby = .(outcome)][order(-I_resid_eq_0)]

ggplot(d3, aes(winsorize, threshold, fill = I_resid)) +
    facet_grid(trucker ~ outcome, labeller = labeller(outcome = function(x) paste0("Outcome: ", x))) +
    geom_tile() +
    geom_text(aes(label = txt), col = "white", size = 5) +
    scale_y_continuous(breaks = seq(40, 80, 5)) +
    scale_fill_viridis_c(begin = 0.1, end = 0.7) +
    labs(x = "Winsorization", y = "Domain quality threshold") +
    theme(
        legend.position = "none",
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 15),
        strip.text = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18)
    )

ggsave(here("figures", "fig-s15-I_resid.png"), dpi = 300, bg = "white", width = 15, height = 12)


# %% ITT effects across exps R1, R2, R3 (s2.2.3 figure s16)


d10 <- fread(here("data", "twitter-meta", "multiverse-meta_itt_results.csv"))
d10[, .N, keyby = n_exps]
d10[, psig := ifelse(pval <= 0.05, 1, 0)]
d10[, .(prop_sig = round(mean(psig), 3)), keyby = .(outcome)][order(-prop_sig)]

d10[, winsorize := factor(winsorize)]
d10[outcome == "frac", estimate := estimate * 100]
d10[outcome == "frac", se := se * 100]

# make factor for outcome: Count, Sum, Fraction
d10[, outcome := factor(outcome, levels = c("count", "sum", "frac"), labels = c("Count", "Sum", "Fraction"))]
setnames(d10, "outcome", "Outcome")

# trucker: hashtag, domain
d10[, Trucker := factor(trucker, levels = c("domain", "hashtag"), labels = c("Experiment R2 outcome: Domain", "Experiment R2 outcome: Hashtag"))]

d10[, txt := glue_data(.SD, "{round(estimate, 3)}\n({round(se, 3)})")]

ggplot(d10, aes(winsorize, threshold, fill = estimate)) +
    facet_grid(Trucker ~ Outcome, labeller = labeller(Outcome = label_value, Trucker = label_value)) +
    geom_tile() +
    geom_tile(
        data = d10[psig == 1],
        aes(winsorize, threshold),
        fill = NA, color = "red", size = 1.2, linewidth = 0.8
    ) +
    geom_text(aes(label = txt), col = "white") +
    scale_fill_viridis_c(begin = 0.1, end = 0.7) +
    labs(x = "Winsorization", y = "Domain quality threshold") +
    theme(
        legend.position = "none",
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 15),
        strip.text = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18)
    )

ggsave(here("figures", "fig-s16-meta_itt_results.png"), dpi = 300, bg = "white", width = 15, height = 12)

# %%

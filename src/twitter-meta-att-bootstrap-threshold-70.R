# bootstrap analysis for ATT effect at quality threshold 70

# %% set up =================================================

rm(list = ls())
library(tidyverse)
library(data.table)
library(glue)
library(patchwork)
library(here)
library(boot)
theme_set(theme_minimal())

# %%

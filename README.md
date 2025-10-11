# =============================================================
# README: Cost-Effectiveness Analysis (CEA) Pipeline
# =============================================================
# This script runs three regression-based CEA methods (SUR, OLS, GLM)
# using bootstrap resampling. It estimates incremental costs, incremental
# QALYs, ICERs, and visualizes uncertainty via cost-effectiveness planes
# and cost-effectiveness acceptability curves (CEACs).We have provided an example dataset (example dataset.csv, n=100).
# =============================================================

# -----------------------------
# 1. Required Packages & Setup
# -----------------------------
R_version <- "4.4.1"
required_packages <- c(
  "mice", "systemfit", "car", "boot", "ggplot2",
  "ggpointdensity", "readxl", "tidyverse"
)

installed <- rownames(installed.packages())
for (pkg in required_packages) {
  if (!(pkg %in% installed)) install.packages(pkg, dependencies = TRUE)
}

library(mice)           # v3.18.0
library(systemfit)      # v1.1-30
library(car)            # v3.1-2
library(boot)           # v1.3-30
library(ggplot2)        # v3.5.0
library(ggpointdensity) # v0.2.0
library(readxl)         # v1.4.3
library(tidyverse)      # v2.0.0

# -----------------------------
# 2. Data Import
# -----------------------------
dataset <- read.csv("example dataset.csv", header = TRUE)

# -----------------------------
# 3. Data Dictionary
# -----------------------------
# class:      0 = control, 1 = intervention
# societal:   cost from the societal perspective for over 6 months
# QALYs:      Quality-adjusted life years
# societal_m0:    Baseline cost from the societal perspective
# QALYs_m0:   Baseline QALYs
# age:        1 = <30 years; 2 = ≥30 years
# group:      1 = daily PrEP; 2 = event-driven PrEP
# income:     1 = ≤3999; 2 = 4000–7999; 3 = ≥8000 (CNY)
# casual:     1 = has casual male partner; 2 = none
# units:      currency = USD

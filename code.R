#Load required packages
library(systemfit) # For Seemingly Unrelated Regression (SUR) analysis
library(car) # For running SUR models
library(boot) # For bootstrap methods
library(ggplot2) # For cost-effectiveness plane and CEAC plots
library(ggpointdensity) # For enhanced cost-effectiveness plane visualization
library(readxl) # For importing .xlsx format data
library(tidyverse) # For data manipulation

#Data import
dataset <- read.csv("C:/Users/xxx/xxx.csv", header = TRUE)

#1.Define SUR model fitting function
set.seed(3407) # Set random seed for reproducibility
fsur <- function(x, i) {
  dataset <- x[i,] # Extract bootstrap sample
  r1 <- cost ~ class + cost_m0 + xxx + xxx + xxx + xxx # Cost regression model (class = treatment group variable)
  r2 <- effectiveness ~ class + effectiveness_m0 + xxx + xxx + xxx + xxx # Effectiveness regression model
  fitsur <- systemfit(list(costreg = r1, effectreg = r2), "SUR", data = dataset) # Fit SUR model
  betas <- fitsur$coefficients # Extract regression coefficients
  return(c(betas[["costreg_class"]], betas[["effectreg_class"]])) # Return incremental costs and effects
}

#Implement bootstrap procedure
bootce <- boot(data = dataset, statistic = fsur, R = 5000) # Perform bootstrap with 5000 replicates

#Extract bootstrap statistics
postboot <- as.data.frame(bootce$t) # Extract all bootstrap results
colnames(postboot) <- c("bootcost_diff", "booteffect_diff") # Rename columns

#Calculate incremental costs, effects and ICER
cost_diff_pooled <- mean(postboot$bootcost_diff) # Mean incremental cost
effect_diff_pooled <- mean(postboot$booteffect_diff) # Mean incremental effect
ICER <- cost_diff_pooled / effect_diff_pooled # Calculate ICER

#Compute covariance matrix
cov_boot <- cov(postboot) # Calculate covariance matrix from bootstrap samples

#Simplified covariance calculation
cov_pooled <- cov_boot # Pooled covariance equals bootstrap covariance

#Calculate point estimates (incremental costs and effects)
cost_diff_pooled <- mean(postboot$bootcost_diff)
effect_diff_pooled <- mean(postboot$booteffect_diff)

#Calculate confidence intervals (using bootstrap covariance)
Za <- 1.96 # Z-value for 95% CI
LL_cost_pooled <- cost_diff_pooled - Za * sqrt(cov_pooled[1, 1])
UL_cost_pooled <- cost_diff_pooled + Za * sqrt(cov_pooled[1, 1])
LL_effect_pooled <- effect_diff_pooled - Za * sqrt(cov_pooled[2, 2])
UL_effect_pooled <- effect_diff_pooled + Za * sqrt(cov_pooled[2, 2])

#Generate cost-effectiveness plane
boot <- postboot # Consolidated bootstrap samples

# Calculate point estimates
cost_mean <- mean(boot$bootcost_diff) # Mean incremental cost
effect_mean <- mean(boot$booteffect_diff) # Mean incremental effect

p <- ggplot(boot, aes(x = booteffect_diff, y = bootcost_diff)) +
  geom_point(colour = "darkgrey", shape = 1) +
  coord_cartesian(ylim = c(0,600), xlim = c(0,0.05)) +
  stat_ellipse(type = "norm",
               colour = "steelblue",
               level = 0.95, size = 1,
               linetype = 2) +
  geom_point(aes(x = mean(booteffect_diff), y = mean(bootcost_diff)),
             colour = "orange",
             size = 5,
             shape = 17) +
  geom_abline(aes(intercept = 0, slope = 10400, color = "darkgreen"), linetype = 5, show.legend = TRUE) +
  geom_abline(aes(intercept = 0, slope = 20800, color = "darkred"), linetype = 5, show.legend = TRUE) +
  geom_abline(aes(intercept = 0, slope = 31200, color = "slateblue"), linetype = 5, show.legend = TRUE) +
  scale_colour_manual(name = "WTP",
                      labels = c("$10,400/QALY","$20,800/QALY","$31,200/QALY"),
                      values = c("darkgreen","darkred","slateblue")) +
  theme_bw() +
  theme(axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = c(0.8,0.15))

# Cost-effectiveness acceptability curve (CEAC)
# Calculate incremental net benefit (INB)
wtp <- seq(0, 35000, 100) # Willingness-to-pay (WTP) range
INB <- (wtp * effect_diff_pooled) - cost_diff_pooled # Calculate INB
varINB <- wtp^2 * cov_pooled[2, 2] + cov_pooled[1, 1] - 2 * wtp * cov_pooled[1, 2] # INB variance
seINB <- sqrt(varINB) # Standard error
z <- INB / seINB # Z-score
CEAC <- as.data.frame(wtp) # Convert to dataframe
CEAC$prob <- pnorm(z, 0, 1) # Probability of cost-effectiveness

# Plot CEAC curve
CEAC <- ggplot(data = CEAC, aes(x = wtp, y = prob)) +
  geom_line(colour = "black", size = 1) +
  ylim(0, 1) +
  labs(x = "Willingness-to-pay: incremental costs per QALY gained") +
  labs(y = "Probability of cost-effectiveness") +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  theme_minimal()

# 2.Ordinary Least Squares (OLS) Regression Implementation
set.seed(3407)  # Set random seed for reproducibility

# Define OLS regression function for bootstrap
fols <- function(x, i) {
  dataset <- x[i, ]  # Extract bootstrap sample
  
  #Cost model specification (linear regression)
  ols_cost <- lm(cost ~ class + cost_m0 + xxx + xxx + xxx + xxx, 
                 data = dataset)
  
  #Effectiveness model specification (linear regression)
  ols_qalys <- lm(effectiveness ~ class + effectiveness_m0 + xxx + xxx + xxx + xxx, 
                  data = dataset)
  
  # Extract treatment coefficients (incremental cost and QALYs)
  cost_coef <- coef(ols_cost)["class"]  # Intervention coefficient from cost model
  qalys_coef <- coef(ols_qalys)["class"]  # Intervention coefficient from QALY model
  
  return(c(cost_coef, qalys_coef))  # Return incremental cost and QALYs
}

# Execute bootstrap procedure
bootce <- boot(data = dataset, statistic = fols, R = 5000)  # 5000 bootstrap iterations

#Extract bootstrap statistics
postboot <- as.data.frame(bootce$t)  # Matrix of bootstrap results
colnames(postboot) <- c("bootcost_diff", "booteffect_diff")  # Appropriate column naming

#Calculate point estimates and ICER
cost_diff_pooled <- mean(postboot$bootcost_diff)  # Mean incremental cost
effect_diff_pooled <- mean(postboot$booteffect_diff)  # Mean incremental QALYs
ICER <- cost_diff_pooled / effect_diff_pooled  # Incremental Cost-Effectiveness Ratio

#[Identical covariance and CI calculations as SUR implementation...]

#3.Generalized Linear Model (GLM) Implementation
set.seed(3407)  # Reset random seed

# Define GLM bootstrap function
fsur_glm <- function(x, i) {
  dataset <- x[i, ]  # Draw bootstrap sample
  
  #Cost model: Gamma distribution with log link
  cost_model <- glm(
    cost ~ class + cost_m0 + xxx + xxx + xxx + xxx,
    family = Gamma(link = "log"),  # Appropriate for right-skewed cost data
    data = dataset
  )
  
  #Effectiveness model: Gamma distribution (Gaussian if PADS)
  qaly_model <- glm(
    effectiveness ~ class + effectiveness_m0 + xxx + xxx + xxx + xxx,
    family = Gamma(link = "log"),  
    data = dataset
  )
  
  # Counterfactual prediction approach
  intervention_data <- dataset  # Create copy for intervention scenario
  control_data <- dataset  # Create copy for control scenario
  intervention_data$class <- 1  # All subjects assigned to intervention
  control_data$class <- 0  # All subjects assigned to control
  
  # Predict potential outcomes
  pred_cost_intervention <- predict(cost_model, newdata = intervention_data, type = "response")
  pred_cost_control <- predict(cost_model, newdata = control_data, type = "response")
  inc_cost <- mean(pred_cost_intervention - pred_cost_control)  # Average treatment effect on costs
  
  pred_qaly_intervention <- predict(qaly_model, newdata = intervention_data, type = "response")
  pred_qaly_control <- predict(qaly_model, newdata = control_data, type = "response")
  inc_qaly <- mean(pred_qaly_intervention - pred_qaly_control)  # Average treatment effect on QALYs
  
  return(c(inc_cost, inc_qaly))  # Return differential outcomes
}

# Execute GLM bootstrap
bootce <- boot(data = dataset, statistic = fsur_glm, R = 5000)  # 5000 replicates

#[Identical post-processing and visualization as previous implementations...]



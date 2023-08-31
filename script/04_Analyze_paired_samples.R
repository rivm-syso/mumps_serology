################################################################################
#
# Copyright 2023 Rijksinstituut voor Volksgezondheid en Milieu (RIVM).
#
# This program is free software: you can redistribute it and/or modify it under 
# the terms of the GNU Affero General Public License as published by the Free 
# Software Foundation, either version 3 of the License, or any later version.
# This program is distributed in the hope that it will be useful, but WITHOUT ANY 
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR 
# A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with 
# this program.  If not, see <https://www.gnu.org/licenses/>.”
#
################################################################################
#
# Mumps serology analysis
# - analysis of paired serology samples in stan
#
################################################################################

# some reasonable initial values for Stan
mm_pre = mean(mumps_data$Pre)
sd_pre = sqrt(var(mumps_data$Pre))

# initial values of parameters for Stan
initial_values = function(){
  return(
    list(
      Mu_Uninf = mm_pre, 
      Sigma_Uninf = sd_pre, 
      Mu_Inf = 11, 
      Sigma_Inf = 1, 
      ProbInf1 = 0.01,
      ProbInf4 = 0.99,
      Sigma_Random = 0.35,
      Ind_Titer_Level = mumps_data$Pre
      )
  )
} 

# data for Stan
data_values = list(
  'N'= nrow(mumps_data),
  'Titer_Pre' = mumps_data$Pre,
  'Titer_Post' = mumps_data$Post, 
  'Prior_Mu_Pre' = mm_pre, 
  'Prior_Sigma_Pre' = sd_pre,
  Mode = 0
  )  

# compile Stan model
model <- stan(
  file = "./script/stan/analyze_paired_samples.stan", 
  data = data_values, 
  init = initial_values, 
  iter = 0, 
  chains = 0)

# run Stan model
fit <- stan(
  fit = model, 
  data = data_values, 
  init = initial_values, 
  iter = 10000, 
  warmup = 5000, 
  thin = 5, 
  chains = 10, 
  control = list(adapt_delta = 0.95, max_treedepth = 12)
)

saveRDS(fit, "results/fit.rds")

print(fit)

# traceplots
traceplot(
  fit, 
  pars = c(
    "Mu_Uninf", 
    "Mu_Inf",
    "Sigma_Uninf", 
    "Sigma_Inf", 
    "Sigma_Random",
    "x0", 
    "kpar",
    "ProbInf1",
    "ProbInf2",
    "ProbInf4",
    "prob_inf_pred_overall"
  )
)

# print estimates
print(fit, digits=5, 
      pars=c(
        "Mu_Uninf",
        "Sigma_Uninf", 
        "Sigma_Inf", 
        "Sigma_Random", 
        "Mu_Inf", 
        "x0",
        "kpar",
        "ProbInf1", 
        "ProbInf2", 
        "ProbInf4", 
        "intraclass_corr",
        "prob_inf_pred_overall"
      )
)
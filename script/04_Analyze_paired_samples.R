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

# remove paired samples with more than fourfold titer decrease
mumps_data <- mumps_data_all |> 
  filter(Pre - Post < 2)

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
  iter = 7000, 
  warmup = 2000, 
  thin = 5, 
  chains = 10, 
  control = list(adapt_delta = 0.99, max_treedepth = 15)
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
    "ProbInf1",
    "ProbInf4", 
    "x0", 
    "kpar"
  )
)


# print estimates
print(fit, digits=4, 
      pars=c(
        "Mu_Uninf",
        "Sigma_Uninf", 
        "Sigma_Inf", 
        "Sigma_Random", 
        "Mu_Inf", 
        "x0",
        "kpar",
        "ProbInf4", 
        "ProbInf1", 
        "intraclass_corr"
      )
)

pareto_k_ids(loo_output, threshold = 1.1)

traceplot(
  fit, 
  pars = c(
    "pjump[8]",
    "pjump[133]",
    "pjump[188]",
    "pjump[571]",
    "pjump[647]",
    "pjump[136]",
    "pjump[157]",
    "pjump[285]"
  )
)

traceplot(
  fit, 
  pars=c(
    "Ind_Titer_Level[8]", 
    "Ind_Titer_Level[133]", 
    "Ind_Titer_Level[188]", 
    "Ind_Titer_Level[571]",
    "Ind_Titer_Level[647]",
    "Ind_Titer_Level[136]",
    "Ind_Titer_Level[157]",
    "Ind_Titer_Level[285]"
  )
)


# pair plots
pairs(fit, 
      pars=c(
        #"Mu_Uninf",
        #"Sigma_Uninf", 
        "Sigma_Random", 
        "Mu_Inf", 
        "Sigma_Inf",
        "x0",
        "kpar",
        "ProbInf4",
        "ProbInf1"
      )
)


# calculate WAIC/WBIC for predictive performance/model selection
#LL = extract_log_lik(fit, parameter_name = 'log_lik')
#waic(LL)
loo_output = loo(fit, cores = 10, is_method = "psis")
loo_output
warnings()
# options(max.print=100000)
plot(loo_output$pointwise[, "elpd_loo"])
plot(loo_output$pointwise[, "mcse_elpd_loo"])
plot(loo_output$pointwise[,])
plot(loo_output)
pareto_k_table(loo_output)
pareto_k_ids(loo_output, threshold = 1.0)
pareto_k_values(loo_output)
psis_n_eff_values(loo_output)
mcse_loo(loo_output, threshold = 0.5)


# MAP estimates 
posmax = as.numeric(which(params$lp__ == max(params$lp__)))
MAP = as.data.frame(fit)[posmax, c("Mu_Uninf",
                                   "Sigma_Uninf", 
                                   "Mu_Inf", 
                                   "Sigma_Inf", 
                                   "Sigma_Random", 
                                   "ProbInf4",
                                   "ProbInf1")]


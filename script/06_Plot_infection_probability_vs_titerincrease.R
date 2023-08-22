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
# Plot infection probability vs titer increase
#
################################################################################

if(!exists("fit") & file.exists("results/fit.rds")) {
  message("Loading fitted results")
  fit <- readRDS("results/fit.rds")
} else if(!exists("fit") & !file.exists("results/fit.rds")) {
  warning("No fitted results found")
}

# Extract parameters from stan fit
params <- rstan::extract(fit, pars = c("prob_inf_pred", "Mu_Inf", "Sigma_Inf", "Mu_Uninf", "Sigma_Uninf", "ProbInf4", "ProbInf1"))

# logistic function for probability of infection
probinfection = function(x, P4, P0){
  k = 0.5 * ((log((1 / P0) - 1)) - (log((1 / P4) - 1)))
  x0 = -2 * ((log((1 / P0) - 1)) / ((log((1 / P4) - 1)) - (log((1 / P0) - 1)))) 
  return(1 / (1 + exp((-k) * (x - x0))))
} 

data_titerpinf <- mumps_data |> 
  mutate(diff = Post - Pre,
         pinf_med = apply(params$prob_inf_pred, MARGIN = 2, quantile, probs = 0.5),
         pinf_low = apply(params$prob_inf_pred, MARGIN = 2, quantile, probs = 0.025),
         pinf_high = apply(params$prob_inf_pred, MARGIN = 2, quantile, probs = 0.975),
         pinf_cat = cut(pinf_med, breaks = c(0, 0.1, 0.9, 1), 
                        labels = c("< 10%", "10% - 90%", "≥ 90%"), right = FALSE, include.lowest = TRUE),
         diff = if_else(diff > 3, 3.2, if_else(diff < -1, -1.2, diff)),
         pinf_med = if_else(diff > 3, 1, if_else(diff < -1, 0, pinf_med)),
         pinf_low = if_else(diff > 3, 1, if_else(diff < -1, 0, pinf_low)),
         pinf_high = if_else(diff > 3, 1, if_else(diff < -1, 0, pinf_high))) |> 
  group_by(diff, pinf_med, pinf_low, pinf_high, pinf_cat) |> 
  count()


data_logistic_lines <- expand_grid(x = seq(-1, 3, by = .01),
                          i = round(seq(1, length(params$ProbInf4), length.out = 100))) |> 
  mutate(y = probinfection(x, P4 = params$ProbInf4[i], P0 = params$ProbInf1[i]))



ggplot() +
  geom_line(data = data_logistic_lines,
            aes(x = x, y = y, group = i),
            col =  rgb(0.5, 0.5, 0.5),
            alpha = 0.5) +
  geom_pointrange(data = data_titerpinf,
             aes(x = diff, y = pinf_med, ymin = pinf_low, ymax = pinf_high, size = n, col = pinf_cat),
             inherit.aes = FALSE,
             #col = "#6948AA",
             alpha = 0.6) +
  scale_size_area(max_size = 3) +
  scale_x_continuous(expand = expansion(c(0.05, 0.05)),
                     breaks = c(-1.2, -1:3, 3.2),
                     labels = c("<-1", as.character(-1:3), "≥3"),
                     minor_breaks = seq(-1, 3, 0.5)) +
  scale_y_continuous(limits = c(-0.05, 1.05),
                     expand = expansion(c(0, 0))) +
  scale_color_manual(values = c( "steelblue3", "#6948AA", "firebrick")) +
  labs(x = "Post-titer - Pre-titer",
       y = "Infection probability",
       color = "Median\ninfection\nprobability") +
  theme_light() +
  theme(legend.position = c(0.99, 0.5),
        legend.justification = c(1, 0.5),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 13)) +
  guides(size = "none")


ggsave("./figures/Fig_infection_probability_vs_titer_increase.png", dpi = 300, height = 5, width = 7, bg = "white")

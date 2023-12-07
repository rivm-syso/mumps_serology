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
# Plot pre-titer vs infection probability
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

data_probinf_grouped <- mumps_data %>% 
  mutate(pinf_med = apply(params$prob_inf_pred, MARGIN = 2, quantile, probs = 0.5),
         pinf_cat = cut(pinf_med, breaks = c(0, 0.1, 0.9, 1), labels = c("< 10%", "10% - 90%", "≥ 90%"), right = FALSE, include.lowest = TRUE))

stat_box_data <- function(y) {
  return( 
    data.frame(
      y = 15,  
      label = paste('n =', length(y), '\n')
    )
  )
}

ggplot(data = data_probinf_grouped,
       mapping = aes(x = pinf_cat, y = Pre, fill = pinf_cat)) +
  geom_boxplot(alpha = 0.8) +
  stat_summary(
    fun.data = stat_box_data, 
    geom = "text",
    alpha = 0.5,
    size = 6,
    hjust = 0.5,
    vjust = 1.0
  ) +
  geom_quasirandom(alpha = 0.2, size = 1.5, color = "black", stroke = 0) +
  scale_y_continuous(limits = c(2, 15), breaks = seq(2, 16, 2)) +
  scale_fill_manual(values = c("steelblue3", "#6948AA",  "firebrick")) +
  labs(x = "Infection probability",
       y = "Pre-titer") + 
  theme_light() +
  theme(legend.position = "none",
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 13))

ggsave(paste0("./figures/Fig_pretiter_vs_infection_probability.png"), dpi = 300, height = 5, width = 7, bg = "white")

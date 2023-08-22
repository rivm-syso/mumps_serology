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
# Plot infection probability vs pre-titer
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


data_probinf <- mumps_data %>% 
  mutate(pinf_med = apply(params$prob_inf_pred, MARGIN = 2, quantile, probs = 0.5),
         pinf_low = apply(params$prob_inf_pred, MARGIN = 2, quantile, probs = 0.025),
         pinf_high = apply(params$prob_inf_pred, MARGIN = 2, quantile, probs = 0.975),
         Pre_group   = cut_number(Pre, n = 4),
         pinf_cat = cut(pinf_med, breaks = c(0, 0.1, 0.9, 1), labels = c("< 10%", "10% - 90%", "≥ 90%"), right = FALSE, include.lowest = TRUE)) 
           

probinf_group <- tibble()
for(group in levels(data_probinf$Pre_group)) {
  sel <- data_probinf$Pre_group == group
  tmp <- quantile(apply(params$prob_inf_pred[,sel], MARGIN = 1, mean), probs = c(0.025, 0.5, 0.975))
  range <- gsub(x = group, pattern = "\\(|\\]|\\[", replacement = "")
  range <- str_split_1(range, pattern = ",")
  probinf_group <- bind_rows(probinf_group,
                             tibble(titer_start = as.numeric(range[1]),
                                    titer_end =  as.numeric(range[2]),
                                    pinf_med = tmp[2],
                                    pinf_low = tmp[1],
                                    pinf_high = tmp[3]))
}

probinf_group <- probinf_group %>% 
  pivot_longer(cols = starts_with("titer"), values_to = "titer")

# median probability of infection vs pre titer
ggplot(data = data_probinf,
       mapping = aes(x = Pre, y = pinf_med, ymin = pinf_low, ymax = pinf_high, color = pinf_cat)) +
  geom_linerange(linewidth = 0.8, 
                 alpha = 0.4) +
  geom_point(size = 1.5,
             alpha = 0.5) + 
  geom_ribbon(data = probinf_group ,
            aes(x = titer, ymin = pinf_low, ymax = pinf_high),
            inherit.aes = FALSE,
            col = NA,
            fill = rgb(0.3, 0.3, 0.3),
            alpha = 0.3) +
  geom_line(data = probinf_group,
            aes(x = titer, y = pinf_med),
            inherit.aes = FALSE,
            col = rgb(0.3, 0.3, 0.3)) +
  scale_x_continuous(breaks = seq(4, 12, 2)) +
  scale_y_continuous(limits = c(-0.01, 1.01),
                     expand = expansion(c(0, 0))) +
  scale_colour_manual(values = c( "steelblue3", "#6948AA",  "firebrick")) +
  labs(x = "Pre-titer",
       y = "Infection probability",
       color = "Median\ninfection\nprobability") + 
  theme_light() +
  theme(legend.position = c(0.99, 0.5),
        legend.justification = c(1, 0.5),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 13))
  
  
ggsave(paste0("./figures/Fig_infection_probability_vs_pretiter.png"), dpi = 300, height = 5, width = 7, bg = "white")

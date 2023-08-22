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
# Plot different components
# - pre-titer: only uninfected component
# - post-titer: mixture of infected and uninfected component
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


# mean of median infection probability per student
frac_infected <- mean(apply(params$prob_inf_pred, MARGIN = 2, median))

data_titer <- mumps_data |> 
  pivot_longer(cols = c("Pre", "Post"), names_to = "type", values_to = "titer") |>
  mutate(type = factor(type, levels = c("Pre", "Post")),
         titer_group = factor(cut(titer, breaks = seq(0.1, 15, 0.2), include.lowest = TRUE, right = FALSE, labels = seq(0.1, 14.85, 0.2) + 0.1 )),
         titer_num = as.numeric(as.character(titer_group)))


data_components <- bind_rows(
  "infected_post" = tibble(x = seq(3, 17, by = .1)) |> 
    rowwise() |> 
    mutate(y_low = quantile(frac_infected*dnorm(x, mean = params$Mu_Inf, sd = params$Sigma_Inf), probs = 0.025),
           y_med = quantile(frac_infected*dnorm(x, mean = params$Mu_Inf, sd = params$Sigma_Inf), probs = 0.5),
           y_high = quantile(frac_infected*dnorm(x, mean = params$Mu_Inf, sd = params$Sigma_Inf), probs = 0.975)),
  "uninfected_post" = tibble(x = seq(3, 17, by = .1)) |> 
    rowwise() |> 
    mutate(y_low = quantile((1-frac_infected)*dnorm(x, mean = params$Mu_Uninf, sd = params$Sigma_Uninf), probs = 0.025),
           y_med = quantile((1-frac_infected)*dnorm(x, mean = params$Mu_Uninf, sd = params$Sigma_Uninf), probs = 0.5),
           y_high = quantile((1-frac_infected)*dnorm(x, mean = params$Mu_Uninf, sd = params$Sigma_Uninf), probs = 0.975)),
  "uninfected_pre" = tibble(x = seq(3, 17, by = .1)) |> 
    rowwise() |> 
    mutate(y_low = quantile(dnorm(x, mean = params$Mu_Uninf, sd = params$Sigma_Uninf), probs = 0.025),
           y_med = quantile(dnorm(x, mean = params$Mu_Uninf, sd = params$Sigma_Uninf), probs = 0.5),
           y_high = quantile(dnorm(x, mean = params$Mu_Uninf, sd = params$Sigma_Uninf), probs = 0.975)),
  "all" = tibble(x = seq(3, 17, by = .1)) |> 
    rowwise() |> 
    mutate(y_low = quantile(frac_infected*dnorm(x, mean = params$Mu_Inf, sd = params$Sigma_Inf) + (1-frac_infected)*dnorm(x, mean = params$Mu_Uninf, sd = params$Sigma_Uninf), probs = 0.025),
           y_med = quantile(frac_infected*dnorm(x, mean = params$Mu_Inf, sd = params$Sigma_Inf) + (1-frac_infected)*dnorm(x, mean = params$Mu_Uninf, sd = params$Sigma_Uninf), probs = 0.5),
           y_high = quantile(frac_infected*dnorm(x, mean = params$Mu_Inf, sd = params$Sigma_Inf) + (1-frac_infected)*dnorm(x, mean = params$Mu_Uninf, sd = params$Sigma_Uninf), probs = 0.975)),
  .id = "type"
)


plot_posterior_pre <- data_titer |> 
  filter(type == "Pre") |> 
  group_by(titer_num) |> 
  count() |> 
  ungroup() |> 
  mutate(n = n/sum(n)/0.2) |> 
  ggplot(aes(x = titer_num, y = n)) +
  geom_bar(stat = "identity",
           col = NA,
           fill = "steelblue3") +
  geom_ribbon(data = data_components |> filter(type == "uninfected_pre"),
              aes(x = x, ymin = y_low, ymax = y_high),
              col = NA,
              fill = "dodgerblue4",
              alpha = 0.4,
              inherit.aes = FALSE) +
  geom_line(data = data_components |> filter(type == "uninfected_pre"),
            aes(x = x, y = y_med),
            col = "dodgerblue4",
            alpha = 1,
            inherit.aes = FALSE) +
  scale_x_continuous(limits = c(3, 15),
                     breaks = seq(4, 14, 2),
                     expand = expansion(c(0, 0))) +
  scale_y_continuous(limits = c(0, 0.38),
                     expand = expansion(c(0, 0))) +
  labs(x = "Pre-titer",
       y = NULL) +
  theme_light() +
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 13)) +
  guides(color = "none") 


plot_posterior_post <- data_titer |> 
  filter(type == "Post") |> 
  mutate(probinf = apply(params$prob_inf_pred, MARGIN = 2, median),
         probinf_cat = cut(probinf, breaks = c(0, 0.1, 0.9, 1), labels = c("< 10%", "10% - 90%", "≥ 90%"), right = FALSE, include.lowest = TRUE)) |> 
  group_by(titer_num, probinf_cat) |> 
  count() |> 
  ungroup() |> 
  mutate(n = n/sum(n)/0.2) |> 
  ggplot(aes(x = titer_num, y = n, fill = probinf_cat)) +
  geom_bar(stat = "identity",
           col = NA) +
  geom_ribbon(data = data_components |> filter(type == "all"),
              aes(x = x, ymin = y_low, ymax = y_high),
              col = NA,
              fill = rgb(0.2, 0.2, 0.2),
              alpha = 0.4,
              inherit.aes = FALSE) +
  geom_line(data = data_components |> filter(type == "all"),
            aes(x = x, y = y_med),
            col = rgb(0.2, 0.2, 0.2),
            alpha = 1,
            inherit.aes = FALSE) +
  geom_ribbon(data = data_components |> filter(type == "infected_post"),
              aes(x = x, ymin = y_low, ymax = y_high),
              col = NA,
              fill = "darkred",
              alpha = 0.4,
              inherit.aes = FALSE) +
  geom_line(data = data_components |> filter(type == "infected_post"),
            aes(x = x, y = y_med),
            col = "darkred",
            alpha = 1,
            inherit.aes = FALSE) +
  scale_x_continuous(limits = c(3, 15),
                     breaks = seq(4, 14, 2),
                     expand = expansion(c(0, 0))) +
  scale_y_continuous(limits = c(0, 0.38),
                     expand = expansion(c(0, 0))) +
  scale_fill_manual(values = c( "steelblue3", "#6948AA", "firebrick")) +
  labs(x = "Post-titer",
       y = NULL,
       fill = "Infection\nprobability") +
  theme_light() +
  theme(legend.position = c(0.9, 0.9),
        legend.justification = c(1, 1),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 13)) +
  guides(color = "none") 


plot_grid(plot_posterior_pre, plot_posterior_post,
          nrow = 2,
          align = "v",
          label_size = 14,
          hjust = c(-2,-1.5),
          vjust = 3)

ggsave("./figures/Fig_components.png", dpi = 300, height = 7, width = 7, bg = "white")


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
# Plot data
# - show pre- and post-titer often similar
# - four-fold increase and posttiter > 300 infected according to Gouma et al.
# - four-fold decrease will be removed from data (n = 16)
#
################################################################################


plot_points <- mumps_data_all |> 
  mutate(infected = (Post - Pre >= 2 & Post > log(300, base = 2)),
         toberemoved = Pre - Post >= 2) |> 
  ggplot(aes(x = Pre, y = Post, col = infected, alpha = toberemoved)) +
  geom_abline(slope = 1, intercept =-2:2, col = rgb(0.5, 0.5, 0.5)) +
  geom_point() +
  coord_equal() +
  scale_x_continuous(limits = c(3, 15),
                     breaks = seq(4, 14, 2)) +
  scale_y_continuous(limits = c(3, 15),
                     breaks = seq(4, 14, 2)) +
  scale_color_manual(values = c("steelblue3", "firebrick")) +
  scale_alpha_manual(values = c(0.7, 0.3)) +
  labs(x = "Pre-titer",
       y = "Post-titer") + 
  guides(color = "none",
         alpha = "none") +
  theme_light()


p <- ggMarginal(plot_points, 
           type="histogram",
           col = adjustcolor(rgb(0.5, 0.5, 0.5), 0.2),
           fill = adjustcolor(rgb(0.5, 0.5, 0.5), 0.5))

ggsave(p, filename = "./figures/Fig_data.png", dpi = 300, height = 4.5, width = 7, bg = "white")



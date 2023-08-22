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
# Load R packages and set options
# 
################################################################################


library(tidyverse)
library(dplyr)
library(lubridate)
library(grid)
library(rstan)
library(loo)
library(cowplot)
library(ggExtra)
rstan_options(auto_write = TRUE)
options(mc.cores = 10)



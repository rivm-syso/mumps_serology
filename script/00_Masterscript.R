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
# Masterscript for analysis of paired serology samples
# 
################################################################################


source("./script/01_Load_packages.R")
source("./script/02_Load_data.R")
source("./script/03_Plot_data.R")
source("./script/04_Analyze_paired_samples.R")
source("./script/05_Plot_components.R")
source("./script/06_Plot_infection_probability_vs_titerincrease.R")
source("./script/07_Plot_infection_probability_vs_pretiter.R")

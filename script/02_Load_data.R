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
# Get data for mumps analysis
# 
################################################################################

raw_data_filename <- "./data/bof_cutoff_20180921_2.csv"
data_filename <- "./data/mumps_serology_data.csv"

if(file.exists(raw_data_filename) & !file.exists(data_filename)) {
  
  # import data
  mumps_data <- read_csv(raw_data_filename)
  
  # select variables and transform data for analysis
  mumps_data <- mumps_data |> 
    transmute(ID = ID,
              Pre = log(IgG_JL_pre, base = 2),
              Post = log(IgG_JL_post, base = 2)
    )
  
  mumps_data |> write_csv(file = data_filename)
  
  message("Raw data loaded, cleaned and stored")
  
} else if(file.exists(data_filename)) {
  
  mumps_data <- read_csv(data_filename)
  message("Data loaded")
  
} else {
  
  message("No data found")
}

rm(raw_data_filename)
rm(data_filename)
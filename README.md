# Mumps serology

This repository contains the data and code used for the analysis of paired mumps serology samples, as described in ["Estimation of the infection attack rate of mumps in an outbreak among college students using paired serology"](https://www.medrxiv.org/content/10.1101/2023.09.12.23295419v1), by [Michiel van Boven](mailto:michiel.van.boven@rivm.nl), Jantien A. Backer, Irene Veldhuijzen, Justin Gomme, Rob van Binnendijk, and Patricia Kaaijk, consisting of:

* `script`: code for analysis and figures

* `data`: data of 746 persons with paired serology samples

All code has been written in the programming language [R](https://www.r-project.org/about.html); see [Requirements](#requirements) for detailed specification.

## Data

The data comprises:

* ID: identification number of person

* Pre: pre-outbreak IgG antibody concentration (log2-transformed)

* Post: post-outbreak IgG antibody concentration (log2-transformed)

The raw data is not publicly available due to GDPR constraints.


## Usage

`script` contains the code for analysis and figures, consisting of several numbered R files that reproduce the results when executed in order. Note that intermediate results and figures are not part of this repository, but can be reproduced.


## <a name = "requirements"></a> Requirements

The code has been developed and runs under the RIVM R-Studio servers.

```
R version 4.3.1 (2023-06-16) Beagle Scouts
Platform: x86_64-redhat-linux-gnu (64-bit)
Running under: Red Hat Cloud Infrastructure
```

Next to the R base packages, the following packages(_versions) were used

```
 loo_2.6.0
 rstan_2.21.8
 StanHeaders_2.21.0-7
 ggExtra_0.10.1  
 cowplot_1.1.1   
 lubridate_1.9.2 
 forcats_1.0.0   
 stringr_1.5.0   
 dplyr_1.1.2     
 purrr_1.0.2     
 readr_2.1.4    
 tidyr_1.3.0     
 tibble_3.2.1    
 ggplot2_3.4.3   
 tidyverse_2.0.0
```

## License

This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.

## Feedback

If you encounter a clear bug, please file an issue with a minimal reproducible example on GitHub.



# covidstats

<!-- badges: start -->
<!-- badges: end -->


## Overview
covidstats is to COVID-19 pandemic data extraction tool, providing RMS location
specific COVID-19 data extraction functions. Included functions are:
- get_country_counts() retrieves daily country case and death counts
- get_country_vax() retrieves daily country vaccination rates
- get_us_counts() retrieves US states daily case and death counts
- get_us_msa() retrieves US Metropolitan Statistical Area delineation data
- get_us_vax() retrieves US states daily vaccination rates
- get_us_vx_demog() retrieves US state daily vaccination rates by age group
- get_zurich_counts() retrieves Zurich canton daily case and death counts
- get_london_counts() retrieves London daily case and death counts
- get_london_vx_demog() retrieves London daily vaccination rates by age group



## Installation

You can install the development version of covidstats from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("bjabo-rms/covidstats")
```

## Usage

This is example show the retrival of daily COVID-19 cases and deaths for Switzerland

``` r
library(covidstats)
get_country_counts("CHE")
```

## Getting help

If you encounter a bug, please file an issue with a minimal reproducible example on [GitHub](https://github.com/bjabo-rms/covidstats/issues). 

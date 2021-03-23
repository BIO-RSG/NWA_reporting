# Stephanie.Clay@dfo-mpo.gc.ca
# Mar 2021

# Input: Phytofit bloom metrics csv file

# This calculates means and anomalies and formats it to create scorecards and time
# series plots using 02_Make_Figures.R

rm(list=ls())

library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(grid)
library(patchwork)
source("R/custom_functions.R")

# interval (temporal binning) and sensor used for the fits, for the output filename
interval <- "daily"
sensor <- "modis"

# range of years to process
first_year <- 2003
last_year <- 2020

# range of years to use for reference when computing climatology mean, standard deviation, and anomalies
first_ref_year <- 2003
last_ref_year <- 2010

region_str <- c("GS", "CLS", "LAS")

input_path <- "AZOMP/01_raw_data/"
input_file <- "modis_atlantic_poly4_2003-2021_daily_loggedChla_Gaussian_created_2021-03-18-181022_fulltimeseries/bloom_fit_params.csv"

output_path <- "AZOMP/02_processed_data/"
output_file <- paste0(output_path,"Bloom_parameters_processed_",sensor,"_",interval,"_refyears",first_ref_year,"-",last_ref_year,"_timeseries",first_year,"-",last_year,".RData")


#*******************************************************************************
# READ DATA, FORMAT/REARRANGE

if (first_ref_year < first_year | last_ref_year > last_year) {stop("Reference years beyond range of selected years")}

# load and format the data from the input file
df_data <- read.csv(paste0(input_path, input_file)) %>%
    # dplyr::rename(region=Region, year=Year, start=t.start., amplitude=Amplitude, duration=t.duration., magnitude=Magnitude) %>%
    dplyr::rename(region=Region, year=Year, start=t.start., amplitude=Amplitude.real., duration=t.duration., magnitude=Magnitude.real.) %>%
    dplyr::select(region, year, start, amplitude, duration, magnitude) %>%
    dplyr::filter(between(year, first_year, last_year) & region %in% region_str) %>%
    dplyr::arrange(region, year)

# make sure the input data contains all the selected years for all the selected regions
reg_match <- as.character(df_data$region) == rep(region_str[order(region_str)], each=length(first_year:last_year))
year_match <- as.numeric(as.character(df_data$year)) == rep(first_year:last_year, length(region_str))
if (!(all(reg_match) & all(year_match))) {
    error("Input data missing regions or years")
}

# rearrange data frame
df_data <- df_data %>% tidyr::gather(variable, value, 3:6)


#*******************************************************************************
# COMPUTE STATS, CLIMATOLOGY, ANOMALIES

df_means_annual <- df_data %>%
    dplyr::mutate(annual_mean=value, annual_sd=0) %>%
    dplyr::select(variable, region, year, annual_mean, annual_sd)

# annual climatology
df_climatology_annual <- annual_climatologies(df_means_annual, first_ref_year, last_ref_year)

# annual anomalies
df_anomaly_annual <- annual_anomalies(df_means_annual, df_climatology_annual)


#*******************************************************************************
# SAVE TO OUTPUT RDATA FILES

save(file=output_file, list=c("df_data", "df_means_annual", "df_climatology_annual", "df_anomaly_annual"))


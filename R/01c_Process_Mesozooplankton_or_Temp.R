# Stephanie.Clay@dfo-mpo.gc.ca
# Mar 2021

# Input: csv files containing data that has already been through some processing (e.g. temperature and mesozooplankton)
# They should contain the following columns (region = the box, e.g. LAS, CLS, GS...): 
    # variable
    # region
    # year
    # annual_mean
    # annual_sd

# This script calculates climatologies and anomalies and formats it to create scorecards and time
# series plots using 02_Make_Figures.R

rm(list=ls())

library(dplyr)
library(tidyr)
source("R/custom_functions.R")

# range of years to process
first_year <- 1995
last_year <- 2020

# range of years to use for reference when computing climatology mean, standard
# deviation, and anomalies
first_ref_year <- 1999
last_ref_year <- 2010

# location of raw data and file
input_path <- "AZOMP/01_raw_data/"
input_file <- "Temperature_AVG_0_100.csv"

# location of processed data
output_path <- "AZOMP/02_processed_data/"

variable <- "temperature"

output_file <- paste0("AZOMP_", variable,
                      "_refyears", first_ref_year, "-", last_ref_year,
                      "_timeseries", first_year, "-", last_year, "_processed.RData")


#*******************************************************************************
# LOAD, FORMAT, AND PROCESS DATA

df_means_annual <- read.csv(paste0(input_path, input_file)) %>%
    dplyr::mutate(variable=trimws(as.character(variable)),
                  region=trimws(as.character(region)),
                  year=as.numeric(as.character(year)),
                  annual_mean=as.numeric(as.character(annual_mean)),
                  annual_sd=as.numeric(as.character(annual_sd))) %>%
    dplyr::select(variable, region, year, annual_mean, annual_sd)

# annual climatology
df_climatology_annual <- annual_climatologies(df_means_annual, first_ref_year, last_ref_year)

# annual anomalies
df_anomaly_annual <- annual_anomalies(df_means_annual, df_climatology_annual)


#*******************************************************************************
# SAVE DATAFRAMES TO RDATA FILES

save(file=paste0(output_path, output_file),
     list=c("df_means_annual",
            "df_climatology_annual",
            "df_anomaly_annual"))


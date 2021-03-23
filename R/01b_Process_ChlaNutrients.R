# Stephanie.Clay@dfo-mpo.gc.ca
# Mar 2021

# Input: Data extracted from BioCHEM using the scripts in the extractions/azomp subfolder
# of the BioCHEM_extractions repository.

# This calculates climatologies and anomalies and formats it to create scorecards and time
# series plots using 02_Make_Figures.R

rm(list=ls())

library(lubridate)
library(dplyr)
library(tidyr)
library(lsmeans)
library(sp)
library(caTools) # for integration of water column
source("R/custom_functions.R")


#*******************************************************************************
# VARIABLES TO CHANGE

# range of years to process
first_year <- 1995
last_year <- 2020

# range of years to use for reference when computing climatology mean, standard
# deviation, and anomalies
ref_years <- 1999:2010

# subset to ar7w line / boxes CLS/LAS/GS? (for data extracted from BioCHEM)
azomp_subset <- TRUE

# use value integrated over depth for climatologies and anomalies, or mean value over depth?
integrated_depth <- TRUE

# Depth ranges for profiles
depths1 <- c(0,100)
depths2 <- c(100,Inf) # for no max depth, enter Inf

# NOTE: param_names, annual_indices, and depth_layer must be in the same order
# variable names
param_names <- c("Chlorophyll_A",
                 #"Temperature",
                 rep("Nitrate",2),
                 rep("Phosphate",2),
                 rep("Silicate",2))
# variables, depths included
annual_indices <- c(paste0("Chlorophyll_A_",depths1[1],"_",depths1[2]),
                    #paste0("Temperature_",depths1[1],"_",depths1[2]),
                    paste0("Nitrate_",depths1[1],"_",depths1[2]),
                    paste0("Nitrate_",depths2[1],"_",depths2[2]),
                    paste0("Phosphate_",depths1[1],"_",depths1[2]),
                    paste0("Phosphate_",depths2[1],"_",depths2[2]),
                    paste0("Silicate_",depths1[1],"_",depths1[2]),
                    paste0("Silicate_",depths2[1],"_",depths2[2]))
# depth layer (e.g. depth layer 1 = 0-100m, depth layer 2 = 100+ m)
depth_layer <- c(1,1,2,1,2,1,2)#c(1,1,1,2,1,2,1,2)

# location of raw data
input_path <- "AZOMP/01_raw_data/"

# Rdata filenames for sample IDs and data, including the extension
sample_file <- "AZOMP_DIS_Metadata_ChlaNutrients_20210317.RData"
data_file <- "AZOMP_DIS_Data_ChlaNutrients_20210317.RData"

# location of processed data
output_path <- "AZOMP/02_processed_data/"

output_file <- paste0(output_path, "AZOMP_ChlaNutrients_",
                      "_refyears", paste0(range(ref_years), collapse="-"),
                      "_timeseries", first_year, "-", last_year, "_",
                      ifelse(integrated_depth, "integrated_profile", "avg_profile"),
                      "_processed.RData")


#*******************************************************************************
# LOAD DATA

if (ref_years[1] < first_year | ref_years[length(ref_years)] > last_year) {stop("Reference years beyond range of selected years")}

# load sample_id
load(paste0(input_path, sample_file))
df_sample <- df
rm(df, sql_str)

# load data
load(paste0(input_path, data_file))
df_data <- df
rm(df, sql_str)

# control check: only use data with sample_ID in both sample list and data list, and within the selected years
target_samples <- base::intersect(df_sample$sample_id, df_data$sample_id)
df_sample <- dplyr::filter(df_sample, sample_id %in% target_samples & between(year, first_year, last_year)) %>% dplyr::distinct()
df_data <- dplyr::filter(df_data, sample_id %in% target_samples & between(year, first_year, last_year)) %>% dplyr::distinct()

# add columns to df_sample, and assign a transect (region) name to each
df_sample <- df_sample %>%
    tidyr::unite(., yearday, year, month, day, sep="-", remove=FALSE) %>%
    dplyr::mutate(., yearday=yday(ymd(yearday)),
                  season=ifelse(month %in% c(12,1,2), "winter", ifelse(month %in% 3:5, "spring", ifelse(month %in% 6:8, "summer", "fall"))),
                  #station=as.character(collector_station_name),
                  region=ifelse(between(latitude, 55.5, 60.1) & between(longitude, -53.7, -48.8), "CLS",
                                  ifelse(between(latitude, 53.6, 55.5) & between(longitude, -55.7, -53.7), "LAS",
                                         ifelse(between(latitude, 60.1, 60.7) & between(longitude, -48.8, -48.1), "GS", NA))),
                  event_id=paste(mission_descriptor, collector_event_id, sep="_")) %>%
    dplyr::filter(!is.na(region))

# # If using station names for linear modelling to fill in points in the time
# # series with no data, fix station names here
# for (i in seq(1,30,by=0.5)) {
#     ind <- df_sample$station %in% apply(expand.grid(c("L3", "AR7W"),
#                                                     c("_", "-", " "),
#                                                     c(i, pad_num(i,2))),
#                                                  1, paste0, collapse="")
#     df_sample[ind, "station"] <- paste0("L3_", pad_num(i,2))
# }
# 
# # Write station names to a file for checking
# tmp_df <- df_sample %>%
#     dplyr::select(., region, mission_descriptor, mission_name, collector_event_id, collector_sample_id, station,
#                      year, month, day, yearday, time, latitude, longitude, start_depth) %>%
#     dplyr::distinct(., region, year, station, latitude, longitude) %>%
#     dplyr::arrange(., region, year, station, latitude, longitude)
# 
# write.csv(tmp_df, paste0(main_path, "sample_stations.csv"), row.names=FALSE)

if (azomp_subset) {
    # SUBSET DATA TO AR7W LINE, THEN FURTHER SUBSET TO THE 3 BOXES (LAS, GS, CLS)?
    df_sample <- subset_to_ar7w(df_sample)
    df_sample <- subset_to_labsea_boxes(df_sample)
    df_data <- subset_to_ar7w(df_data)
    df_data <- subset_to_labsea_boxes(df_data)
}


#*******************************************************************************
# CALCULATE AVERAGE DATA VALUE OF DIFFERENT METHODS FOR A GIVEN SAMPLE

df_data_avg_method <- df_data %>%
    dplyr::select(sample_id, parameter_name, year, mission_descriptor, data_value) %>%
    dplyr::mutate(parameter_name=gsub(" ", "_", parameter_name)) %>%
    dplyr::filter(!is.na(data_value)) %>%
    # note: include year and mission descriptor in the grouping in case sample IDs were accidentally used twice
    dplyr::group_by(sample_id, parameter_name, year, mission_descriptor) %>%
    dplyr::summarize(data_value=mean(data_value, na.rm=TRUE)) %>%
    dplyr::ungroup() %>% dplyr::left_join(.,
                                          df_sample %>% dplyr::select(sample_id, event_id, start_depth, year, season, latitude, longitude, region),
                                          by="sample_id")


# # FOR HPLC PROCESSING
# # There is an odd year out in 2007 where hplc samples were taken at 2710m,
# # everything else is in relatively shallow water (<100m)
# df_data_avg_method <- df_data_avg_method %>% dplyr::filter(start_depth < 100)
# annual_indices <- sort(unique(df_data_avg_method$parameter_name))
# param_names <- annual_indices
# depth_layer <- rep(1,length(annual_indices))


#*******************************************************************************
# CALCULATE AVERAGE AND INTEGRATED VALUES IN THE WATER COLUMN FOR A GIVEN PARAMETER AND DEPTH LAYER (e.g. 0-100m or >100m)

# The values for each event_id are integrated over depth for AZMP (they typically have shallower depths).
# For AZOMP (Lab Sea), try both integrating and averaging.
df_data_depth_summary <- data.frame()

# Loop through and integrate (or average) over the water column for each variable
if (integrated_depth) {
    
    # to get nominal depth for integration
    data("nwa_bath_4km", package="oceancolouR")
    data("nwa_lats_4km", package="oceancolouR")
    data("nwa_lons_4km", package="oceancolouR")
    for (i in 1:length(annual_indices)) {
        if (depth_layer[i]==1) {tmp_depths <- depths1
        } else if (depth_layer[i]==2) {tmp_depths <- depths2}
        if (is.infinite(tmp_depths[2])) {tmp_depths[2] <- 10000}
        df_data_depth_summary <- dplyr::bind_rows(df_data_depth_summary,
                                                  df_data_avg_method %>%
                                                      dplyr::filter(parameter_name==param_names[i], start_depth >= tmp_depths[1], start_depth < tmp_depths[2]) %>%
                                                      dplyr::group_by(event_id) %>%
                                                      dplyr::summarize(nominal_depth=get_bathy(mean(longitude), mean(latitude), vlon=nwa_lons_4km, vlat=nwa_lats_4km, vvar=nwa_bath_4km),
                                                                       value=DIS_Integrate_Profile(depth=start_depth, value=data_value, nominal_depth=nominal_depth, depth_range=tmp_depths)) %>%
                                                      dplyr::ungroup() %>%
                                                      dplyr::mutate(parameter_name=annual_indices[i]))
    }
    
} else {
    
    for (i in 1:length(annual_indices)) {
        if (depth_layer[i]==1) {tmp_depths <- depths1
        } else if (depth_layer[i]==2) {tmp_depths <- depths2}
        if (is.infinite(tmp_depths[2])) {tmp_depths[2] <- 10000}
        df_data_depth_summary <- dplyr::bind_rows(df_data_depth_summary,
                                                  df_data_avg_method %>%
                                                      dplyr::filter(parameter_name==param_names[i], start_depth >= tmp_depths[1], start_depth < tmp_depths[2]) %>%
                                                      dplyr::group_by(event_id) %>%
                                                      dplyr::summarize(value=ifelse(sum(is.finite(data_value))==0, NA, mean(data_value, na.rm=TRUE))) %>%
                                                      dplyr::ungroup() %>%
                                                      dplyr::mutate(parameter_name=annual_indices[i]))
    }
    
}

df_data_depth_summary <- df_data_depth_summary %>%
    dplyr::left_join(.,
                     df_sample %>%
                         dplyr::select(event_id, region, year, month, day, season) %>%
                         #dplyr::select(event_id, station, region, year, month, day, season) %>%
                         dplyr::group_by(event_id) %>%
                         dplyr::slice(1),
                     by="event_id") %>%
    dplyr::rename(variable=parameter_name)


#*******************************************************************************
# USE A LINEAR MODEL TO FILL IN BLANK PARAMETERS/YEARS (or just take the mean across event IDs)

df_means_annual <- data.frame()

# loop through each variable
for (i_variable in annual_indices) {
    
    # loop through each region
    for (i_region in c("LAS", "CLS", "GS")) {
        
        # subset data, log-transform chla, convert categorical variables to factors (year, season, station)
        tmp_data <- df_data_depth_summary %>%
            dplyr::filter(variable==i_variable & region==i_region & between(year, first_year, last_year)) %>%
            dplyr::mutate(year=factor(year)) %>%
            #               season=factor(season),
            #               station=factor(station)) %>%
            dplyr::mutate(density=ifelse(variable==paste0("Chlorophyll_A_",depths1[1],"_",depths1[2]), log10(value+1), value))
        
        
        # # fit a linear model that can be used to fill in the blanks and adjust
        # # existing data to account for differences in year, station, season
        # #-----------------------------------------------------------------------
        #
        # # linear model fit
        # tmp_lm <- lm(as.formula(density ~ year + season), data=tmp_data)
        # 
        # # least square annual means
        # tmp_means <- summary(lsmeans(tmp_lm, "year"))
        # 
        # if (all(is.na(tmp_means$lsmean))) {
        #     # just find the mean of each year instead
        #     tmp_lm <- lm(as.formula(density ~ year), data=tmp_data)
        #     tmp_means <- summary(lsmeans(tmp_lm, "year"))
        #     # note: this is the same as computing the mean value for each year:
        #     # tmp_means <- tmp_data %>% dplyr::group_by(year) %>% summarize(value=mean(density, na.rm=TRUE))
        # }
        # 
        # # https://stackoverflow.com/questions/53384585/r-ls-means-analysis-produces-nas
        # # TROUBLESHOOTING NOTE:
        # # if all the estimates are NA, it could mean there are too few datapoints
        # # to give estimates for each of the factors in the model above
        # # try removing a factor with many levels (for example, station, which
        # # could have a lot of extra values from random sampling)
        # # 
        # # OR... use normal means with existing data, not filling in the blanks
        # # or accounting for differences between year/season/station
        # tmp_means <- tmp_data %>%
        #             dplyr::group_by(year) %>% #
        # 		      dplyr::mutate(lsmean=mean(density, na.rm=TRUE)) %>%
        #             dplyr::ungroup() %>%
        #             dplyr::select(year, season, lsmean) %>%
        #             dplyr::distinct(year, season, lsmean, .keep_all=TRUE)
        #
        # # add to data frame
        # df_means_annual <- dplyr::bind_rows(df_means_annual,
        #                                     tmp_means %>% 
        #                                           dplyr::mutate(year=as.numeric(as.vector(year))) %>%
        #                                           dplyr::select(year, lsmean) %>%
        #                                           dplyr::mutate(variable=i_variable, region=i_region) %>%
        #                                           dplyr::rename(value=lsmean))
        # 
        # #-----------------------------------------------------------------------
        
        
        # 22jan2020: or just take the mean of each year, instead of accounting for
        # differences in seasons and stations, because there are too few data
        df_means_annual <- dplyr::bind_rows(df_means_annual,
                                            tmp_data %>%
                                                  dplyr::mutate(year=as.numeric(as.vector(year))) %>%
                                                  dplyr::group_by(year) %>%
                                                  dplyr::summarize(annual_mean=mean(density, na.rm=TRUE),
                                                                   annual_sd=sd(density, na.rm=TRUE)) %>%
                                                  dplyr::ungroup() %>%
                                                  dplyr::mutate(variable=i_variable,
                                                                region=i_region))
        
    }
}

# reshape
df_data_depth_summary <- df_data_depth_summary %>%
    tidyr::spread(variable, value) %>%
    dplyr::arrange(desc(season), region, year)

df_climatology_annual <- annual_climatologies(df_means_annual, ref_years = ref_years)
df_anomaly_annual <- annual_anomalies(df_means_annual, df_climatology_annual) %>% dplyr::filter(!is.na(anom_value))

#*******************************************************************************
# SAVE DATAFRAMES TO RDATA FILE

save(file=output_file,
     list=c("df_sample",
            "df_data",
            "df_data_avg_method",
            "df_data_depth_summary",
            "df_means_annual",
            "df_climatology_annual",
            "df_anomaly_annual"))

# Create time series plots and scorecards for:
        # Bloom parameters
        # Chla
        # Nutrients
        # HPLC
        # Mesozooplankton
        # Temperature

rm(list=ls())

library(tidyr)
library(dplyr)
library(ggplot2)
library(grid)
library(patchwork)
source("R/custom_functions.R")

# range of years to process
first_year <- 2003
last_year <- 2020

# range of years to use for reference when computing climatology mean, standard
# deviation, and anomalies (must match those used in processing)
ref_years <- 2003:2010
# Note that ref_years are not used in the scorecards, only in the time series plots, and in the TS plots you must have all ref years (including late sampling years) so that it can calculate the mean value for the full ref period, the ref period with only early years, and the ref period for only late years (so for this script, do NOT remove ref years based on the late_sampling vector)

input_path <- "AZOMP/02_processed_data/"

input_file <- "Bloom_parameters_processed_modis_daily_refyears2003-2010_timeseries2003-2020.RData"

# location of figures
output_path <- "AZOMP/03_figures/"

# regions - abbreviations, full names, and colors used in the plots
# THESE NEED TO BE IN THE ORDER YOU WANT THEM TO PLOT, starting at the bottom
region_str <- c("GS", "CLS", "LAS")
region_lbl <- c("Greenland Shelf", "Central Labrador Sea", "Labrador Shelf")
region_col <- c("darkgreen", "red", "blue")

# type of input data
# options: bloom, nutrients_shallow, nutrients_deep, chla, hplc, calanus, pseudo, euphausiids, temperature
data_type <- "bloom"

# cutoff day of year between "early" and "late" sampling
cutoff <- 170
# late sampling years will have open circles in the time series plots and greyed out anomaly boxes in the scorecards
late_sampling <- read.csv("AZOMP/01_raw_data/Cruise_bloom_date.csv") %>% dplyr::filter(AR7W_start_doy >= cutoff)
late_sampling <- as.numeric(unique(unlist(late_sampling$year)))

# Depth ranges used in profiles (must be the same as used in processing)
depths1 <- c(0,100)
depths2 <- c(100,Inf) # for no max depth, enter Inf


# VARIABLES CONTROLLING THE FORMATTING AND LABELLING OF THE FIGURES, DEPENDING ON DATA_TYPE
#
# variable_str      = The variable names used in the input Rdata file (case-sensitive,
#                     including spaces or symbols/underscores)
# variable_lbl      = Corresponding labels to use in the output images
# variable_log      = Set to TRUE if variable should be logged in the time series, FALSE otherwise
#                     (if it's already been logged, set to FALSE so it won't be double-logged)
# variable_neg      = Set to TRUE if the variable can be negative, FALSE otherwise
#                     (logged values will be set to TRUE later on either way).
#                     The lower end of the error bars will not go below 0 if this is FALSE
# format_str        = This affects the format of the mean +- SD labels on the scorecards
#                     (one format for each variable)
# ytitles           = Title for the y axis of the time series plots

ytitles <- NULL

if (data_type=="bloom") {
    # BLOOM PARAMETERS
    variable_str <- c("start", "duration", "amplitude", "magnitude")
    variable_lbl <- c("Bloom initiation", "Bloom duration", "Bloom amplitude", "Bloom magnitude")
    variable_log <- c(FALSE, FALSE, FALSE, FALSE)
    variable_neg <- c(FALSE, FALSE, FALSE, FALSE)
    format_str <- c("%.0f", "%.0f", "%.1f", "%.1f")
    ytitles <- c("Day of year", "Day of year", expression(paste("OCx [",italic("chla"),"] (mg",~m^-3,")")), expression(paste("OCx [",italic("chla"),"] (mg",~m^-3,")")))

} else if (data_type=="nutrients_shallow") {
    # NUTRIENTS - note the depths must match the ones used in processing
    varnames <- c("Nitrate", "Phosphate", "Silicate")
    variable_str <- paste0(varnames, "_", depths1[1], "_", depths1[2])
    variable_lbl <- paste0(varnames, " (", depths1[1], "-", depths1[2], "m)")
    format_str <- c("%.1f", "%.1f", "%.1f")
    variable_log <- rep(FALSE, 6)
    variable_neg <- rep(FALSE, 6)
    
} else if (data_type=="nutrients_deep") {
    # NUTRIENTS - note the depths must match the ones used in processing
    varnames <- c("Nitrate", "Phosphate", "Silicate")
    variable_str <- paste0(varnames, "_", depths2[1], "_", depths2[2])
    variable_lbl <- ifelse(is.finite(rep(depths2[2], length(varnames))),
                           paste0(varnames, " (", depths2[1], "-", depths2[2], "m)"),
                           paste0(varnames, "(", depths2[1], "+ m)"))
    format_str <- c("%.1f", "%.1f", "%.1f")
    variable_log <- rep(FALSE, 6)
    variable_neg <- rep(FALSE, 6)
    
} else if (data_type=="chla") {
    # CHLA - note the depths must match the ones used in processing
    variable_str <- paste0("Chlorophyll_A_", depths1[1], "_", depths1[2])
    variable_lbl <- paste0("Chlorophyll-a (", depths1[1], "-", depths1[2], "m)")
    format_str <- c("%.1f")
    variable_log <- FALSE
    variable_neg <- FALSE
    
} else if (data_type=="hplc") {
    # HPLC
    variable_str <- c("HPLC_CHLA", "HPLC_FUCOX", "HPLC_PERID")
    variable_lbl <- c("Chlorophyll-A", "Fucoxanthin", "Peridinin")
    format_str <- c("%.2f", "%.2f", "%.2f")
    variable_log <- rep(FALSE, 3)
    variable_neg <- rep(FALSE, 3)
    
} else if (data_type=="calanus") {
    variable_str <- c("Calanus_finmarchicus", "Calanus_glacialis", "Calanus_hyperboreus")
    variable_lbl <- c("C. finmarchicus", "C. glacialis", "C. hyperboreus")
    variable_log <- c(TRUE, TRUE, TRUE)
    variable_neg <- c(TRUE, TRUE, TRUE)
    # format_str <- c("%.0e", "%.0e", "%.0e") # large numbers, scientific notation
    format_str <- c("%.1f", "%.1f", "%.1f") # for logged values, one decimal place
    
} else if (data_type=="pseudo") {
    variable_str <- c("Pseudocalanus_spp", "Oithona_spp")
    variable_lbl <- c("Pseudocalanidae", "Oithonidae")
    variable_log <- c(TRUE, TRUE)
    variable_neg <- c(TRUE, TRUE)
    # format_str <- c("%.0e", "%.0e") # large numbers, scientific notation
    format_str <- c("%.1f", "%.1f") # for logged values, one decimal place
    
} else if (data_type=="euphausiids") {
    variable_str <- c("Euphausiids", "Amphipods")
    variable_lbl <- c("Euphausiids", "Amphipods")
    variable_log <- c(TRUE, TRUE)
    variable_neg <- c(TRUE, TRUE)
    # format_str <- c("%.0e", "%.0e") # large numbers, scientific notation
    format_str <- c("%.1f", "%.1f") # for logged values, one decimal place
    
} else if (data_type=="temperature") {
    variable_str <- c("temperature")
    variable_lbl <- c("Temperature")
    variable_log <- c(FALSE, FALSE)
    variable_neg <- c(TRUE, TRUE)
    format_str <- c("%.1f", "%.1f") # one decimal place
    ytitles <- "Degrees Celsius"
    
}



#*******************************************************************************
# READ DATA

load(paste0(input_path, input_file))


# MANUAL CORRECTIONS
if (data_type != "bloom") {
    # NO BIOCHEM DATA FOR 2017 OR GS 1998 (i.e. chla, nutrients, mesozooplankton, temperature - NOT bloom parameters)
    df_anomaly_annual[df_anomaly_annual$year==2017,"anom_value"] <- NA
    df_anomaly_annual[df_anomaly_annual$year==1998 & df_anomaly_annual$region=="GS","anom_value"] <- NA
} else if (data_type == "pseudo") {
    # FOR PSEUDOCALANUS: 2003 CLS abundance = 0 so logged values are NA
    df_anomaly_annual[df_anomaly_annual$year==2003 & df_anomaly_annual$region=="CLS" & df_anomaly_annual$variable=="Pseudocalanidae","anom_value"] <- NA
}


#*******************************************************************************
# TIME SERIES PLOTS

if (data_type != "bloom") {
    
    allplots <- get_TS_plots(df = df_means_annual,
                             variable_str = variable_str,
                             variable_lbl = variable_lbl,
                             region_str = rev(region_str),
                             region_lbl = rev(region_lbl),
                             region_col = rev(region_col),
                             variable_log = variable_log,
                             variable_neg = variable_neg,
                             ytitles = ytitles,
                             late_sampling = late_sampling,
                             ref_years = ref_years)
    
    # Write time series plots to png files
    for (i in 1:length(allplots)) {
        fname <- paste0(output_path, "TimeSeries_", input_file, "_", variable_str[i], ".png")
        png(filename=fname, height=3, width=10, units="in", pointsize=2, bg="white", res=300)
        print(allplots[[i]])
        dev.off()
    }
    
}


#*******************************************************************************
# SCORECARD PLOTS

# Fill in data frame with NA's where data is missing
# THE PURPOSE OF NOT SIMPLY FILTERING OUT REGIONS/VARIABLES/YEARS YOU DON'T WANT:
# You want the entire list of possibilities, with missing data boxes blank, not simply excluded
df_anomaly <- dplyr::left_join(expand.grid(region=region_str,
                                           year=seq(first_year, last_year, by=1),
                                           variable=variable_str,
                                           stringsAsFactors=FALSE),
                               df_anomaly_annual %>% dplyr::filter(region %in% region_str),
                               by=c("region", "year", "variable"))

# exclude any years?
if (data_type!="bloom") {
    df_anomaly[df_anomaly$year %in% late_sampling, "anom_value"] <- NA
}

df_climatology <- dplyr::left_join(expand.grid(region=region_str,variable=variable_str, stringsAsFactors=FALSE),
                                   df_climatology_annual %>% dplyr::filter(region %in% region_str),
                                   by=c("region", "variable"))

slist <- get_scorecards(df_anomaly=df_anomaly,
                        df_climatology=df_climatology,
                        variable_str = variable_str,
                        variable_lbl = variable_lbl,
                        format_str = format_str,
                        region_str = region_str,
                        region_lbl = region_lbl)

gt <- slist[[1]]
legend <- slist[[2]]

# get the formatting parameters for the scorecards
sc_params <- scorecard_params(length(variable_str))
wd <- sc_params$wd
total_width <- sc_params$total_width
topy <- sc_params$topy
bottomy <- sc_params$bottomy
gaps <- sc_params$gaps
heights <- sc_params$heights
ht <- sc_params$ht
legendy <- sc_params$legendy

# output to png
fname <- paste0(output_path, "Scorecard_", data_type, "_", input_file, ".png")
png(filename=fname, height=ht, width=wd, units="px", pointsize=2, bg="white", res=300)

# main viewport
grid.newpage()
vp <- viewport(width=1, height=1)
pushViewport(vp)

# loop through viewports for each variable
for (i in 1:length(variable_str)) {
    if (i==length(variable_str)) {
        pushViewport(viewport(x=0.5, y=bottomy, width=total_width, height=(heights+0.005)))
    } else {
        pushViewport(viewport(x=0.5, y=(topy-(i-1)*gaps), width=total_width, height=heights))
    }
    grid.draw(gt[[i]])
    upViewport()
}

# legend
vp7 <- viewport(x=0.535, y=legendy, width=1, height=0.2)
pushViewport(vp7)
grid.draw(legend)
upViewport()

# close device
dev.off()

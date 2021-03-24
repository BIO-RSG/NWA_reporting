# Stephanie.Clay@dfo-mpo.gc.ca
# 2021-03-18

# Create the png plot of the bloom timing and cruise timing.
# Coded by Emmanuel Devred

rm(list=ls())

library(dplyr)

years <- 1995:2020

# region to use
reg <- "GS"

path <- "AZOMP/01_raw_data/"

fit_params <- "modis_atlantic_poly4_2003-2021_daily_loggedChla_Gaussian_created_2021-03-24-142956_fulltimeseries/bloom_fit_params.csv"
cruise_dates <- "Cruise_bloom_date.csv"

output_path <- "AZOMP/03_figures/"
output_file <- paste0("Cruise_Bloom_dates_", reg, ".png")

# cruise start day of year (if >= this day, the cruise is "late")
cutoff <- 170

#*******************************************************************************

df <- dplyr::left_join(read.csv(paste0(path, cruise_dates)) %>%
                           dplyr::filter(between(year, years[1], years[length(years)])),
                       read.csv(paste0(path, fit_params)) %>%
                           dplyr::filter(Region==reg) %>%
                           dplyr::rename(year=Year,
                                         bloom_start="t.start.",
                                         bloom_end="t.end.") %>%
                           dplyr::select(year, bloom_start, bloom_end),
                       by="year")

# get x axis limits based on cruise and bloom days, and x axis length for formatting
xlim <- c(100,260)#c(min(df %>% dplyr::select(-year), na.rm=TRUE), max(df %>% dplyr::select(-year), na.rm=TRUE))
xaxis_len <- diff(xlim)

png(paste0(output_path, output_file), height = 900, width = 600, pointsize = 24)
plot(xlim,c(-years[1],-years[length(years)]),type="n",xlab="Day of year", ylab="Year", axes = F)
axis(1)
axis(2,at=seq(-years[1],-years[length(years)],-2),las=1,labels = seq(years[1],years[length(years)],2))
# Here we draw the rectangles for cruises and satellite bloom one at a time
for (i in 1:nrow(df)) {
    # spring bloom dates
    rect(df$bloom_start[i],-1*df$year[i]-0.25,
         df$bloom_end[i],-1*df$year[i]+0.25,
         col="green4",
         density = 110,
         border=NA)  
    # cruise dates
    rect(df$AR7W_start_doy[i],-1*df$year[i]-0.45,
         df$AR7W_end_doy[i],-1*df$year[i]+0.45,
         col="blue",
         density = 85,
         border=NA)
}
abline(v=cutoff, col="red")
# legend boxes
vertical_offset <- 1.5
rect(xlim[1],-(years[1]-vertical_offset)-0.45,
     (xlim[1]+xaxis_len*0.1),-(years[1]-vertical_offset)+0.45,
     col="blue",
     density = 85,
     border=NA,xpd=NA)
rect((xlim[1]+xaxis_len*0.4),-(years[1]-vertical_offset)-0.25,
     (xlim[1]++xaxis_len*0.58),-(years[1]-vertical_offset)+0.25,
     col="green4",
     density = 110,
     border=NA,xpd=NA)
box()
vertical_offset <- 2.8
legend(xlim[1],-(years[1]-vertical_offset),c("Cruise","Spring Bloom"),xpd=NA,bty="n",horiz = T, pch = NA, col=c("blue","green4"),pt.cex = 2.2)
dev.off()



# #*******************************************************************************
# # Code to plot cutoff point for "early" vs "late" cruises
# 
# library(ggplot2)
# library(dplyr)
# 
# # cruise start day of year (if >= this day, the cruise is "late")
# cutoff <- 170
# 
# cruise_dates <- read.csv("AZOMP/01_raw_data/Cruise_bloom_date.csv") %>%
#     dplyr::mutate(year = floor(Data),
#                   timing = ifelse(AR7W_start_doy < cutoff, "early", "late")) %>%
#     dplyr::filter(!is.na(AR7W_start_doy))
# 
# p <- ggplot(cruise_dates, aes(x=year, y=AR7W_start_doy, color=timing)) +
#     geom_point() +
#     geom_hline(yintercept=cutoff, color="red") +
#     theme_minimal() +
#     ggtitle(paste("Late years:", paste0(cruise_dates[cruise_dates$timing=="late","year"], collapse=", ")))
# 
# print(p)
# 
# p <- ggplot(cruise_dates) +
#     geom_histogram(aes(x=AR7W_start_doy), color="black", fill="darkgreen", alpha=0.8) +
#     theme_minimal() +
#     theme(axis.title.y = element_blank())
# 
# # cat("Late years:", paste0(cruise_dates[cruise_dates$timing=="late","year"], collapse=", "))




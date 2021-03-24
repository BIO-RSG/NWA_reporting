# Stephanie.Clay@dfo-mpo.gc.ca
# 2021-03-18

# Make a grid of plots of the bloom fits from PhytoFit

rm(list=ls())

library(ggplot2)
library(data.table)
library(oceancolouR)
library(dplyr)
library(patchwork)
library(grid)
library(stringr)

path <- "AZOMP/01_raw_data/modis_atlantic_poly4_2003-2021_daily_loggedChla_Gaussian_created_2021-03-24-142956_fulltimeseries/"

output_path <- "AZOMP/03_figures/"

fit_params <- "bloom_fit_params.csv"

# settings that were used in the fit
regions <- c("LAS", "CLS", "GS")    # region abbreviations
years <- 2003:2020                  # vector of years
interval <- "daily"                 # temporal binning (daily or weekly)
dailystat <- "mean"                 # daily/weekly statistic
fitcov <- 20                        # required percent coverage for each day/week
t_range <- c(85,299)                # range of days used for the fit

# # flag settings
#
# flag1_lim1 <- 0.75
# flag1_lim2 <- 1.25
# flag2_lim1 <- 0.85
# flag2_lim2 <- 1.15
# ti_limits <- c(90,174)
# tm_limits <- c(110,192)
# 
# # Compare the real and fitted time series, and flag it if it meets certain criteria
# flag_check <- function(mag_real, mag_fit, amp_real, amp_fit, sigma, time_res=1,
#                        flag1_lim1, flag1_lim2, flag2_lim1, flag2_lim2,
#                        ti, ti_limits, tm, tm_limits, tt, t_range) {
#     flags <- c(amp_fit/amp_real <= flag1_lim1 | amp_fit/amp_real >= flag1_lim2,
#                mag_fit/mag_real <= flag2_lim1 | mag_fit/mag_real >= flag2_lim2,
#                sigma <= time_res,
#                ti %in% ti_limits,
#                tm %in% tm_limits,
#                tt == t_range[2])
#     return(as.numeric(paste0(which(flags), collapse="")))
# }



#*******************************************************************************

all_params <- fread(paste0(path, fit_params))

stats <- expand.grid(years, regions) %>% dplyr::arrange(Var1, Var2)

plot_list <- list()

for (i in 1:nrow(stats)) {
    
    year <- stats[i,1]
    region <- as.character(stats[i,2])
    
    params <- all_params %>% dplyr::filter(Region==region, Year==year)
    if (sum(grepl("beta", colnames(params)))==0) {params$beta <- 0}
    
    df <- fread(paste0(path, paste0(year, "_", region, "_stats.csv")))
    
    if (dailystat=="mean") {
        df$y <- df$mean_chl
    } else if (dailystat=="median") {
        df$y <- df$median_chl
    }
    
    df[df$percent_coverage <= fitcov, "y"] <- NA
    
    # plot all real data points that have sufficient percent coverage, sized by percent coverage
    p <- ggplot(df) +
        geom_point(aes(x=doy, y=y, size=percent_coverage), alpha=0.6) +
        theme_bw() +
        scale_size_continuous(name = "Percent coverage",#"Percent\ncoverage",
                              breaks = c(25, 50, 75, 100),
                              limits = c(10, 100),
                              labels = c(25, 50, 75, 100),
                              range = c(0.5, 3)) +
        theme(axis.title.y=element_blank(),
              axis.title.x=element_blank(),
              panel.border = element_rect(colour="black", fill=NA, size=0.4))
    
    
    if (is.na(params$B0)) {
        
        p <- p + annotation_custom(grobTree(textGrob("NO FIT", x=0.1, y=0.9,vjust=1,hjust=0,gp=gpar(fontsize=16, col="red"))))
        
    } else {
        
        df <- df %>% dplyr::mutate(yfit = shifted_gaussian(tv=doy, params$B0, params$beta, params$h, params$sigma, as.numeric(unlist(params[,"t[max]"]))))
        
        df[df$doy < t_range[1] | df$doy > t_range[2], "yfit"] <- NA
        
        # plot the fitted line in the range of days used in the fit (i.e. given by t_range)
        p <- p + geom_line(data=df, aes(x=doy, y=yfit), color="blue")
        
        # # print flag info on plot
        #
        # mag_real <- as.numeric(unlist(params[,"Magnitude[real]"]))
        # mag_fit <- as.numeric(unlist(params[,"Magnitude[fit]"]))
        # amp_real <- as.numeric(unlist(params[,"Amplitude[real]"]))
        # amp_fit <- as.numeric(unlist(params[,"Amplitude[fit]"]))
        # sigma <- params$sigma
        # ti <- as.numeric(unlist(params[,"t[start]"]))
        # tm <- as.numeric(unlist(params[,"t[max]"]))
        # tt <- as.numeric(unlist(params[,"t[end]"]))
        # 
        # flags <- flag_check(mag_real, mag_fit, amp_real, amp_fit, sigma,
        #                     time_res=ifelse(interval=="daily", 1, 8),
        #                     flag1_lim1, flag1_lim2, flag2_lim1, flag2_lim2,
        #                     ti, ti_limits, tm, tm_limits, tt, t_range)
        # 
        # # Add notes to the plot of problematic gaussian fits
        # new_note <- ""
        # if (grepl(1,flags)) {
        #     new_note <- paste0("Peak difference:\n",round(peak_diff,3))
        # }
        # if (grepl(2,flags)) {
        #     new_note <- paste0(new_note, "\nData_area/Curve_area:\n",round(area_data/area_fit,3))
        # }
        # if (grepl(3,flags)) {
        #     new_note <- paste0(new_note,"\nSigma:\n",round(sigma,3)," day(s)\nTime interval:\n",time_resolution," day(s)")
        # }
        # p <- p + annotation_custom(grobTree(textGrob(new_note, x=0.5, y=0.9,vjust=1,hjust=0,gp=gpar(fontsize=6, col="red"))))
        
    }
    
    # Add plot to the list of plots to write to output grid later.
    plot_list[[paste0(year,"_",region)]] <- p
    
}


# add row names
r_inds <- ((1:length(years))-1)*length(regions)+1
plot_list[r_inds] <- lapply(1:length(years), function(i) {plot_list[[(i-1)*length(regions)+1]] <- plot_list[[(i-1)*length(regions)+1]] + ylab(years[i]) +
    theme(axis.title.y=element_text(size=14,angle=90,hjust=0.5,vjust=1))})

# add column names
plot_list[1:length(regions)] <- lapply(1:length(regions), function(i) {plot_list[[i]] <- plot_list[[i]] + ggtitle(regions[i]) +
    theme(plot.title=element_text(size=14,hjust=0.5,vjust=0.5))})

# get number of rows and pages
num_rows <- length(plot_list)/length(regions)
num_pages <- 1 + floor(num_rows/5) # 5 rows per page

# get indices of plots to print per page
plot_inds_per_page <- lapply(1:num_pages, function(i) {(length(regions) * 5 * (i-1) + 1) : (length(regions) * 5 * i)})

# make a png for each page
for (i in 1:num_pages) {
    
    cat("Printing page", i, "...\n")
    
    if (num_pages==1) {
        tmp_rows <- num_rows
    } else if (i==num_pages) {
        tmp_rows <- (num_pages-1) %% num_rows
    } else {
        tmp_rows <- 5
    }
    
    inds_to_print <- plot_inds_per_page[[i]]
    inds_to_print <- inds_to_print[inds_to_print <= length(plot_list)]
    
    fname <- paste0(output_path, paste0(range(years), collapse="-") ,"_gaussian_plots_", interval, "_", str_pad(i, width=2, side="left", pad="0"), ".png")
    png(filename=fname, width=600, height=(160*tmp_rows))
    print(wrap_plots(plot_list[inds_to_print], nrow=tmp_rows) + plot_layout(guides="collect") & theme(legend.position="bottom"))
    dev.off()
    
}


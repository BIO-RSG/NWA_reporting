# Stephanie.Clay@dfo-mpo.gc.ca
# Jan 2020

# compute annual climatologies for the reference period
# input:
#       df: dataframe with columns variable, transect (region), year, annual_mean
#       first_ref_year to last_ref_year: single numeric values to compute a sequence of years to use as reference for the calculations
annual_climatologies <- function(df, first_ref_year, last_ref_year) {
    
    require(dplyr)
    df %>%
        dplyr::filter(between(year, first_ref_year, last_ref_year)) %>%
        # remove Inf
        dplyr::mutate(annual_mean=ifelse(!is.finite(annual_mean), NA, annual_mean)) %>%
        dplyr::group_by(variable, region) %>%
        dplyr::summarise(mean=mean(annual_mean, na.rm=TRUE),
                         sd=sd(annual_mean, na.rm=TRUE)) %>%#,
        #n=sum(!is.na(annual_mean)),
        #se=sd/sqrt(n)) %>%
        dplyr::ungroup() %>%
        dplyr::select(region, variable, mean, sd)
    
}

#===============================================================================

# compute annual anomalies based on climatology
# input:
#       df: dataframe with columns variable, transect (region), year, annual_mean
#       df_climatology: annual climatology of df, computed with annual_climatologies() function
annual_anomalies <- function(df, df_climatology) {
    
    require(dplyr)
    dplyr::left_join(df, df_climatology, by=c("variable", "region")) %>%
        dplyr::mutate(anom_value=(annual_mean-mean)/sd) %>%
        dplyr::select(region, year, variable, anom_value)
    
}


#===============================================================================
# CREATE TIME SERIES PLOTS AND RETURN THEM IN A LIST

get_TS_plots <- function(df, variable_str, variable_lbl, region_str, region_lbl, region_col, variable_log, variable_neg, ytitles, late_sampling = c()) {
    
    require(ggplot2)
    require(patchwork)
    
    total_plot_list <- list()
    
    xbreaks <- seq(first_year,last_year,by=1)
    xlabels <- as.vector(rbind(seq(first_year,last_year,by=2),""))
    if (length(xbreaks) != length(xlabels)) {
        xlabels <- xlabels[1:(length(xlabels)-1)]
    }
    
    for (i_variable in 1:length(variable_str)) {
        
        ytitle <- ytitles[i_variable]
        
        # get the input string and output labels for this variable, and the booleans
        # that indicate if it's logged and whether or not it can be negative
        var_str <- variable_str[i_variable]
        var_lab <- gsub("-Inf", "+ ", variable_lbl[i_variable])
        var_log <- variable_log[i_variable]
        var_neg <- variable_neg[i_variable]
        
        # format data for this variable and compute top/bottom of error bars
        var_data <- df %>%
            dplyr::filter(variable==var_str & between(year, first_year, last_year) & region %in% region_str) %>%
            dplyr::mutate(late=year %in% late_sampling,
                          top_values=(annual_mean + annual_sd),
                          bottom_tmp=(annual_mean - annual_sd)) %>%
            # if bottom of the error bars go below 0 but physically can't, set them to 0
            dplyr::mutate(bottom_values=ifelse(var_log | var_neg | bottom_tmp > 0, bottom_tmp, 0)) %>%
            dplyr::select(-bottom_tmp) %>%
            dplyr::mutate_all(~na_if(., -Inf), ~na_if(., Inf), ~na_if(., NaN))
        
        plot_list <- as.list(rep(NA,length(region_str)))
        names(plot_list) <- region_str
        
        # collect the ranges of the plots to combine them later
        p_ranges_y <- c()
        
        # loop through each transect and plot results
        for (i_transect in 1:length(region_str)) {
            
            ts_str <- region_str[i_transect]
            ts_name_str <- region_lbl[i_transect]
            ts_col_str <- region_col[i_transect]
            
            trans_data <- var_data %>% dplyr::filter(region==ts_str)
            tmp_df <- trans_data %>% dplyr::filter(between(year, first_ref_year, last_ref_year))
            tmp_mean <- mean(tmp_df$annual_mean, na.rm=TRUE)
            tmp_mean_early <- tmp_df %>% dplyr::filter(!late) %>% dplyr::select(annual_mean) %>% unlist() %>% mean(na.rm=TRUE)
            tmp_mean_late <- tmp_df %>% dplyr::filter(late) %>% dplyr::select(annual_mean) %>% unlist() %>% mean(na.rm=TRUE)
            
            p <- ggplot(trans_data) +
                geom_errorbar(aes(x=year, ymin=bottom_values, ymax=top_values),
                              colour=ts_col_str, width=0.5) +
                geom_point(aes(x=year, y=annual_mean, fill=factor(late)),
                           shape=21, colour=ts_col_str, size=3) +
                scale_fill_manual(values = c(ts_col_str, "white")) +
                geom_hline(yintercept=tmp_mean, linetype="solid", alpha=0.8) +
                geom_hline(yintercept=tmp_mean_early, linetype="longdash", color="#555555", alpha=0.8) +
                geom_hline(yintercept=tmp_mean_late, linetype="dotted", color="#555555", alpha=0.8) +
                scale_x_continuous(breaks=xbreaks, labels=xlabels) +
                ggtitle(ts_name_str) +
                labs(y=ytitle) +
                theme_classic() +
                theme(legend.position="none",
                      plot.title = element_text(hjust=0.5),
                      axis.title.x=element_blank(),
                      axis.text.x=element_text(size=12,angle=90,colour='black', vjust=0.5, hjust=1),#vjust=1, hjust=1),
                      axis.title.y=element_blank(),
                      axis.text.y=element_blank())
            
            plot_list[[ts_str]] <- p
            
            p_ranges_y <- c(p_ranges_y, ggplot_build(p)$layout$panel_scales_y[[1]]$range$range)
            
        }
        
        # distance between axis title and text
        # https://stackoverflow.com/questions/14487188/increase-distance-between-text-and-title-on-the-y-axis
        plot_list[[1]] <- plot_list[[1]] +
            theme(axis.title.y=element_text(angle=90, margin = margin(t = 0, r = 5, b = 0, l = 0)),
                  axis.text.y=element_text(size=10,colour='black'))
        
        total_plot_list[[i_variable]] <- wrap_plots(plot_list) & ylim(min(p_ranges_y), max(p_ranges_y))
        
    }
    
    return(total_plot_list)
    
}


#===============================================================================
# CREATE SCORECARDS AND RETURN THEM IN A LIST

get_scorecards <- function(df_anomaly, df_climatology, variable_str, variable_lbl, format_str, region_str, region_lbl) {
    
    require(ggplot2)
    require(grid)
    
    p <- list()
    gt <- list()
    
    ## set x-axis
    x.limits <- c((first_year - 0.5), (last_year + 0.5))
    x.breaks <- seq(x.limits[1]+.5, x.limits[2]-.5, by=1)
    # x-axis labels set within loop below
    
    ## set y-axis
    y.limits <- region_str # this will give the order of the y axis
    y.breaks <- y.limits
    y.labels <- region_lbl
    
    ## set color scheme
    BtoW <- colorRampPalette(c("blue", "white"))(4)
    W <- colorRampPalette(c("white"))(1)
    WtoR <- colorRampPalette(c("white", "red"))(4)
    color.values <- c(BtoW[1:3], W, WtoR[2:4])
    color.limits <- c(-3, 3)
    color.breaks <- seq(color.limits[1], color.limits[2], length=length(color.values))
    
    # loop over variables
    i <- 0
    for (i_variable in variable_str) {
        
        # counter increment
        i <- i+1
        
        # reduce to target transects and variables
        tmp_anomaly <- df_anomaly %>%
            dplyr::filter(variable==i_variable & region %in% region_str)
        
        # modify data for plotting purposes
        tmp_anomaly <- tmp_anomaly %>%
            dplyr::mutate(value_tmp=ifelse(anom_value>3, 3, ifelse(anom_value< -3, -3, anom_value)),
                          region=factor(region, levels=region_str),
                          label=ifelse(is.na(anom_value), "", paste(sprintf("%.2f", anom_value))))
        
        # modify clim data (\U00B1 gives the plus-minus sign)
        tmp_climatology <- df_climatology %>%
            dplyr::filter(variable==i_variable & region %in% region_str) %>%   
            dplyr::mutate(label=sprintf(paste0("", format_str[i], " \U00B1 ", format_str[i]), mean, sd),
                          region=factor(region, levels=region_str))
        
        # x-axis labels
        x.labels <- rep("     ", length.out=length(x.breaks))
        if(i==length(variable_str)) {
            x.labels <- x.breaks
        }
        
        ## initialize plot
        p[[i]] <- ggplot() +
            coord_cartesian() +
            scale_x_continuous(name="", limits=x.limits, breaks=x.breaks, labels=x.labels, expand=c(0,0)) +
            scale_y_discrete(name=NULL, limits=y.limits, breaks=y.breaks, labels=y.labels, expand=c(0,0))
        
        # generate color bar for one plot
        if(i==1) {
            p[[i]] <- p[[i]] + 
                scale_fill_gradientn(colours=color.values,
                                     limits=color.limits,
                                     breaks=color.breaks,
                                     na.value = "grey80",
                                     guide="colorbar")
        } else {
            p[[i]] <- p[[i]] + 
                scale_fill_gradientn(colours=color.values,
                                     limits=color.limits,
                                     breaks=color.breaks,
                                     na.value = "grey80",
                                     guide="none")
        }
        
        ## heat plot
        # geom tile plots y axis alphabetically by default (using the region name, not the labels)
        # for example: GS, CLS, LAS, not "Greenland Shelf", "Central Labrador Sea", "Labrador Shelf"
        p[[i]] <- p[[i]] + 
            layer(
                data=tmp_anomaly,
                mapping=aes(x=year, y=region, fill=value_tmp),
                stat="identity",
                geom="tile",
                params=list(size=0, colour="grey"),
                position=position_identity()
            )
        
        # add text to individual cells
        p[[i]] <- p[[i]] + 
            layer(
                data=tmp_anomaly,
                mapping=aes(x=year, y=region, label=label),
                stat="identity",
                geom="text",
                params=list(size=1.5, colour="black"),
                position=position_identity()
            )
        
        # add text to right margin
        p[[i]] <- p[[i]] + 
            layer(
                data=tmp_climatology,
                mapping=aes(x=Inf, y=region, label=label),
                stat="identity",
                geom="text",
                params=list(size=2.5, colour="black", hjust=-0.1),
                position=position_identity()
            )
        
        ## customize plot components
        p[[i]] <- p[[i]] +
            theme_bw() +
            ggtitle(gsub("-Inf", "+ ", variable_lbl[i])) +
            theme(
                text=element_text(size=8),
                axis.text.x=element_text(colour="black", angle=90, hjust=0.5, vjust=0.5),
                plot.title=element_text(colour="black", hjust=0, vjust=0, size=8),
                legend.text=element_text(size=8),
                legend.position="bottom",
                legend.direction="horizontal",
                panel.border=element_rect(size=0.25, colour="black"),
                plot.margin=unit(c(0.1,1.5,0.1,0.1), "cm")
            )
        
        ## x-axis ticks
        if(i<length(variable_str)) {
            p[[i]] <- p[[i]] + theme(axis.ticks.x=element_blank())
        }
        
        ## customize legend
        if(i==1) {
            p[[i]] <- p[[i]] +
                guides(fill=guide_colourbar(title = NULL,
                                            label.position="bottom",
                                            label.hjust=0.5,
                                            label.vjust=0.5,
                                            barwidth=7.5,
                                            barheight=.25,
                                            default.unit="cm",
                                            reverse=FALSE)) 
            # get legend from p[[1]]
            tmp <- ggplot_gtable(ggplot_build(p[[i]]))
            leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
            legend <- tmp$grobs[[leg]]
            
            # remove legend from plots
            p[[i]] <- p[[i]] + theme(legend.position="none")
        }
        
        ## turn off clipping
        gt[[i]] <- ggplotGrob(p[[i]])
        gt[[i]]$layout$clip[gt[[i]]$layout$name == "panel"] <- "off"
        
    }
    
    return(list(gt, legend))
    
}


#===============================================================================

# given a df containing columns "longitude" and "latitude", subset it to within a
# certain distance (line_dist, degrees) of the ar7w line
subset_to_ar7w <- function(df, line_dist=0.1) {
    
    # slope and "intercept" of ar7w line were created by extracting the lats/lons of the stations and fitting a linear model:
    #
    # # list of AR7W stations from Li and Harrison, 2013, Table 2:
    # ar7w_stations <- read.csv("AZOMP/li_harrison_2013_table2.csv")
    # colnames(ar7w_stations) <- c("region","station","latitude","longitude","bathymetry","section_dist")
    # ar7w_stations$longitude <- -ar7w_stations$longitude
    # ar7w_lat <- ar7w_stations$latitude
    # ar7w_lon <- ar7w_stations$longitude
    # # find line and slope/intercept corresponding to ar7w line
    # ar7w_line <- lm(ar7w_lat ~ ar7w_lon)
    # m <- as.numeric(coef(ar7w_line)[2])
    # b <- as.numeric(coef(ar7w_line)[1])

    m <- 0.9368089
    b <- 105.8197
    x <- seq(-56,-48,by=.1)
    y <- m*x + b
    
    # only use data between 2 lines that enclose the ar7w line (shift line_dist in direction perpendicular to line)
    top_coords <- oceancolouR::shift_line(x=x, y=y, dist=line_dist, dir="up")
    bottom_coords <- oceancolouR::shift_line(x=x, y=y, dist=line_dist, dir="down")
    polyy <- c(top_coords$y[1], top_coords$y[2], bottom_coords$y[2], bottom_coords$y[1], top_coords$y[1])
    polyx <- c(top_coords$x[1], top_coords$x[2], bottom_coords$x[2], bottom_coords$x[1], top_coords$x[1])
    
    # create the mask for this polygon around the ar7w line
    mask <- sp::point.in.polygon(df$longitude, df$latitude, polyx, polyy)
    mask <- as.logical(mask)
    
    # return subsetted data around ar7w
    return(df[mask,])
    
}


#===============================================================================

# df must be a dataframe with latitude and longitude columns
subset_to_labsea_boxes <- function(df) {
    
    # HARRISON AND LI, 2008:
    # Labrador Sea Monitoring Program (LSMP) station locations, L3-01 (Labrador) to L3-28 (Greenland) and sub-regions:
    #     Labrador Shelf/Slope  LS (L3-01 to L3-10) - a.k.a. LAS
    #     Labrador Basin        LB (L3-11 to L3-23) - a.k.a. CLS (Central Labrador Sea)
    #     Greenland Shelf/Slope GS (L3-24 to L3-28)
    GS <- list(lat = c(60.1, 60.7, 60.7, 60.1, 60.1),
               lon = c(-48.8, -48.8, -48.1, -48.1, -48.8))
    CLS <- list(lat = c(55.5, 60.1, 60.1, 55.5, 55.5),
                lon = c(-53.7, -53.7, -48.8, -48.8, -53.7))
    LAS <- list(lat = c(53.6, 55.5, 55.5, 53.6, 53.6),
                lon = c(-55.7, -55.7, -53.7, -53.7, -55.7))
    
    LAS_ind <- as.logical(sp::point.in.polygon(df$longitude, df$latitude, LAS$lon, LAS$lat))
    CLS_ind <- as.logical(sp::point.in.polygon(df$longitude, df$latitude, CLS$lon, CLS$lat))
    GS_ind <- as.logical(sp::point.in.polygon(df$longitude, df$latitude, GS$lon, GS$lat))
    
    total_ind <- LAS_ind | CLS_ind | GS_ind
    
    return(df[total_ind,])
    
}


#===============================================================================

# given a number of panels, return the best set of image formatting parameters
scorecard_params <- function(num_panels) {
    
    # COMMON IMAGE PARAMETERS
    wd <- 1500
    total_width <- 1
    
    if (num_panels==19) {
        # 19 PANELS (ALL HPLC)
        topy <- 0.96
        bottomy <- 0.07
        gaps <- (topy - bottomy)/18.1
        heights <- 0.07
        ht <- 5000
        legendy <- 0.03
    } else if (num_panels==6) {
        # 6 PANELS (NUTRIENTS)
        topy <- 0.875
        bottomy <- 0.14
        gaps <- (topy - bottomy)/5
        heights <- 0.19
        ht <- 2000
        legendy <- 0.03
    } else if (num_panels==4) {
        # 4 PANELS (BLOOM PARAMETERS)
        topy <- 0.84
        bottomy <- 0.22
        gaps <- (topy - bottomy)/3
        heights <- 0.3
        ht <- 1200
        legendy <- 0.05
    } else if (num_panels==3) {
        # 3 PANELS (HPLC)
        topy <- 0.8
        bottomy <- 0.26
        gaps <- (topy - bottomy)/2
        heights <- 0.38
        ht <- 1000
        legendy <- 0.05
    } else if (num_panels==2) {
        # 2 PANELS
        topy <- 0.74
        bottomy <- 0.36
        gaps <- (topy - bottomy)#/2
        heights <- 0.55
        ht <- 650
        legendy <- 0.07
    } else if (num_panels==1) {
        # 1 PANEL (CHLA)
        topy <- 0.56
        bottomy <- topy
        gaps <- 0.05
        heights <- 0.88
        ht <- 420
        legendy <- 0.1
    }
    
    return(list(wd=wd, total_width=total_width, topy=topy, bottomy=bottomy, gaps=gaps, heights=heights, ht=ht, legendy=legendy))
    
}


#===============================================================================

# given latitude and longitude, and a vector of longitudes/latitudes/variable, calculate variable
get_bathy <- function(longitude, latitude, vlon, vlat, vvar) {
    
    # reduce size of grid for speed
    lt_deg <- abs(longitude - vlon) < 0.05 & abs(latitude - vlat) < 0.05
    mat <- cbind(vlon[lt_deg], vlat[lt_deg])
    dists <- as.numeric(geosphere::distGeo(p1=mat, p2=c(longitude, latitude)))
    
    return(abs(as.numeric(vvar[lt_deg][which.min(dists)])))
    
}


#===============================================================================
# profile integration function written by Benoit Casault
# nominal depth ~ bathymetry

DIS_Integrate_Profile <- function(depth, value, nominal_depth, depth_range) {
    
    # required packages
    library(pracma)
    library(caTools)
    
    # order depth_range
    depth_range <- c(min(depth_range), max(depth_range))
    
    # check upper intergration limit vs nominal depth
    if (depth_range[1] > nominal_depth) {
        return(NA)
    }
    # check lower intergration limit vs nominal depth
    if (depth_range[2] > nominal_depth) {
        depth_range[2] <- nominal_depth
    }
    
    # remove NaN from data
    na_index <- is.na(value)
    if (any(na_index)) {
        depth <- depth[!na_index]
        value <- value[!na_index]
    }
    
    if (length(value)<=1) {
        return(NA)
    } else {
        # extrapolate profile
        # at the top
        index <- which.min(depth)
        min_depth <- depth[index]
        if (depth_range[2] < min_depth) {
            return(NA)
        } else  if (depth_range[1] < min_depth) {
            depth <- c(depth, depth_range[1])
            value <- c(value, value[index])
        }  
        # at the bottom
        index <- which.max(depth)
        nominal_depth <- depth[index]
        if (depth_range[1] > nominal_depth) {
            return(NA)
        } else if (depth_range[2] > nominal_depth) {
            depth <- c(depth, depth_range[2])
            value <- c(value, value[index])
        }
        
        # interpolate profile
        index <- !(depth_range %in% depth)
        if (any(index)) {
            tmp_depth <- depth_range[index]
            tmp_value <- interp1(depth, value, tmp_depth, method="linear")
            depth <- c(depth, tmp_depth)
            value <- c(value, tmp_value)
        }
        
        # sort the profile
        index <- order(depth)
        depth <- depth[index]
        value <- value[index]
        
        # calculate integration
        index <- depth>=depth_range[1] & depth<=depth_range[2]
        return(caTools::trapz(depth[index],value[index]))
        
    }
}


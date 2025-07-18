#!/usr/bin/env Rscript

#' Script to detect precipitation events from seismic data.
#' Count of droplets hitting the ground can be used to estimate
#' the amount of precipitation.

## set up script
library("eseis")
library(ggplot2)
library(lubridate)
library(dplyr)
library(caTools)
Sys.setenv(TZ = "UTC")

# directories
load("/mnt/projects/retrogress/programmes/r/seismic/directories.rda")

st <- read.table(file = paste0(dirs$stations,"stationinfo_analysis.txt"),
                sep = ",",
                header = TRUE)

# just of one station
# CGC03
st_i <- st[3,]

# date of landslide
ls_date <- as.POSIXct("2023-08-24 03:15:54")

# time window before and after landslide: one week
w_before <- 3600 * 24 * 7
w_after <- 3600 * 24 * 1

# loop through time in hourly intervals
start <- round_date(ls_date - w_before, unit = "hour")
stop <- round_date(ls_date + w_after, unit = "hour")

# dateframe to save rain picks per hour
rain_picks_hourly <- data.frame(
    start = seq(from = start, to = stop, by = 3600),
    end = seq(from = start + 3600, to = stop + 3600, by = 3600),
    count = NA
)

print("Getting hourly rain picks for:")

while (start < stop){
    print(start)
    # read data
    s <- read_data(start = start,
                   duration = 3600,
                   station = st_i$ID,
                   format = "mseed",
                   dir = dirs$seismic)
    # filter by frequency
    s_f <- signal_filter(data = s, f = c(50,90), p = 0.05)

    # envelope
    # must be positive values of sta/lta, otherwise negative values
    s_f_env <- signal_envelope(data = s_f)

    # pick
    # parameters from Mohr et al (2024): Ideas and perspective
    # here: sta/lta in samples, freeze lta, on/off threshold
    rain <- pick_stalta(data = s_f_env, sta = 0.3 * 200, freeze = FALSE,
                        lta = 300 * 200, on = 3, off = 1.1)

    # duration of picks lower 0.5
    rain_picks <- rain$picks[rain$picks$duration < 0.5,]

    # count picks for complete hour
    rain_picks_hourly$count[rain_picks_hourly$start == start] <- nrow(rain_picks)

    # change start time
    start <- start + 3600
}

# save data as rda for final plotting
save(rain_picks_hourly, file = paste0(dirs$picks, "rain_50-90Hz_", st$ID[3], ".rda"))
#load(paste0(dirs$picks, "rain_50-90Hz_", st$ID[3], ".rda"))
# plot
jpeg(paste0(dirs$figures, "rain_50-90Hz_", st$ID[3], ".jpg"))

plot(rain_picks_hourly$start,rain_picks_hourly$count, type = "l")

dev.off()